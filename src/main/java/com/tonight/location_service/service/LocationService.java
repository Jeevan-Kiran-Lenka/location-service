package com.tonight.location_service.service;

import com.tonight.location_service.model.LocationUpdate;
import com.tonight.location_service.utils.GeoHashUtil;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.geo.Circle;
import org.springframework.data.geo.Distance;
import org.springframework.data.geo.GeoResult;
import org.springframework.data.geo.Point;
import org.springframework.data.redis.connection.RedisGeoCommands;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LocationService {

    private static final Logger log = LoggerFactory.getLogger(LocationService.class);
    private final RedisTemplate<String, Object> redisTemplate;
    private final GeoHashUtil geoHashUtils;

    // Key prefixes
    private static final String LOCATION_KEY = "user:locations";
    private static final String GEOHASH_BUCKET_KEY = "geobucket:";
    private static final String ONLINE_USERS_KEY = "users:online:";
    private static final String PROFILE_KEY_PREFIX = "user:";
    private static final String PROFILE_KEY_SUFFIX = ":profile";
    private static final String LIKE_KEY_PREFIX = "like:";

    public void processLocationUpdate(LocationUpdate update) {
        if (update.isLocationActive()) {
            // Generate geohash for the location
            String geohash = geoHashUtils.generateGeoHash(update.getLatitude(), update.getLongitude());
            String geohashPrefix = geoHashUtils.getGeohashPrefix(geohash);

            // Store the user location in Redis GEO set
            redisTemplate.opsForGeo().add(
                    LOCATION_KEY,
                    new Point(update.getLongitude(), update.getLatitude()),
                    update.getUserId()
            );

            // Store user in geohash bucket
            redisTemplate.opsForSet().add(GEOHASH_BUCKET_KEY + geohashPrefix, update.getUserId());

            // Add to online users for this geohash bucket
            redisTemplate.opsForSet().add(ONLINE_USERS_KEY + geohashPrefix, update.getUserId());

            // Store geohash in user profile
            String profileKey = PROFILE_KEY_PREFIX + update.getUserId() + PROFILE_KEY_SUFFIX;
            redisTemplate.opsForHash().put(profileKey, "geohash", geohash);
            redisTemplate.opsForHash().put(profileKey, "active", "true");

            // Set expiration for user's active status - 1 hour
            redisTemplate.expire(profileKey, 1, TimeUnit.HOURS);
            redisTemplate.expire(ONLINE_USERS_KEY + geohashPrefix, 1, TimeUnit.HOURS);

            log.info("Updated location for user {} at lat: {}, long: {}, geohash: {}",
                    update.getUserId(), update.getLatitude(), update.getLongitude(), geohash);
        } else {
            // Get current geohash from profile to remove from correct buckets
            String profileKey = PROFILE_KEY_PREFIX + update.getUserId() + PROFILE_KEY_SUFFIX;
            String geohash = (String) redisTemplate.opsForHash().get(profileKey, "geohash");

            if (geohash != null) {
                String geohashPrefix = geoHashUtils.getGeohashPrefix(geohash);

                // Remove from geohash bucket and online users
                redisTemplate.opsForSet().remove(GEOHASH_BUCKET_KEY + geohashPrefix, update.getUserId());
                redisTemplate.opsForSet().remove(ONLINE_USERS_KEY + geohashPrefix, update.getUserId());
            }

            // Remove from geo set if location is toggled off
            redisTemplate.opsForGeo().remove(LOCATION_KEY, update.getUserId());

            // Update user profile
            redisTemplate.opsForHash().put(profileKey, "active", "false");

            log.info("Removed location for user {}", update.getUserId());
        }
    }

    public List<String> findNearbyUsers(String userId, String gender, double latitude, double longitude, double radiusInKm) {
        // Generate geohash for search location and get adjacent geohashes
        Set<String> searchGeohashes = geoHashUtils.getGeohashesAround(latitude, longitude, radiusInKm);

        // Get online users from all relevant geohash buckets
        List<String> potentialMatches = new ArrayList<>();
        for (String geohash : searchGeohashes) {
            String geohashPrefix = geoHashUtils.getGeohashPrefix(geohash);
            Set<Object> onlineUsers = redisTemplate.opsForSet().members(ONLINE_USERS_KEY + geohashPrefix);
            if (onlineUsers != null) {
                potentialMatches.addAll(onlineUsers.stream()
                        .map(Object::toString)
                        .collect(Collectors.toList()));
            }
        }

        // Filter potential matches by gender
        List<String> genderFilteredMatches = potentialMatches.stream()
                .filter(nearUserId -> !nearUserId.equals(userId)) // Exclude self
                .filter(nearUserId -> {
            String nearUserProfileKey = PROFILE_KEY_PREFIX + nearUserId + PROFILE_KEY_SUFFIX;
            String nearUserGender = (String) redisTemplate.opsForHash().get(nearUserProfileKey, "gender");

            Object nearUserActiveObj = redisTemplate.opsForHash().get(nearUserProfileKey, "active");
            boolean isActive = false;

            if (nearUserActiveObj instanceof Boolean) {
                isActive = (Boolean) nearUserActiveObj; // Direct Boolean retrieval
            } else if (nearUserActiveObj instanceof String) {
                isActive = Boolean.parseBoolean((String) nearUserActiveObj); // Convert String to Boolean
            }

            return gender != null &&
                    !gender.equals(nearUserGender) &&
                    isActive; // Use Boolean instead of comparing with "true"
        })
                .collect(Collectors.toList());

        // Now do precise distance calculation with Redis GEO commands
        Point point = new Point(longitude, latitude);
        Distance distance = new Distance(radiusInKm, RedisGeoCommands.DistanceUnit.KILOMETERS);
        Circle circle = new Circle(point, distance);

        List<GeoResult<RedisGeoCommands.GeoLocation<Object>>> geoResults =
                redisTemplate.opsForGeo().radius(LOCATION_KEY, circle).getContent();

        List<String> preciseMatches = geoResults.stream()
                .map(result -> result.getContent().getName().toString())
                .filter(genderFilteredMatches::contains)
                .collect(Collectors.toList());

        log.info("Found {} nearby users for user {} at lat: {}, long: {}, radius: {}km",
                preciseMatches.size(), userId, latitude, longitude, radiusInKm);

        return preciseMatches;
    }

    public void createLike(String userId, String targetId) {
        String likeKey = LIKE_KEY_PREFIX + userId + ":" + targetId;
        redisTemplate.opsForValue().set(likeKey, "1");
        redisTemplate.expire(likeKey, 1, TimeUnit.HOURS);

        // Check if mutual like exists
        String reverseLikeKey = LIKE_KEY_PREFIX + targetId + ":" + userId;
        Boolean mutualLike = redisTemplate.hasKey(reverseLikeKey);

        if (Boolean.TRUE.equals(mutualLike)) {
            log.info("Mutual like detected between {} and {}", userId, targetId);
            // Logic for mutual like can be implemented here or via events
        }
    }
}