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

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
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
    private static final String ONLINE_USERS_KEY = "online:";
    private static final String LIKE_KEY_PREFIX = "like:";
    private static final String LIKE_COUNT_KEY_PREFIX = "like:count:";
    private static final String EXPIRY_TIME_KEY_PREFIX = "expiry:";
    private static final String MATCH_KEY_PREFIX = "match:";

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

            // Add to online users for this geohash bucket (now nested within geobucket)
            redisTemplate.opsForSet().add(GEOHASH_BUCKET_KEY + geohashPrefix + ":" + ONLINE_USERS_KEY, update.getUserId());

            // Store user's activity timestamp with 1 hour expiry
            redisTemplate.opsForValue().set(
                    EXPIRY_TIME_KEY_PREFIX + update.getUserId(),
                    update.getTimestamp().toString(),
                    1,
                    TimeUnit.HOURS
            );

            // Set expiration for user's active status in buckets - 1 hour
            redisTemplate.expire(GEOHASH_BUCKET_KEY + geohashPrefix + ":" + ONLINE_USERS_KEY, 1, TimeUnit.HOURS);

            log.info("Updated location for user {} at lat: {}, long: {}, geohash: {}, time: {}",
                    update.getUserId(), update.getLatitude(), update.getLongitude(), geohash, update.getTimestamp());
        } else {
            // Get current geohash to remove from correct buckets
            String geohash = null;
            Set<Object> userGeoPos = Collections.singleton(redisTemplate.opsForGeo().position(LOCATION_KEY, update.getUserId()));
            Point point = (Point) userGeoPos.iterator().next();
            geohash = geoHashUtils.generateGeoHash(point.getY(), point.getX());

            if (geohash != null) {
                String geohashPrefix = geoHashUtils.getGeohashPrefix(geohash);

                // Remove from geohash bucket and online users
                redisTemplate.opsForSet().remove(GEOHASH_BUCKET_KEY + geohashPrefix, update.getUserId());
                redisTemplate.opsForSet().remove(GEOHASH_BUCKET_KEY + geohashPrefix + ":" + ONLINE_USERS_KEY, update.getUserId());
            }

            // Remove from geo set if location is toggled off
            redisTemplate.opsForGeo().remove(LOCATION_KEY, update.getUserId());

            // Update user's last active time with 1 hour expiry for time window visibility
            redisTemplate.opsForValue().set(
                    EXPIRY_TIME_KEY_PREFIX + update.getUserId(),
                    update.getTimestamp().toString(),
                    1,
                    TimeUnit.HOURS
            );

            log.info("Removed location for user {} at time {}", update.getUserId(), update.getTimestamp());
        }
    }

    public List<String> findNearbyUsers(String userId, double latitude, double longitude, double radiusInKm) {
        // Generate geohash for search location and get adjacent geohashes
        Set<String> searchGeohashes = geoHashUtils.getGeohashesAround(latitude, longitude, radiusInKm);

        // Get user's last active time
        String userExpiryTimeStr = (String) redisTemplate.opsForValue().get(EXPIRY_TIME_KEY_PREFIX + userId);
        LocalDateTime userExpiryTime = userExpiryTimeStr != null ?
                LocalDateTime.parse(userExpiryTimeStr) : LocalDateTime.now();

        // Get online users from all relevant geohash buckets
        List<String> potentialMatches = new ArrayList<>();
        for (String geohash : searchGeohashes) {
            String geohashPrefix = geoHashUtils.getGeohashPrefix(geohash);
            Set<Object> onlineUsers = redisTemplate.opsForSet().members(GEOHASH_BUCKET_KEY + geohashPrefix + ":" + ONLINE_USERS_KEY);
            if (onlineUsers != null) {
                potentialMatches.addAll(onlineUsers.stream()
                        .map(Object::toString)
                        .collect(Collectors.toList()));
            }
        }

        // Filter potential matches based on time constraints and exclude self
        List<String> filteredMatches = potentialMatches.stream()
                .filter(nearUserId -> !nearUserId.equals(userId)) // Exclude self
                .filter(nearUserId -> {
                    // Check if the user already liked this person
                    String likeKey = LIKE_KEY_PREFIX + userId + ":" + nearUserId;
                    boolean alreadyLiked = Boolean.TRUE.equals(redisTemplate.hasKey(likeKey));

                    // Check time constraints
                    String nearUserExpiryTimeStr = (String) redisTemplate.opsForValue().get(EXPIRY_TIME_KEY_PREFIX + nearUserId);
                    if (nearUserExpiryTimeStr != null) {
                        LocalDateTime nearUserExpiryTime = LocalDateTime.parse(nearUserExpiryTimeStr);

                        // Calculate visibility expiry time based on minimum time + 1 hour
                        LocalDateTime minExpiryTime = userExpiryTime.isBefore(nearUserExpiryTime) ?
                                userExpiryTime : nearUserExpiryTime;
                        LocalDateTime visibilityExpiry = minExpiryTime.plusHours(1);

                        return LocalDateTime.now().isBefore(visibilityExpiry) && !alreadyLiked;
                    }

                    return !alreadyLiked; // Default to showing unliked users if no expiry time
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
                .filter(filteredMatches::contains)
                .collect(Collectors.toList());

        log.info("Found {} nearby users for user {} at lat: {}, long: {}, radius: {}km",
                preciseMatches.size(), userId, latitude, longitude, radiusInKm);

        return preciseMatches;
    }

    public void createLike(String userId, String targetId) {
        String likeKey = LIKE_KEY_PREFIX + userId + ":" + targetId;

        // Check if the like already exists
        if (Boolean.FALSE.equals(redisTemplate.hasKey(likeKey))) {
            // Set expiry time based on minimum of user and target's last active time
            String userExpiryTimeStr = (String) redisTemplate.opsForValue().get(EXPIRY_TIME_KEY_PREFIX + userId);
            String targetExpiryTimeStr = (String) redisTemplate.opsForValue().get(EXPIRY_TIME_KEY_PREFIX + targetId);

            LocalDateTime userExpiryTime = userExpiryTimeStr != null ?
                    LocalDateTime.parse(userExpiryTimeStr) : LocalDateTime.now();
            LocalDateTime targetExpiryTime = targetExpiryTimeStr != null ?
                    LocalDateTime.parse(targetExpiryTimeStr) : LocalDateTime.now();

            // Calculate expiry time as minimum time + 1 hour
            LocalDateTime minExpiryTime = userExpiryTime.isBefore(targetExpiryTime) ?
                    userExpiryTime : targetExpiryTime;
            LocalDateTime likeExpiryTime = minExpiryTime.plusHours(1);

            // Calculate expiry time in seconds
            long expirySeconds = Duration.between(LocalDateTime.now(), likeExpiryTime).getSeconds();
            if (expirySeconds > 0) {
                redisTemplate.opsForValue().set(likeKey, "1", expirySeconds, TimeUnit.SECONDS);

                // Increment like count for the target user
                redisTemplate.opsForValue().increment(LIKE_COUNT_KEY_PREFIX + targetId);

                // Get the current like count
                Long likeCount = redisTemplate.opsForValue().increment(LIKE_COUNT_KEY_PREFIX + targetId, 0);
                if (likeCount >= 2) {
                    log.info("User {} has received {} likes", targetId, likeCount);
                    // Here you could trigger a notification to the target user
                }
            }
        }

        // Check if mutual like exists
        String reverseLikeKey = LIKE_KEY_PREFIX + targetId + ":" + userId;
        Boolean mutualLike = redisTemplate.hasKey(reverseLikeKey);

        if (Boolean.TRUE.equals(mutualLike)) {
            log.info("Mutual like detected between {} and {}", userId, targetId);

            // Create a match entry
            String matchKey = MATCH_KEY_PREFIX + userId + ":" + targetId;
            redisTemplate.opsForValue().set(matchKey, "1");

            // Handle redirecting to chat service
            log.info("User {} and User {} matched! Redirecting to chat service.", userId, targetId);
            // Here you would trigger the redirect to the chat service
        }
    }
}