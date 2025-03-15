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
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LocationService {

    private static final Logger log = LoggerFactory.getLogger(LocationService.class);
    private final RedisTemplate<String, Object> redisTemplate;
    private final GeoHashUtil geoHashUtils;

    // Key prefixes
    private static final String LOCATION_KEY_PREFIX = "user:location:";  // Changed to track multiple locations
    private static final String ALL_LOCATIONS_KEY = "all:locations";     // Global geo index
    private static final String USER_LOCATIONS_KEY_PREFIX = "user:locations:"; // Set of a user's active locations
    private static final String ONLINE_USERS_KEY = "online:users";
    private static final String LIKE_KEY_PREFIX = "like:";
    private static final String LIKE_COUNT_KEY_PREFIX = "like:count:";
    private static final String MATCH_KEY_PREFIX = "match:";

    // Constants
    private static final int LOCATION_EXPIRY_MINUTES = 30;

    public void processLocationUpdate(LocationUpdate update) {
        String userId = update.getUserId();
        LocalDateTime timestamp = update.getTimestamp();

        if (update.isLocationActive()) {
            // Create a unique location ID for this update
            String locationId = userId + ":" + timestamp.toString();

            // Create a geo key for this specific location
            String locationGeoKey = LOCATION_KEY_PREFIX + locationId;

            // Add this location to the global geo index
            redisTemplate.opsForGeo().add(
                    ALL_LOCATIONS_KEY,
                    new Point(update.getLongitude(), update.getLatitude()),
                    locationId
            );

            // Store additional metadata for this location
            redisTemplate.opsForHash().put(locationGeoKey, "userId", userId);
            redisTemplate.opsForHash().put(locationGeoKey, "latitude", update.getLatitude());
            redisTemplate.opsForHash().put(locationGeoKey, "longitude", update.getLongitude());
            redisTemplate.opsForHash().put(locationGeoKey, "timestamp", timestamp.toString());

            // Set expiry for this location (30 minutes)
            redisTemplate.expire(locationGeoKey, LOCATION_EXPIRY_MINUTES, TimeUnit.MINUTES);

            // Add this location to the user's set of active locations
            redisTemplate.opsForSet().add(USER_LOCATIONS_KEY_PREFIX + userId, locationId);

            // Add/update user in the online users set
            redisTemplate.opsForSet().add(ONLINE_USERS_KEY, userId);

            // Set 30 minute expiry for location in the global index and user's location set
            redisTemplate.expire(USER_LOCATIONS_KEY_PREFIX + userId, LOCATION_EXPIRY_MINUTES, TimeUnit.MINUTES);

            log.info("Added location for user {} at lat: {}, long: {}, time: {}",
                    update.getUserId(), update.getLatitude(), update.getLongitude(), timestamp);
        } else {
            // User is explicitly going offline
            // Get all active locations for this user
            Set<Object> userLocations = redisTemplate.opsForSet().members(USER_LOCATIONS_KEY_PREFIX + userId);

            if (userLocations != null && !userLocations.isEmpty()) {
                // Remove all locations from the global geo index
                for (Object locationId : userLocations) {
                    redisTemplate.opsForGeo().remove(ALL_LOCATIONS_KEY, locationId);
                    redisTemplate.delete(LOCATION_KEY_PREFIX + locationId);
                }

                // Clear the user's location set
                redisTemplate.delete(USER_LOCATIONS_KEY_PREFIX + userId);
            }

            // Remove from online users
            redisTemplate.opsForSet().remove(ONLINE_USERS_KEY, userId);

            log.info("Removed all locations for user {} at time {}", userId, timestamp);
        }
    }

    public List<String> findNearbyUsers(String userId, double latitude, double longitude, double radiusInKm) {
        // Use Redis GEO commands to find nearby locations
        Point searchPoint = new Point(longitude, latitude);
        Distance searchRadius = new Distance(radiusInKm, RedisGeoCommands.DistanceUnit.KILOMETERS);
        Circle searchArea = new Circle(searchPoint, searchRadius);

        // Get all location IDs within radius
        List<GeoResult<RedisGeoCommands.GeoLocation<Object>>> geoResults =
                redisTemplate.opsForGeo().radius(ALL_LOCATIONS_KEY, searchArea).getContent();

        if (geoResults.isEmpty()) {
            return Collections.emptyList();
        }

        // Extract unique user IDs from location IDs (excluding self)
        Set<String> nearbyUserIds = new HashSet<>();

        for (GeoResult<RedisGeoCommands.GeoLocation<Object>> result : geoResults) {
            String locationId = result.getContent().getName().toString();
            String locationUserId = locationId.split(":")[0]; // Extract userId from locationId

            // Skip if this is the current user
            if (!locationUserId.equals(userId)) {
                // Check if this location has expired
                if (redisTemplate.hasKey(LOCATION_KEY_PREFIX + locationId)) {
                    nearbyUserIds.add(locationUserId);
                }
            }
        }

        // Filter out users already liked
        List<String> filteredUsers = nearbyUserIds.stream()
                .filter(nearUserId -> {
                    // Check if the user already liked this person
                    String likeKey = LIKE_KEY_PREFIX + userId + ":" + nearUserId;
                    return !Boolean.TRUE.equals(redisTemplate.hasKey(likeKey));
                })
                .collect(Collectors.toList());

        log.info("Found {} nearby users for user {} at lat: {}, long: {}, radius: {}km",
                filteredUsers.size(), userId, latitude, longitude, radiusInKm);

        return filteredUsers;
    }

    public void createLike(String userId, String targetId) {
        String likeKey = LIKE_KEY_PREFIX + userId + ":" + targetId;

        // Check if the like already exists
        if (Boolean.FALSE.equals(redisTemplate.hasKey(likeKey))) {
            // Store the like with no expiration (permanent until unliked)
            redisTemplate.opsForValue().set(likeKey, "1");

            // Increment like count for the target user
            redisTemplate.opsForValue().increment(LIKE_COUNT_KEY_PREFIX + targetId);

            // Get the current like count
            Long likeCount = redisTemplate.opsForValue().increment(LIKE_COUNT_KEY_PREFIX + targetId, 0);
            if (likeCount >= 2) {
                log.info("User {} has received {} likes", targetId, likeCount);
                // Here you could trigger a notification to the target user
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

    // Method to clean up expired locations
    // This could be scheduled to run periodically as a batch job
    public void cleanupExpiredLocations() {
        // Let Redis TTL handle most of the cleanup automatically
        // This method would handle any edge cases or additional cleanup needed
        log.info("Running expired location cleanup job");

        // Find users with no active locations and remove from online users
        Set<Object> onlineUsers = redisTemplate.opsForSet().members(ONLINE_USERS_KEY);
        if (onlineUsers != null) {
            for (Object user : onlineUsers) {
                String userId = user.toString();
                Long locationCount = redisTemplate.opsForSet().size(USER_LOCATIONS_KEY_PREFIX + userId);

                if (locationCount == null || locationCount == 0) {
                    redisTemplate.opsForSet().remove(ONLINE_USERS_KEY, userId);
                    log.info("Removed user {} from online users due to no active locations", userId);
                }
            }
        }
    }
}