package com.tonight.location_service.controller;

import com.tonight.location_service.model.LocationUpdate;
import com.tonight.location_service.service.LocationProducerService;
import com.tonight.location_service.service.LocationService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.List;

@RestController
@RequestMapping("/api/location")
public class LocationController {

    private final LocationService locationService;
    private final LocationProducerService locationProducerService;

    public LocationController(LocationService locationService, LocationProducerService locationProducerService) {
        this.locationService = locationService;
        this.locationProducerService = locationProducerService;
    }

    @PostMapping("/update")
    public ResponseEntity<Void> updateLocation(@RequestBody LocationUpdate locationUpdate) {
        // Set current timestamp if not provided
        if (locationUpdate.getTimestamp() == null) {
            locationUpdate.setTimestamp(LocalDateTime.now());
        }

        // Send to Kafka for processing
        locationProducerService.sendLocationUpdate(locationUpdate);
        return ResponseEntity.accepted().build();
    }

    @GetMapping("/nearby")
    public ResponseEntity<List<String>> getNearbyUsers(
            @RequestParam String userId,
            @RequestParam double latitude,
            @RequestParam double longitude,
            @RequestParam(defaultValue = "5.0") double radiusInKm) {

        List<String> nearbyUsers = locationService.findNearbyUsers(
                userId, latitude, longitude, radiusInKm);
        return ResponseEntity.ok(nearbyUsers);
    }

    @PostMapping("/like")
    public ResponseEntity<Void> createLike(
            @RequestParam String userId,
            @RequestParam String targetId) {

        locationService.createLike(userId, targetId);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/offline")
    public ResponseEntity<Void> goOffline(@RequestParam String userId) {
        // Create an "offline" location update
        LocationUpdate update = LocationUpdate.builder()
                .userId(userId)
                .locationActive(false)
                .timestamp(LocalDateTime.now())
                .build();

        locationProducerService.sendLocationUpdate(update);
        return ResponseEntity.accepted().build();
    }
}