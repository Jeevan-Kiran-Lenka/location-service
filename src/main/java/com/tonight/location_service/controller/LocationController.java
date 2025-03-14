package com.tonight.location_service.controller;

import com.tonight.location_service.model.LocationUpdate;
import com.tonight.location_service.service.LocationProducerService;
import com.tonight.location_service.service.LocationService;
import lombok.RequiredArgsConstructor;
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
        locationUpdate.setTimestamp(LocalDateTime.now());
        // Send to Kafka instead of direct processing
        locationProducerService.sendLocationUpdate(locationUpdate);
        return ResponseEntity.accepted().build();
    }

    @GetMapping("/nearby")
    public ResponseEntity<List<String>> getNearbyUsers(
            @RequestParam String userId,
            @RequestParam(required = false) String gender,
            @RequestParam double latitude,
            @RequestParam double longitude,
            @RequestParam(defaultValue = "5.0") double radiusInKm) {

        List<String> nearbyUsers = locationService.findNearbyUsers(
                userId, gender, latitude, longitude, radiusInKm);
        return ResponseEntity.ok(nearbyUsers);
    }

    @PostMapping("/like")
    public ResponseEntity<Void> createLike(
            @RequestParam String userId,
            @RequestParam String targetId) {

        locationService.createLike(userId, targetId);
        return ResponseEntity.ok().build();
    }
}