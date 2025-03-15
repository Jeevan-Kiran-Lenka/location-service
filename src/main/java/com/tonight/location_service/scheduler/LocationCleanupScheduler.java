package com.tonight.location_service.scheduler;

import com.tonight.location_service.service.LocationService;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@EnableScheduling
@RequiredArgsConstructor
public class LocationCleanupScheduler {

    private static final Logger log = LoggerFactory.getLogger(LocationCleanupScheduler.class);
    private final LocationService locationService;

    /**
     * Run cleanup job every 5 minutes
     * This helps catch any locations that didn't properly expire
     * and ensures the online users list stays accurate
     */
    @Scheduled(fixedRate = 300000) // 5 minutes in milliseconds
    public void scheduleLocationCleanup() {
        log.info("Starting scheduled location cleanup task");
        try {
            locationService.cleanupExpiredLocations();
            log.info("Completed scheduled location cleanup task");
        } catch (Exception e) {
            log.error("Error during scheduled location cleanup", e);
        }
    }
}
