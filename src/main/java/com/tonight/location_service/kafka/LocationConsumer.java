package com.tonight.location_service.kafka;

import com.tonight.location_service.model.LocationUpdate;
import com.tonight.location_service.service.LocationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class LocationConsumer {

    private static final Logger log = LoggerFactory.getLogger(LocationConsumer.class);
    private final LocationService locationService;

    @KafkaListener(
            topics = "user-locations",
            groupId = "location-service-group",
            containerFactory = "kafkaListenerContainerFactory"
    )
    public void consumeLocationUpdate(LocationUpdate locationUpdate, Acknowledgment acknowledgment) {
        try {
            log.info("Received location update for user: {}", locationUpdate.getUserId());
            locationService.processLocationUpdate(locationUpdate);
            acknowledgment.acknowledge();
        } catch (Exception e) {
            log.error("Error processing location update for user: {}", locationUpdate.getUserId(), e);
            // Consider retry logic or dead letter queue here
            acknowledgment.acknowledge(); // Still ack to avoid reprocessing the same error
        }
    }
}