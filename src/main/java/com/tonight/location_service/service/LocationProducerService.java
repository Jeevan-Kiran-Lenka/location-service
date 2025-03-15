package com.tonight.location_service.service;

import com.tonight.location_service.model.LocationUpdate;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class LocationProducerService {

    private static final Logger log = LoggerFactory.getLogger(LocationProducerService.class);
    private final KafkaTemplate<String, Object> kafkaTemplate;

    private static final String TOPIC = "user-locations";

    public void sendLocationUpdate(LocationUpdate locationUpdate) {
        // Using userId as the key for consistent partitioning
        kafkaTemplate.send(TOPIC, locationUpdate.getUserId(), locationUpdate)
                .whenComplete((result, ex) -> {
                    if (ex == null) {
                        log.info("Location update for user {} sent to partition {} with offset {}",
                                locationUpdate.getUserId(),
                                result.getRecordMetadata().partition(),
                                result.getRecordMetadata().offset());
                    } else {
                        log.error("Failed to send location update for user {}", locationUpdate.getUserId(), ex);
                    }
                });
    }
}