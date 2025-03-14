package com.tonight.location_service.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class LocationUpdate {
    private String userId;
    private double latitude;
    private double longitude;
    private LocalDateTime timestamp;
    private boolean locationActive;
}