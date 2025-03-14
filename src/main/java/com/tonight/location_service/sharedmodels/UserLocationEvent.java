package com.tonight.location_service.sharedmodels;

public class UserLocationEvent {
    private String userId;
    private double latitude;
    private double longitude;
    private String clientTimestamp;  // When the browser obtained the location
    private String serverTimestamp; // When the server processed the request

    // Default constructor for JSON serialization
    public UserLocationEvent() {}

    // Getters and setters
    public String getUserId() { return userId; }
    public void setUserId(String userId) { this.userId = userId; }
    public double getLatitude() { return latitude; }
    public void setLatitude(double latitude) { this.latitude = latitude; }
    public double getLongitude() { return longitude; }
    public void setLongitude(double longitude) { this.longitude = longitude; }
    public String getClientTimestamp() { return clientTimestamp; }
    public void setClientTimestamp(String clientTimestamp) { this.clientTimestamp = clientTimestamp; }
    public String getServerTimestamp() { return serverTimestamp; }
    public void setServerTimestamp(String serverTimestamp) { this.serverTimestamp = serverTimestamp; }
}