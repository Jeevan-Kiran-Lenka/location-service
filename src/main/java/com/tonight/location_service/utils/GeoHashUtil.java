package com.tonight.location_service.utils;

import ch.hsr.geohash.GeoHash;
import ch.hsr.geohash.WGS84Point;
import ch.hsr.geohash.util.VincentyGeodesy;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.Set;

@Component
public class GeoHashUtil {

    // Precision level for main geohash (approximately neighborhood level)
    private static final int GEOHASH_PRECISION = 6;

    /**
     * Generate a geohash for a given latitude and longitude
     */
    public String generateGeoHash(double latitude, double longitude) {
        return GeoHash.withCharacterPrecision(latitude, longitude, GEOHASH_PRECISION).toBase32();
    }

    /**
     * Get the geohash and its adjacent geohashes to cover the area
     */
    public Set<String> getGeohashesAround(double latitude, double longitude, double radiusInKm) {
        // Create the center geohash
        GeoHash centerHash = GeoHash.withCharacterPrecision(latitude, longitude, GEOHASH_PRECISION);
        Set<String> hashes = new HashSet<>();
        hashes.add(centerHash.toBase32());

        // Add adjacent hashes
        for (GeoHash adjacent : centerHash.getAdjacent()) {
            hashes.add(adjacent.toBase32());

            // Add diagonal hashes for better coverage
            for (GeoHash diagonalAdjacent : adjacent.getAdjacent()) {
                // Only add if not already in set and not the center
                if (!hashes.contains(diagonalAdjacent.toBase32()) &&
                        !diagonalAdjacent.equals(centerHash)) {

                    // Check if this geohash is within our radius
                    WGS84Point diagonalPoint = diagonalAdjacent.getBoundingBoxCenter();
                    WGS84Point centerPoint = new WGS84Point(latitude, longitude);

                    double distance = VincentyGeodesy.distanceInMeters(centerPoint, diagonalPoint);
                    if (distance <= radiusInKm) {
                        hashes.add(diagonalAdjacent.toBase32());
                    }
                }
            }
        }

        return hashes;
    }

    /**
     * Extracts the prefix (first 3 chars) of a geohash for bucket partitioning
     */
    public String getGeohashPrefix(String geohash) {
        if (geohash == null || geohash.length() < 3) {
            return geohash;
        }
        return geohash.substring(0, 3);
    }
}