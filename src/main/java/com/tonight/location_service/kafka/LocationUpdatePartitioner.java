package com.tonight.location_service.kafka;
import ch.hsr.geohash.GeoHash;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.tonight.location_service.model.LocationUpdate;
import org.apache.kafka.clients.producer.Partitioner;
import org.apache.kafka.common.Cluster;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class LocationUpdatePartitioner implements Partitioner {

    private static final Logger log = LoggerFactory.getLogger(LocationUpdatePartitioner.class);
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public int partition(String topic, Object key, byte[] keyBytes, Object value, byte[] valueBytes, Cluster cluster) {
        try {
            if (value instanceof LocationUpdate update) {
                return getPartitionFromLocationUpdate(update, cluster, topic);
            } else {
                // Try to deserialize if not already a LocationUpdate
                LocationUpdate update = objectMapper.readValue(valueBytes, LocationUpdate.class);
                return getPartitionFromLocationUpdate(update, cluster, topic);
            }
        } catch (Exception e) {
            log.error("Error determining partition for location update", e);
            // Fallback to default partitioning using key
            return Math.abs((key.toString().hashCode()) % cluster.partitionCountForTopic(topic));
        }
    }

    private int getPartitionFromLocationUpdate(LocationUpdate update, Cluster cluster, String topic) {
        // Generate a geohash with precision 5 (neighborhood level)
        String geohash = GeoHash.withCharacterPrecision(
                update.getLatitude(), update.getLongitude(), 5).toBase32();

        // Create compound key: userId + geohash prefix (first 3 chars)
        String compoundKey = update.getUserId() + "_" + geohash.substring(0, 3);

        // Get partition count and calculate partition
        int numPartitions = cluster.partitionCountForTopic(topic);
        return Math.abs(compoundKey.hashCode() % numPartitions);
    }

    @Override
    public void close() {}

    @Override
    public void configure(Map<String, ?> configs) {}
}
