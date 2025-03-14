package com.tonight.location_service.controller;

import com.tonight.location_service.model.UserProfile;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class ProfileController {

    private final RedisTemplate<String, Object> redisTemplate;

    public ProfileController(RedisTemplate<String, Object> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @PostMapping("/profile/update")
    public ResponseEntity<Void> updateProfile(@RequestBody UserProfile userProfile) {
        String profileKey = "user:" + userProfile.getUserId() + ":profile";
        redisTemplate.opsForHash().put(profileKey, "gender", userProfile.getGender());
        redisTemplate.opsForHash().put(profileKey, "active", userProfile.isActive());
        return ResponseEntity.ok().build();
    }
}
