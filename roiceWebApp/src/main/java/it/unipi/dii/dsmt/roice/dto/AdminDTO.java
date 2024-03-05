package it.unipi.dii.dsmt.roice.dto;

import lombok.Getter;

@Getter
public class AdminDTO {
    private String email;
    private String password;

    public AdminDTO(String email, String password) {
        this.email = email;
        this.password = password;
    }
}
