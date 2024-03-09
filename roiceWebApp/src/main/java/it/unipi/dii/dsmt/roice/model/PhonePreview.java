package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PhonePreview {

    private String name;
    private String picture;

    public PhonePreview(String name, String picture) {
        this.name = name;
        this.picture = picture;
    }
}
