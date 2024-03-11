package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PhonePreview {

    private String id;
    private String name;
    private String picture;

    public PhonePreview(String id, String name, String picture) {
        this.id = id;
        this.name = name;
        this.picture = picture;
    }
}
