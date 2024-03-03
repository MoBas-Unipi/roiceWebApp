package it.unipi.dii.dsmt.roice.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class Notification {

    private String title;
    private String body;

    public Notification(String title, String body) {
        this.title = title;
        this.body = body;
    }
}
