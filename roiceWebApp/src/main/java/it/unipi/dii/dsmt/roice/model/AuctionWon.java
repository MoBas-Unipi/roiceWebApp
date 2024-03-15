package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
public class AuctionWon {

    private String phoneName;
    private String phonePicture;
    private LocalDateTime endDate;
    private double price;

    public AuctionWon(String phoneName, String phonePicture, LocalDateTime endDate, double price) {
        this.phoneName = phoneName;
        this.phonePicture = phonePicture;
        this.endDate = endDate;
        this.price = price;
    }

}
