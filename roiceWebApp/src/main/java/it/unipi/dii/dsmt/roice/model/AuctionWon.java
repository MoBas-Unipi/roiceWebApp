package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.Setter;

import java.util.Date;

@Getter
@Setter
public class AuctionWon {

    private String phoneName;
    private String phonePicture;
    private Date endDate;
    private int price;

    public AuctionWon(String phoneName, String phonePicture, Date endDate, int price) {
        this.phoneName = phoneName;
        this.phonePicture = phonePicture;
        this.endDate = endDate;
        this.price = price;
    }

}
