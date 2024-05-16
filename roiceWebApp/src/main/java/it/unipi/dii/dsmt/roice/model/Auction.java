package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.Setter;

import java.util.Date;

@Getter
@Setter
public class Auction {

	private Date startingDate;
	private Date endDate;
	private Integer minimumPrice;

	public Auction(Date startingDate, Date endDate, Integer minimumPrice) {
		this.startingDate = startingDate;
		this.endDate = endDate;
		this.minimumPrice = minimumPrice;
	}
}
