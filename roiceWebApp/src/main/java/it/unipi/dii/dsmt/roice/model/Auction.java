package it.unipi.dii.dsmt.roice.model;

import java.util.Date;

public class Auction {

	private Date startingDate;
	private Date endDate;
	private Double minimumPrice;

	public Auction(Date startingDate, Date endDate, Double minimumPrice) {
		this.startingDate = startingDate;
		this.endDate = endDate;
		this.minimumPrice = minimumPrice;
	}

	public Date getStartingDate() {
		return startingDate;
	}

	public void setStartingDate(Date startingDate) {
		this.startingDate = startingDate;
	}

	public Date getEndDate() {
		return endDate;
	}

	public void setEndDate(Date endDate) {
		this.endDate = endDate;
	}

	public Double getMinimumPrice() {
		return minimumPrice;
	}

	public void setMinimumPrice(Double minimunPrice) {
		this.minimumPrice = minimumPrice;
	}
}
