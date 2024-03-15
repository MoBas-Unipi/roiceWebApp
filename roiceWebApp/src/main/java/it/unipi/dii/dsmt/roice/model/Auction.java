package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
public class Auction {

	private LocalDateTime startingDate;
	private LocalDateTime endDate;
	private Double minimumPrice;

	public Auction(LocalDateTime startingDate, LocalDateTime endDate, Double minimumPrice) {
		this.startingDate = startingDate;
		this.endDate = endDate;
		this.minimumPrice = minimumPrice;
	}
}
