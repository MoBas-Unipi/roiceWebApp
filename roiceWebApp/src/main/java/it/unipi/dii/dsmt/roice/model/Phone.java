package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Setter
@Getter
@NoArgsConstructor
@Document(collection = "phones")
public class Phone {

	@Id
	private String id;
	private String brand;
	@Indexed(unique = true)
	private String name;
	private String picture;
	private String body;
	private String os;
	private String storage;
	private String displaySize;
	private String displayResolution;
	private String cameraPixels;
	private String videoPixels;
	private String ram;
	private String chipset;
	private String batterySize;
	private String batteryType;
	private int releaseYear;
	private Auction auction;

	public Phone(String brand, String name, String picture, String body, String os, String storage,
	             String displaySize, String displayResolution, String cameraPixels, String videoPixels,
	             String ram, String chipset, String batterySize, String batteryType, int releaseYear, Auction auction) {
		this.brand = brand;
		this.name = name;
		this.picture = picture;
		this.releaseYear = releaseYear;
		this.body = body;
		this.os = os;
		this.storage = storage;
		this.displaySize = displaySize;
		this.displayResolution = displayResolution;
		this.cameraPixels = cameraPixels;
		this.videoPixels = videoPixels;
		this.ram = ram;
		this.chipset = chipset;
		this.batterySize = batterySize;
		this.batteryType = batteryType;
		this.auction = auction;
	}

	@Override
	public String toString() {
		return "Phone{" +
				"id='" + id + '\'' +
				", brand='" + brand + '\'' +
				", name='" + name + '\'' +
				", picture='" + picture + '\'' +
				", releaseDate=" + releaseYear +
				", body='" + body + '\'' +
				", os='" + os + '\'' +
				", storage='" + storage + '\'' +
				", displaySize='" + displaySize + '\'' +
				", displayResolution='" + displayResolution + '\'' +
				", cameraPixels='" + cameraPixels + '\'' +
				", videoPixels='" + videoPixels + '\'' +
				", ram='" + ram + '\'' +
				", chipset='" + chipset + '\'' +
				", batterySize='" + batterySize + '\'' +
				", batteryType='" + batteryType + '\'' +
				", auction='" + auction + '\'' +
				'}';
	}
}

