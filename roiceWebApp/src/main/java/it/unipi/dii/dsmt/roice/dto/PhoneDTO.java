package it.unipi.dii.dsmt.roice.dto;

import it.unipi.dii.dsmt.roice.model.Auction;
import it.unipi.dii.dsmt.roice.model.Phone;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Setter
@Getter
public class PhoneDTO implements Serializable {
    @NotBlank(message = "Phone name is required")
    @Size(max = 50, message = "Phone name cannot be longer than 50 characters")
    private String name;

    @NotBlank(message = "Phone picture is required")
    private String picture;

    @NotBlank(message = "Battery size is required")
    private String batterySize;

    @NotBlank(message = "Battery type is required")
    private String batteryType;

    @NotBlank(message = "Body is required")
    private String body;

    @NotBlank(message = "Brand is required")
    private String brand;

    @NotBlank(message = "Camera pixels are required")
    private String cameraPixels;

    @NotBlank(message = "Chipset is required")
    private String chipset;

    @NotBlank(message = "Display resolution is required")
    private String displayResolution;

    @NotBlank(message = "Display size is required")
    private String displaySize;

    @NotBlank(message = "Operating system is required")
    private String os;

    @NotBlank(message = "RAM is required")
    private String ram;

    @Positive(message = "Release year must be positive")
    private int releaseYear;

    @NotBlank(message = "Storage is required")
    private String storage;

    @NotBlank(message = "Video pixels are required")
    private String videoPixels;
    private Auction auction;

    public PhoneDTO() {
    }

    public PhoneDTO(String name, String picture, String batterySize, String batteryType, String body,
                    String brand, String cameraPixels, String chipset, String displayResolution, String displaySize,
                    String os, String ram, int releaseYear, String storage, String videoPixels, Auction auction) {
        this.name = name;
        this.picture = picture;
        this.batterySize = batterySize;
        this.batteryType = batteryType;
        this.body = body;
        this.brand = brand;
        this.cameraPixels = cameraPixels;
        this.chipset = chipset;
        this.displayResolution = displayResolution;
        this.displaySize = displaySize;
        this.os = os;
        this.ram = ram;
        this.releaseYear = releaseYear;
        this.storage = storage;
        this.videoPixels = videoPixels;
        this.auction = auction;
    }


    public static PhoneDTO toPhoneDTO(Phone phone) {
        if (phone == null)
            return null;
        PhoneDTO phoneDTO = new PhoneDTO();

        phoneDTO.setName(phone.getName());
        phoneDTO.setPicture(phone.getPicture());
        phoneDTO.setBatterySize(phone.getBatterySize());
        phoneDTO.setBatteryType(phone.getBatteryType());
        phoneDTO.setBody(phone.getBody());
        phoneDTO.setBrand(phone.getBrand());
        phoneDTO.setCameraPixels(phone.getCameraPixels());
        phoneDTO.setChipset(phone.getChipset());
        phoneDTO.setDisplayResolution(phone.getDisplayResolution());
        phoneDTO.setDisplaySize(phone.getDisplaySize());
        phoneDTO.setOs(phone.getOs());
        phoneDTO.setRam(phone.getRam());
        phoneDTO.setReleaseYear(phone.getReleaseYear());
        phoneDTO.setStorage(phone.getStorage());
        phoneDTO.setVideoPixels(phone.getVideoPixels());
        phoneDTO.setAuction(phone.getAuction());

        return phoneDTO;
    }
}
