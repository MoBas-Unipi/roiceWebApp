package it.unipi.dii.dsmt.roice.dto;

import it.unipi.dii.dsmt.roice.model.Phone;

import java.io.Serializable;
import java.util.Date;

public class PhoneDTO implements Serializable {
    private String id;
    private String name;
    private String picture;
    //private double price;
    private String batterySize;
    private String batteryType;
    private String body;
    private String brand;
    private String cameraPixels;
    private String chipset;
    private String displayResolution;
    private String displaySize;
    private String os;
    private String ram;
    private int releaseYear;
    private String storage;
    private String videoPixels;


    public PhoneDTO() {
    }

    public PhoneDTO(String id, String name, String picture, String batterySize, String batteryType, String body, String brand, String cameraPixels, String chipset, String displayResolution, String displaySize, String os, String ram, int releaseYear, String storage, String videoPixels) {
        this.id = id;
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
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPicture() {
        return picture;
    }

    public void setPicture(String picture) {
        this.picture = picture;
    }

    public String getBatterySize() {
        return batterySize;
    }

    public void setBatterySize(String batterySize) {
        this.batterySize = batterySize;
    }

    public String getBatteryType() {
        return batteryType;
    }

    public void setBatteryType(String batteryType) {
        this.batteryType = batteryType;
    }

    public String getBody() {
        return body;
    }

    public void setBody(String body) {
        this.body = body;
    }

    public String getBrand() {
        return brand;
    }

    public void setBrand(String brand) {
        this.brand = brand;
    }

    public String getCameraPixels() {
        return cameraPixels;
    }

    public void setCameraPixels(String cameraPixels) {
        this.cameraPixels = cameraPixels;
    }

    public String getChipset() {
        return chipset;
    }

    public void setChipset(String chipset) {
        this.chipset = chipset;
    }

    public String getDisplayResolution() {
        return displayResolution;
    }

    public void setDisplayResolution(String displayResolution) {
        this.displayResolution = displayResolution;
    }

    public String getDisplaySize() {
        return displaySize;
    }

    public void setDisplaySize(String displaySize) {
        this.displaySize = displaySize;
    }

    public String getOs() {
        return os;
    }

    public void setOs(String os) {
        this.os = os;
    }

    public String getRam() {
        return ram;
    }

    public void setRam(String ram) {
        this.ram = ram;
    }

    public int getReleaseYear() {
        return releaseYear;
    }

    public void setReleaseYear(int releaseYear) {
        this.releaseYear = releaseYear;
    }

    public String getStorage() {
        return storage;
    }

    public void setStorage(String storage) {
        this.storage = storage;
    }

    public String getVideoPixels() {
        return videoPixels;
    }

    public void setVideoPixels(String videoPixels) {
        this.videoPixels = videoPixels;
    }


    public static PhoneDTO toPhoneDTO(Phone phone) {
        if (phone == null)
            return null;
        PhoneDTO phoneDTO = new PhoneDTO();
        phoneDTO.setId(phone.getId());
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

        return phoneDTO;
    }
}
