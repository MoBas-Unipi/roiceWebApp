package it.unipi.dii.dsmt.roice.dto.mapper;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.model.Auction;
import it.unipi.dii.dsmt.roice.model.Phone;

import java.util.Calendar;
import java.util.Date;

public class PhoneMapper {

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

    public static Phone toPhone(PhoneDTO phoneDTO) {
        if (phoneDTO == null)
            return null;

        Phone phone = new Phone();
        phone.setBrand(phoneDTO.getBrand());
        phone.setName(phoneDTO.getName());
        phone.setPicture(phoneDTO.getPicture());
        phone.setBody(phoneDTO.getBody());
        phone.setOs(phoneDTO.getOs());
        phone.setStorage(phoneDTO.getStorage());
        phone.setDisplaySize(phoneDTO.getDisplaySize());
        phone.setDisplayResolution(phoneDTO.getDisplayResolution());
        phone.setCameraPixels(phoneDTO.getCameraPixels());
        phone.setVideoPixels(phoneDTO.getVideoPixels());
        phone.setRam(phoneDTO.getRam());
        phone.setChipset(phoneDTO.getChipset());
        phone.setBatterySize(phoneDTO.getBatterySize());
        phone.setBatteryType(phoneDTO.getBatteryType());
        phone.setReleaseYear(phoneDTO.getReleaseYear());
        phone.setAuction(phoneDTO.getAuction());

        // Creating an Auction object with specified day, month, and year
//        Calendar auctionDate = Calendar.getInstance();
//        auctionDate.set(2024, Calendar.MARCH, 11);
//
//        Calendar auctionDateEnd = Calendar.getInstance();
//        auctionDateEnd.set(2024, Calendar.MAY, 14);

        return phone;
    }

}
