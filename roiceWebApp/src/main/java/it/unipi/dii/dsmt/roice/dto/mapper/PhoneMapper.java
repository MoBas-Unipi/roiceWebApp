package it.unipi.dii.dsmt.roice.dto.mapper;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.model.Auction;
import it.unipi.dii.dsmt.roice.model.Phone;

import java.util.Calendar;
import java.util.Date;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.model.User;

public class PhoneMapper {

    public static PhoneDTO toPhoneDTO(Phone phone) {
        return new PhoneDTO(
                phone.getName(),
                phone.getPicture(),
                phone.getBatterySize(),
                phone.getBatteryType(),
                phone.getBody(),
                phone.getBrand(),
                phone.getCameraPixels(),
                phone.getChipset(),
                phone.getDisplayResolution(),
                phone.getDisplaySize(),
                phone.getOs(),
                phone.getRam(),
                phone.getReleaseYear(),
                phone.getStorage(),
                phone.getVideoPixels(),
                phone.getAuction()
        );
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
