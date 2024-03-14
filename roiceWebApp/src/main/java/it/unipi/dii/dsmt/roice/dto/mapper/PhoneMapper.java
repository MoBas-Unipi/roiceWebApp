package it.unipi.dii.dsmt.roice.dto.mapper;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.model.User;

public class PhoneMapper {

    public static PhoneDTO toPhoneDTO(Phone phone) {
        return new PhoneDTO(
                phone.getBrand(),
                phone.getName(),
                phone.getPicture(),
                phone.getBatterySize(),
                phone.getBatteryType(),
                phone.getBody(),
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
}
