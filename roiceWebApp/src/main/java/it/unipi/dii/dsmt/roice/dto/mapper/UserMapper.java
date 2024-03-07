package it.unipi.dii.dsmt.roice.dto.mapper;

import it.unipi.dii.dsmt.roice.dto.AdminDTO;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.Admin;
import it.unipi.dii.dsmt.roice.model.User;

public class UserMapper {

    public static UserDTO toUserDTO(User user) {
        return new UserDTO(
                user.getFirstName(),
                user.getLastName(),
                user.getStreetNumber(),
                user.getStreetName(),
                user.getCity(),
                user.getCountry(),
                user.getEmail(),
                user.getHashedPassword(),
                user.getFavoritePhones(),
                user.getNotifications(),
                user.getAuctionsWon()
        );
    }

    public static User toUser(UserDTO userDTO, String salt, String hashedPassword) {

        return new User(
                userDTO.getEmail(), salt,
                hashedPassword,
                "user",
                userDTO.getFirstName(),
                userDTO.getLastName(),
                userDTO.getStreetNumber(),
                userDTO.getStreetName(),
                userDTO.getCity(),
                userDTO.getCountry()
        );
    }

    public static AdminDTO toAdminDTO (Admin admin) {
        return new AdminDTO(
                admin.getEmail(),
                admin.getHashedPassword()
        );
    }

    public static Admin toAdmin(AdminDTO adminDTO) {
        return new Admin(
                adminDTO.getEmail(),
                "",
                adminDTO.getPassword(),
                "admin"
        );
    }
}
