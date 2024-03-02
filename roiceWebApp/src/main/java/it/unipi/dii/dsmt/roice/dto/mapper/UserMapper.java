package it.unipi.dii.dsmt.roice.dto.mapper;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
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
                user.getHashedPassword()
        );
    }

    public static User toUser(UserDTO userDTO) {

        return new User(
                userDTO.getEmail(), "",// salt
                userDTO.getPassword(),
                "user",
                userDTO.getFirstName(),
                userDTO.getLastName(),
                userDTO.getStreetNumber(),
                userDTO.getStreetName(),
                userDTO.getCity(),
                userDTO.getCountry()
        );
    }
}
