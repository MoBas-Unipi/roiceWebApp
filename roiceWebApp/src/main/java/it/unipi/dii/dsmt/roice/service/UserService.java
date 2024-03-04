package it.unipi.dii.dsmt.roice.service;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.User;
import it.unipi.dii.dsmt.roice.repository.GenericUserRepository;
import it.unipi.dii.dsmt.roice.utils.Security;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UserService {

    @Autowired
    private GenericUserRepository userRepository;

    public boolean registerUser(UserDTO userDTO) {
        // Generate salt
        String salt = Security.getSalt();
        // Hash the password using salt
        String hashedPassword = Security.getHashedPassword(userDTO.getPassword(), salt);
        // Create a new User object and populate its fields
        User newUser = new User(
                userDTO.getEmail(),
                salt,
                hashedPassword,
                "user", // Assuming user role
                userDTO.getFirstName(),
                userDTO.getLastName(),
                userDTO.getStreetNumber(),
                userDTO.getStreetName(),
                userDTO.getCity(),
                userDTO.getCountry()
        );
        // Save the user to the database
        return userRepository.addUser(newUser);
    }
}
