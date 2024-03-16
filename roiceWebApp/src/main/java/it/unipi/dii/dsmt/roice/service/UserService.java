package it.unipi.dii.dsmt.roice.service;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.dto.mapper.UserMapper;
import it.unipi.dii.dsmt.roice.model.GenericUser;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.model.PhonePreview;
import it.unipi.dii.dsmt.roice.model.User;
import it.unipi.dii.dsmt.roice.repository.IGenericUserRepository;
import it.unipi.dii.dsmt.roice.utils.Security;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Service
public class UserService {

    @Autowired
    private IGenericUserRepository userRepository;

    public boolean registerUser(UserDTO userDTO) {
        // Generate salt
        String salt = Security.getSalt();
        // Hash the password using salt
        String hashedPassword = Security.getHashedPassword(userDTO.getPassword(), salt);
        // Create a new User object and populate its fields
        User newUser = UserMapper.toUser(userDTO, salt, hashedPassword);
        // Save the user to the database
        return this.addUser(newUser);
    }

    public boolean addUser(GenericUser user) {
        boolean result = true;
        try {
            userRepository.save(user);
        } catch (Exception e) {
            e.printStackTrace();
            result = false;
        }
        return result;
    }

    public Optional<GenericUser> findByEmail(String email) {
        Optional<GenericUser> user = Optional.empty();
        try {
            user = userRepository.findByEmail(email);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return user;
    }

    public int addPhonePreview(UserDTO currentUser, Phone phone) {
        try {
            Optional<GenericUser> genericUser = this.findByEmail(currentUser.getEmail());
            if (genericUser.isPresent()) {
                User user = (User) genericUser.get();
                List<PhonePreview> favoritePhones = user.getFavoritePhones();
                for (PhonePreview phonePW : favoritePhones) {
                    if (Objects.equals(phonePW.getName(), phone.getName())) {
                        return -1;
                    }
                }
                PhonePreview phonePreview = new PhonePreview(phone.getName(), phone.getPicture());
                favoritePhones.add(phonePreview);
                user.setFavoritePhones(favoritePhones);
                userRepository.save(user);
                currentUser.setFavoritePhones(favoritePhones);
                return 0;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return -2;
    }

    public UserDTO deleteFavoritePhone(String userEmail, String phoneName) {
        try {
            Optional<GenericUser> optionalUser = userRepository.findByEmail(userEmail);
            if (optionalUser.isPresent()) {
                User user = (User) optionalUser.get();
                user.getFavoritePhones().removeIf(phone -> phone.getName().equals(phoneName));
                userRepository.save(user);
                return UserMapper.toUserDTO(user);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

}
