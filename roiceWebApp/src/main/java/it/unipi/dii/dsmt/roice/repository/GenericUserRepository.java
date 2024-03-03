package it.unipi.dii.dsmt.roice.repository;

import it.unipi.dii.dsmt.roice.model.GenericUser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class GenericUserRepository {

    public GenericUserRepository() {
    }

    @Autowired
    private IGenericUserRepository genericUserRepository;

    public boolean addUser(GenericUser user) {
        boolean result = true;
        try {
            genericUserRepository.save(user);
        } catch (Exception e) {
            e.printStackTrace();
            result = false;
        }
        return result;
    }

    public Optional<GenericUser> findByEmail(String email) {
        Optional<GenericUser> user = Optional.empty();
        try {
            user = genericUserRepository.findByEmail(email);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return user;
    }
}




