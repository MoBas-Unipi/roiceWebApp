package it.unipi.dii.dsmt.roice.service;

import it.unipi.dii.dsmt.roice.repository.GenericUserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class UserService {

    @Autowired
    private GenericUserRepository genericUserRepository;

    

}
