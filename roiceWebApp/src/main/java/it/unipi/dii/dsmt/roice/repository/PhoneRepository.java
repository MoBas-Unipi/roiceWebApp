package it.unipi.dii.dsmt.roice.repository;

import it.unipi.dii.dsmt.roice.model.Phone;
import org.springframework.data.domain.Page;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.List;

public interface PhoneRepository extends MongoRepository<Phone, String> {
    List<Phone> findByName(String name);


}
