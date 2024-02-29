package it.unipi.dii.dsmt.roice.repository;

import it.unipi.dii.dsmt.roice.model.Phone;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface PhoneRepository extends MongoRepository<Phone, String> {

}
