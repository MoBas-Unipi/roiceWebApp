package it.unipi.dii.dsmt.roice.repository;

import it.unipi.dii.dsmt.roice.model.GenericUser;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface GenericUserRepository extends MongoRepository<GenericUser, String> {
    GenericUser findByEmail(String email);
}
