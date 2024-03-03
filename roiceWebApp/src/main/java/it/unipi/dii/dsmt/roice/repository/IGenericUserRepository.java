package it.unipi.dii.dsmt.roice.repository;

import it.unipi.dii.dsmt.roice.model.GenericUser;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.Optional;

public interface IGenericUserRepository extends MongoRepository<GenericUser, String> {
    Optional<GenericUser> findByEmail(String email);
}
