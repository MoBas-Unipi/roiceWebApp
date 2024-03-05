package it.unipi.dii.dsmt.roice.repository;

import it.unipi.dii.dsmt.roice.model.GenericUser;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface IGenericUserRepository extends MongoRepository<GenericUser, String> {
    Optional<GenericUser> findByEmail(String email);
}
