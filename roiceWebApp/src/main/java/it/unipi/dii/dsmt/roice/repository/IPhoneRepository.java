package it.unipi.dii.dsmt.roice.repository;

import it.unipi.dii.dsmt.roice.model.Phone;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.Optional;

@Repository
public interface IPhoneRepository extends MongoRepository<Phone, String> {

    // Method to search phones by name containing a specific string, ignoring case
    Page<Phone> findByNameContainingIgnoreCase(String name, Pageable pageable);
    Optional<Phone> findByName(String name);
}

