package it.unipi.dii.dsmt.roice.repository;

import it.unipi.dii.dsmt.roice.model.Phone;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.time.LocalDateTime;
import java.util.Date;
import org.springframework.data.mongodb.repository.Query;



@Repository
public interface IPhoneRepository extends MongoRepository<Phone, String> {

    // Method to search phones by name containing a specific string, ignoring case
    Page<Phone> findByNameContainingIgnoreCase(String name, Pageable pageable);

    Phone findByName(String phoneName);

    @Query("{'auction.startingDate': {$lte : ?0}, 'auction.endDate': {$gte : ?0}}")
    Page<Phone> findPhonesWithActiveAuctions(Date currentDate, Pageable pageable);

}

