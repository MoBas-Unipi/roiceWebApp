package it.unipi.dii.dsmt.roice.service;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
public class PhoneService {

    @Autowired
    IPhoneRepository phoneRepository;

    /**
     * Retrieves a page of PhoneDTO objects using "findAll" query
     *
     * @param page  The page number.
     * @param size  The size of each page.
     * @return      A Page containing PhoneDTO objects.
     */
    public Page<PhoneDTO> getPhones(int page, int size) {
        // Creating a pageable object to specify pagination and sorting options
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "releaseYear"));

        // Retrieving a page of Phone entities from the phoneRepository sorted by releaseYear
        Page<Phone> phonePage = phoneRepository.findAll(pageable);

        // Mapping each Phone entity to a PhoneDTO using the static method toPhoneDTO in PhoneDTO class
        Page<PhoneDTO> phoneDTOPage = phonePage.map(phone -> PhoneDTO.toPhoneDTO(phone));

        // Returning the page of PhoneDTO objects
        return phoneDTOPage;
    }


    /**
     * Searches for phones by name and returns a page of PhoneDTO objects using "findByNameContainingIgnoreCase" query
     *
     * @param name  The name to search for.
     * @param page  The page number.
     * @param size  The size of each page.
     * @return      A Page containing PhoneDTO objects matching the search criteria.
     */
    public Page<PhoneDTO> searchPhonesByName(String name, int page, int size) {
        // Create a Pageable object for pagination
        Pageable pageable = PageRequest.of(page, size);

        // Execute a query to search phones by name using the findByNameContainingIgnoreCase method
        Page<Phone> phonePage = phoneRepository.findByNameContainingIgnoreCase(name, pageable);

        // Map each Phone entity to a PhoneDTO using the toPhoneDTO method in the PhoneDTO class
        Page<PhoneDTO> phoneDTOPage = phonePage.map(phone -> PhoneDTO.toPhoneDTO(phone));

        // Return the page of PhoneDTO objects
        return phoneDTOPage;
    }

}