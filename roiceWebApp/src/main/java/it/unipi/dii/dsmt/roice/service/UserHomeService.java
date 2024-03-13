package it.unipi.dii.dsmt.roice.service;


import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.dto.mapper.PhoneMapper;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
public class UserHomeService {

    @Autowired
    IPhoneRepository phoneRepository;


    public Page<PhoneDTO> getPhones(int page, int size) {
        // Creating a pageable object to specify pagination and sorting options
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "releaseYear"));

        // Retrieving a page of Phone entities from the phoneRepository sorted by releaseYear
        Page<Phone> phonePage = phoneRepository.findAll(pageable);

        // Mapping each Phone entity to a PhoneDTO using the static method toPhoneDTO in PhoneDTO class
        Page<PhoneDTO> phoneDTOPage = phonePage.map(phone -> PhoneMapper.toPhoneDTO(phone));

        // Returning the page of PhoneDTO objects
        return phoneDTOPage;
    }


    public Page<PhoneDTO> searchPhonesByName(String name, int page, int size) {
        // Create a Pageable object for pagination
        Pageable pageable = PageRequest.of(page, size);

        // Execute a query to search phones by name using the findByNameContainingIgnoreCase method
        Page<Phone> phonePage = phoneRepository.findByNameContainingIgnoreCase(name, pageable);

        // Map each Phone entity to a PhoneDTO using the toPhoneDTO method in the PhoneDTO class
        Page<PhoneDTO> phoneDTOPage = phonePage.map(phone -> PhoneMapper.toPhoneDTO(phone));

        // Return the page of PhoneDTO objects
        return phoneDTOPage;
    }


}
