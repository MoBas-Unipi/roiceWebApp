package it.unipi.dii.dsmt.roice.service;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
public class PhoneService {

    @Autowired
    IPhoneRepository phoneRepository;

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
