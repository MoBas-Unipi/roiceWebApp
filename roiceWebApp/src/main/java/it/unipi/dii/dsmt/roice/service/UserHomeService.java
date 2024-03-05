package it.unipi.dii.dsmt.roice.service;


import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.repository.PhoneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class UserHomeService {

    @Autowired
    PhoneRepository phoneRepository;

    public ArrayList<PhoneDTO> getPhones() {
        List<Phone> phones = phoneRepository.findAll(PageRequest.of(
                0,
                50,
                Sort.by(Sort.Direction.DESC,"releaseYear")
        )).getContent();
        ArrayList<PhoneDTO> myPhones = new ArrayList<>();
        for (Phone myPhone : phones)
            myPhones.add(PhoneDTO.toPhoneDTO(myPhone));
        return myPhones;
    }
}
