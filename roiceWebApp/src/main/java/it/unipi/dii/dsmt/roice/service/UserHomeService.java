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
public class UserHomeService {

    @Autowired
    IPhoneRepository phoneRepository;


    public Page<PhoneDTO> getPhones(int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "releaseYear"));

        Page<Phone> phonePage = phoneRepository.findAll(pageable);

        Page<PhoneDTO> phoneDTOPage = phonePage.map(phone -> PhoneDTO.toPhoneDTO(phone));

        return phoneDTOPage;
    }

    /*
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
    */
}
