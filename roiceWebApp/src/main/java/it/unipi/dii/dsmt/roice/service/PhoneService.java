package it.unipi.dii.dsmt.roice.service;

import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PhoneService {

    @Autowired
    private IPhoneRepository phoneRepository;

    public boolean addPhone(Phone phone) {
        boolean result = true;
        try {
            phoneRepository.save(phone);
        } catch (Exception e) {
            e.printStackTrace();
            result = false;
        }
        return result;
    }

}
