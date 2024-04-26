package it.unipi.dii.dsmt.roice.service;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.dto.mapper.PhoneMapper;
import it.unipi.dii.dsmt.roice.model.Auction;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Date;

@Service
public class PhoneService {

    @Autowired
    private IPhoneRepository phoneRepository;

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

        // Returning the page of PhoneDTO objects
        return phonePage.map(PhoneMapper::toPhoneDTO);
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

        // Return the page of PhoneDTO objects
        return phonePage.map(PhoneMapper::toPhoneDTO);
    }

    public PhoneDTO addAuction(String phoneName, Auction auction) {
        try {
            Phone phone = phoneRepository.findByName(phoneName);
            if (phone == null) {
                return null;
            }
            phone.setAuction(auction);
            phoneRepository.save(phone);
            return PhoneMapper.toPhoneDTO(phone);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }



    /**
     * Retrieves a page of PhoneDTO objects using a custom query to find phones with live auctions.
     *
     * @param page  The page number.
     * @param size  The size of each page.
     * @return      A Page containing PhoneDTO objects with live auctions.
     */
    public Page<PhoneDTO> getPhonesWithLiveAuctions(int page, int size) {
        // Create a Pageable object for pagination
        Pageable pageable = PageRequest.of(page, size);

        // Get the current date
        Date currentDate = new Date();

        // Execute a query to find phones with active auctions
        Page<Phone> phonePage = phoneRepository.findPhonesWithActiveAuctions(currentDate, pageable);

        // Map each Phone entity to a PhoneDTO using the toPhoneDTO method in the PhoneDTO class
        Page<PhoneDTO> phoneDTOPage = phonePage.map(PhoneMapper::toPhoneDTO);

        // Return the page of PhoneDTO objects
        return phoneDTOPage;
    }


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


    /**
     * Removes the auction from the specific phone document
     * @param phoneName name of the phone to update removing the auction
     */
    public void removeAuctionByName(String phoneName) {
        try {
            Phone phone = phoneRepository.findByName(phoneName);
            if (phone != null) {
                phone.setAuction(null);
                phoneRepository.save(phone);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


}
