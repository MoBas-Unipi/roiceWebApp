package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.model.Auction;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import it.unipi.dii.dsmt.roice.service.PhoneService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

@Controller
public class CreateAuctionController {

    @Autowired
    private PhoneService phoneService;

    @GetMapping("/phoneDetails/createAuction")
    public String showCreateAuction () {
        return "createAuction";
    }

    @PostMapping("/phoneDetails/createAuction")
    public String createAuction(Model model, HttpSession session, @RequestParam("phoneName") String phoneName,
                                @RequestParam("startingDate") String startingDateString, @RequestParam("endDate") String endDateString,
                                @RequestParam("minimumPrice") String minimumPriceString) {

        Date startingDate = parseDateString(startingDateString);
        Date endDate = parseDateString(endDateString);
        Double minimumPrice = parseDouble(minimumPriceString);

        if (startingDate == null || endDate == null || minimumPrice == null) {
            model.addAttribute("auctionMessage", "Error parsing input data!");
            return "createAuction";
        }

        PhoneDTO phoneDTO = phoneService.addAuction(phoneName, new Auction(startingDate, endDate, minimumPrice));

        if (phoneDTO == null) {
            model.addAttribute("auctionMessage", "Error in adding the auction!");
        } else {
            model.addAttribute("auctionMessage", "Auction added!");
        }
        session.setAttribute("phone", phoneDTO);
        return "createAuction";
    }

    private Date parseDateString(String dateString) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");
        try {
            return dateFormat.parse(dateString);
        } catch (ParseException e) {
            // Handle parse exception
            e.printStackTrace();
            return null;
        }
    }

    private Double parseDouble(String doubleString) {
        try {
            return Double.parseDouble(doubleString);
        } catch (NumberFormatException e) {
            // Handle parse exception
            e.printStackTrace();
            return null;
        }
    }
}
