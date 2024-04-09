package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.AdminDTO;
import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.Auction;
import it.unipi.dii.dsmt.roice.service.PhoneService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

@Controller
public class CreateAuctionController {

    @Autowired
    private PhoneService phoneService;

    @GetMapping("/phoneDetails/createAuction")
    public String showCreateAuction (HttpSession session) {

        AdminDTO currenAdmin = (AdminDTO) session.getAttribute("currentUser");
        if (currenAdmin == null) {
            return "redirect:/login";
        }
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
            return "createAuction";
        } else {
            model.addAttribute("message", "Auction added for this phone!");
        }
        session.setAttribute("isAuctionPresent", true);
        session.setAttribute("phone", phoneDTO);
        return "phoneDetails";
    }

    private Date parseDateString(String dateString) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");
        try {
            return dateFormat.parse(dateString);
        } catch (Exception e) {
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
