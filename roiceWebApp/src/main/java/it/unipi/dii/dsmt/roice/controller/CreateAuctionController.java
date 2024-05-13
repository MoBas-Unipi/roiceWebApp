package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.AdminDTO;
import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.model.Auction;
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

        Date startingDate = null;
        Date endDate = null;
        Integer minimumPrice = null;

        try {
            startingDate = parseDateString(startingDateString);
            endDate = parseDateString(endDateString);
            minimumPrice = parseInteger(minimumPriceString);

            if(startingDate == null || endDate == null || minimumPrice == null) {
                model.addAttribute("auctionMessage", "Please, fill out all the fields!");
                return "createAuction";
            }

            // Validate if starting date is in the future
            Date now = new Date();
            if (startingDate.before(now)) {
                model.addAttribute("auctionMessage", "Starting date must be in the future!");
                return "createAuction";
            }

            // Validate if end date is after starting date
            if (endDate.compareTo(startingDate) <= 0) {
                model.addAttribute("auctionMessage", "End date must be after starting date!");
                return "createAuction";
            }

        } catch (ParseException | NumberFormatException e) {
            model.addAttribute("auctionMessage", "Please, fill out all the fields!");
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
        session.setAttribute(phoneName, phoneDTO.getName());
        return "redirect:/phoneDetails?phoneName=" + phoneName;
    }

    private Date parseDateString(String dateString) throws ParseException {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");
            return dateFormat.parse(dateString);
    }

    private Integer parseInteger(String integerString) {
        try {
            return Integer.parseInt(integerString);
        } catch (NumberFormatException e) {
            // Handle parse exception
            e.printStackTrace();
            return null;
        }
    }
}
