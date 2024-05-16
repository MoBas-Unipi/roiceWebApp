package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.AdminDTO;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.*;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import it.unipi.dii.dsmt.roice.service.PhoneService;
import it.unipi.dii.dsmt.roice.service.UserService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.text.SimpleDateFormat;
import java.util.*;

@Controller
public class PhoneDetailsController {

    @Autowired
    private IPhoneRepository phoneRepository;

    @Autowired
    private PhoneService phoneService;

    @Autowired
    private UserService userService;

    @GetMapping("/phoneDetails")
    public String showPhoneDetails(Model model, HttpSession session, @RequestParam("phoneName") String phoneName) {

        if (session.getAttribute("userClass") == null) {
            return "redirect:/login";
        }

        Phone phone = phoneRepository.findByName(phoneName);
        if (phone == null) {
            return "redirect:/homePage";
        }
        Auction auction = phone.getAuction();
        if (auction != null) {
            session.setAttribute("isAuctionPresent", true);
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");
            try {
                String startDateString = sdf.format(auction.getStartingDate());
                String endDateString = sdf.format(auction.getEndDate());
                model.addAttribute("startDate", startDateString);
                model.addAttribute("endDate", endDateString);
            } catch (Exception e) {
                e.printStackTrace();
                return "redirect:/homePage";
            }
        } else {
            session.setAttribute("isAuctionPresent", false);
        }

        if (session.getAttribute("userClass").equals("user")) {
            UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");
            if (currentUser == null) {
                return "redirect:/login";
            }

            List<PhonePreview> favoritePhones = currentUser.getFavoritePhones();

            model.addAttribute("message", "");
            boolean isPhoneInFavorites = false;
            // Check if the phone is already in the user's favorites
            for (PhonePreview phonePreview : favoritePhones) {
                if (phonePreview.getName().equals(phoneName)) {
                    isPhoneInFavorites = true;
                    model.addAttribute("message", "Phone added to the favorites!");
                    break;
                }
            }
            session.setAttribute("isPhoneInFavorites", isPhoneInFavorites);
        } else {
            AdminDTO currentAdmin = (AdminDTO) session.getAttribute("currentUser");
            if (currentAdmin == null) {
                return "redirect:/login";
            }
            if (auction != null) {
                model.addAttribute("message", "Auction added for this phone!");
                session.setAttribute("isAuctionPresent", true);
                SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");
                try {
                    String startDateString = sdf.format(auction.getStartingDate());
                    String endDateString = sdf.format(auction.getEndDate());
                    model.addAttribute("startDate", startDateString);
                    model.addAttribute("endDate", endDateString);
                } catch (Exception e) {
                    e.printStackTrace();
                    return "redirect:/homePage";
                }
            }
        }
        session.setAttribute("phone", phone);
        return "phoneDetails";
    }


    @PostMapping("/phoneDetails")
    public String addFavoritePhone(Model model, HttpSession session) {
        Phone phone = (Phone) session.getAttribute("phone");
        UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");
        if (currentUser == null) {
            return "redirect:/login";
        }
        if (phone == null) {
            model.addAttribute("error", "Error in adding the phone to the list of favorite phones!");
            return "phoneDetails";
        }
        Auction auction = phone.getAuction();
        if (auction != null) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");
            String startDateString = sdf.format(auction.getStartingDate());
            String endDateString = sdf.format(auction.getEndDate());
            model.addAttribute("startDate", startDateString);
            model.addAttribute("endDate", endDateString);
        }
        int result = userService.addPhonePreview(currentUser, phone);
        if (result == -2) {
            model.addAttribute("message", "Error in updating the user information in the database!");
        } else if (result == -1) {
            session.setAttribute("isPhoneInFavorites", true);
            model.addAttribute("message", "Phone added to favorites!");
        } else {
            session.setAttribute("currentUser", currentUser);
            session.setAttribute("isPhoneInFavorites", true);
            model.addAttribute("message", "Phone added to favorites!");
        }
        // Redirecting to the phone details page
        return "/phoneDetails";
    }


    // Remove from favorites
    @PostMapping("/phoneDetails/phone")
    public String removeFromFavorites(Model model, HttpSession session, @RequestParam("phoneName") String phoneName) {
        UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");
        if (currentUser == null) {
            return "redirect:/login";
        }
        if (phoneName == null) {
            model.addAttribute("message", "Error in removing the phone to the list of favorite phones!");
            return "phoneDetails";
        }
        Phone phone = phoneRepository.findByName(phoneName);
        Auction auction = phone.getAuction();
        if (auction != null) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");
            String startDateString = sdf.format(auction.getStartingDate());
            String endDateString = sdf.format(auction.getEndDate());
            model.addAttribute("startDate", startDateString);
            model.addAttribute("endDate", endDateString);
        }
        UserDTO userDTO = userService.deleteFavoritePhone(currentUser.getEmail(), phoneName);
        if (userDTO == null) {
            model.addAttribute("message", "Error in removing the phone from the list of favorites phones!");
        } else {
            model.addAttribute("message", "Phone removed from the favorites!");
            session.setAttribute("isPhoneInFavorites", false);
            session.setAttribute("currentUser", userDTO);
        }
        return "/phoneDetails";
    }


    @PostMapping("/handleAuctionEnd")
    @ResponseStatus(value = HttpStatus.OK)
    public void handleAuctionEnd(@RequestBody Map<String, Object> requestBody) {
        String phoneName = (String) requestBody.get("phone");

        // Case in which there are bidders
        if (!requestBody.get("winner").equals("No bidders")) {
            String winnerEmail = (String) requestBody.get("winner");
            int winningBid = (Integer)requestBody.get("winningBidValue");

            Optional<GenericUser> winnerUser = userService.findByEmail(winnerEmail);
            // Add auction won to the winner user
            winnerUser.ifPresent(genericUser ->
                    userService.addWonAuction(genericUser, phoneName, winningBid));
        }
        // Remove the auction attribute from the phone document searching with phone name
        phoneService.removeAuctionByName(phoneName);
    }
}

