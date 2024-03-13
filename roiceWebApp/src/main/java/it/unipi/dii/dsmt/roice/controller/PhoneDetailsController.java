package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.model.PhonePreview;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import it.unipi.dii.dsmt.roice.service.UserService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;

@Controller
public class PhoneDetailsController {

    @Autowired
    private IPhoneRepository phoneRepository;

    @Autowired
    private UserService userService;

    @GetMapping("/phoneDetails")
    public String showPhoneDetails(Model model, HttpSession session, @RequestParam("phoneName") String phoneName) {
        Phone phone = phoneRepository.findByName(phoneName);
        if (phone == null) {
            return "redirect:/homePage";
        }

        if(session.getAttribute("userClass").equals("user")) {
            UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");
            if (currentUser == null) {
                return "redirect:/login";
            }

            List<PhonePreview> favoritePhones = currentUser.getFavoritePhones(); // Assuming you have a method to get favorite phones for the current user

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
        }
        session.setAttribute("phone", phone);
        return "phoneDetails";
    }


    @PostMapping("/phoneDetails")
    public String addFavoritePhone(Model model, HttpSession session) {
        Phone phone = (Phone) session.getAttribute("phone");
        UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");
        if (currentUser == null || phone == null) {
            model.addAttribute("error", "Error in adding the phone to the list of favorite phones!");
            return "phoneDetails";
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
        return "phoneDetails";
    }


    // Remove from favorites
    @PostMapping("/phoneDetails/phone")
    public String removeFromFavorites(Model model, HttpSession session, @RequestParam("phoneName") String phoneName) {
        UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");
        if (currentUser == null || phoneName == null) {
            model.addAttribute("message", "Error in removing the phone to the list of favorite phones!");
            return "phoneDetails";
        }
        UserDTO userDTO = userService.deleteFavoritePhone(currentUser.getEmail(), phoneName);
        if(userDTO == null) {
            model.addAttribute("message", "Error in removing the phone from the list of favorites phones!");
        } else {
            model.addAttribute("message", "Phone removed from the favorites!");
            session.setAttribute("isPhoneInFavorites", false);
            session.setAttribute("currentUser", userDTO);
        }
        return "phoneDetails";
    }

}
