package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.dto.mapper.UserMapper;
import it.unipi.dii.dsmt.roice.model.GenericUser;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.model.PhonePreview;
import it.unipi.dii.dsmt.roice.model.User;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import it.unipi.dii.dsmt.roice.service.UserService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;
import java.util.Optional;

@Controller
public class PhoneDetailsController {

    @Autowired
    private IPhoneRepository phoneRepository;

    @Autowired
    private UserService userService;

    @GetMapping("/phoneDetails")
    public String showPhoneDetails(Model model, HttpSession session, @RequestParam("phoneName") String phoneName) {
        Phone phone = phoneRepository.findByName(phoneName);
        UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");

        if (phone == null || currentUser == null) {
            return "redirect:/userHome";
        }

        List<PhonePreview> favoritePhones = currentUser.getFavoritePhones(); // Assuming you have a method to get favorite phones for the current user

        model.addAttribute("message", "");
        boolean isPhoneInFavorites = false;
        // Check if the phone is already in the user's favorites
        for (PhonePreview phonePreview :favoritePhones) {
            if (phonePreview.getName().equals(phoneName)) {
                isPhoneInFavorites = true;
                model.addAttribute("message", "Phone added to the favorites!");
                break;
            }
        }

        // Add attributes to session
        session.setAttribute("phone", phone);
        session.setAttribute("isPhoneInFavorites", isPhoneInFavorites);

        // Return the phone details view
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
}
