package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.service.UserHomeService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.RequestParam;


@Controller
public class UserHomeController {

    @Autowired
    private UserHomeService service;


    @GetMapping("/userHome")
    public String userHome(Model model, HttpSession session, @RequestParam(defaultValue = "0") int page,
                           @RequestParam(defaultValue = "50") int size) {

        // Ensure page is not negative
        if (page < 0) {
            page = 0;
        }

        // Get the current user from the session
        Object currentUser = session.getAttribute("currentUser");
        if (currentUser == null) {
            // User not logged in, redirect to login page
            return "redirect:/login";
        }

        // Extract firstName and lastName from the currentUser object and combine them to get fullName
        String fullName = ((UserDTO) currentUser).getFirstName() + " " + ((UserDTO) currentUser).getLastName();

        // Get a page of PhoneDTO objects from the service layer
        Page<PhoneDTO> phonesPage = service.getPhones(page, size);

        // Add the list of phones, current page number, total number of pages, and the user full name to the model
        model.addAttribute("phones", phonesPage.getContent());
        model.addAttribute("currentPage", page);
        model.addAttribute("totalPages", phonesPage.getTotalPages());
        model.addAttribute("fullName", fullName);

        // Return the name of the userHome view template
        return "userHome";
    }

}


