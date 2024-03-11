package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.AdminDTO;
import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.GenericUser;
import it.unipi.dii.dsmt.roice.service.PhoneService;
import it.unipi.dii.dsmt.roice.service.UserHomeService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class AdminHomeController {



    @Autowired
    private UserHomeService userHomeservice;
    @Autowired
    private PhoneService phoneService;


    @GetMapping("/adminHome")
    public String adminHome(Model model, HttpSession session, @RequestParam(defaultValue = "0") int page,
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
        //String fullName = ((AdminDTO) currentUser).getFirstName() + " " + ((AdminDTO) currentUser).getLastName();

        //Extract user class (admin or normal user)
        String userClass = (String) session.getAttribute("userClass");

        // Get a page of PhoneDTO objects from the service layer
        Page<PhoneDTO> phonesPage = userHomeservice.getPhones(page, size);

        // Add the list of phones, current page number, total number of pages, and the user full name to the model
        model.addAttribute("phones", phonesPage.getContent());
        model.addAttribute("currentPage", page);
        model.addAttribute("totalPages", phonesPage.getTotalPages());
        //model.addAttribute("fullName", fullName);
        model.addAttribute("userClass", userClass);
        // Return the name of the userHome view template
        return "adminHome";
    }


    @GetMapping(value = "/adminSearchPhone")
    public String searchPhones(@RequestParam("name") String name,
                               @RequestParam(defaultValue = "0") int page,
                               @RequestParam(defaultValue = "50") int size,
                               Model model, HttpSession session) {

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
        //String fullName = ((UserDTO) currentUser).getFirstName() + " " + ((UserDTO) currentUser).getLastName();

        //Extract user class (admin or normal user)
        String userClass = (String) session.getAttribute("userClass");

        // Execute the queryo to find phones by name
        Page<PhoneDTO> phonesPage = phoneService.searchPhonesByName(name, page, size);

        // Add results to view
        model.addAttribute("phones", phonesPage.getContent());
        model.addAttribute("currentPage", page);
        model.addAttribute("totalPages", phonesPage.getTotalPages());
        //model.addAttribute("fullName", fullName);
        model.addAttribute("paramName", name);
        model.addAttribute("userClass", userClass);

        // Return the name of the searchResults view template
        return "adminSearchPhone";
    }

}
