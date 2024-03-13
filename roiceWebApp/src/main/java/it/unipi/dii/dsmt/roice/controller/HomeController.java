package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.service.PhoneService;
import it.unipi.dii.dsmt.roice.service.HomeService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.RequestParam;


@Controller
public class HomeController {

    @Autowired
    private HomeService homeservice;
    @Autowired
    private PhoneService phoneService;


    /**
     * Handles requests for the home page.
     *
     * @param model     The model to add attributes for the view.
     * @param session   The session object to retrieve user information.
     * @param page      The page number.
     * @param size      The size of each page.
     * @return          The name of the view template to render.
     */
    @GetMapping("/homePage")
    public String homePage(Model model, HttpSession session, @RequestParam(defaultValue = "0") int page,
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

        // Extract user class (admin or normal user)
        String userClass = (String) session.getAttribute("userClass");


        // Get a page of PhoneDTO objects from the service layer
        Page<PhoneDTO> phonesPage = phoneService.getPhones(page, size);

        // Add the list of phones, current page number, total number of pages, and the user full name to the model
        model.addAttribute("phones", phonesPage.getContent());
        model.addAttribute("currentPage", page);
        model.addAttribute("totalPages", phonesPage.getTotalPages());
        model.addAttribute("userClass", userClass);

        // Return the name of the homePage view template
        return "homePage";
    }


    /**
     * Handles the search for phones by name.
     *
     * @param name      The name to search for.
     * @param page      The page number.
     * @param size      The size of each page.
     * @param model     The model to add attributes for the view.
     * @param session   The session object to retrieve user information.
     * @return          The name of the view template to render.
     */
    @GetMapping(value = "/searchPhone")
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

        // Extract user class (admin or normal user)
        String userClass = (String) session.getAttribute("userClass");


        // Execute the query to find phones by name
        Page<PhoneDTO> phonesPage = phoneService.searchPhonesByName(name, page, size);

        // Add results to view
        model.addAttribute("phones", phonesPage.getContent());
        model.addAttribute("currentPage", page);
        model.addAttribute("totalPages", phonesPage.getTotalPages());
        model.addAttribute("paramName", name);
        model.addAttribute("userClass", userClass);

        // Return the name of the searchPhone view template
        return "searchPhone";
    }


}


