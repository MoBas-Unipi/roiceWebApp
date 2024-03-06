package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.service.UserHomeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.ArrayList;

@Controller
public class UserHomeController {

    @Autowired
    private UserHomeService service;


    @GetMapping("/userHome")
    public String userHome(Model model, @RequestParam(defaultValue = "0") int page,
                           @RequestParam(defaultValue = "50") int size) {

        if (page < 0) {
            page = 0;
        }

        // Get phones for the current page
        Page<PhoneDTO> phonesPage = service.getPhones(page, size);

        // Add phones and pagination information to the model
        model.addAttribute("phones", phonesPage.getContent());
        model.addAttribute("currentPage", page);
        model.addAttribute("totalPages", phonesPage.getTotalPages());

        return "userHome";
    }


    /*
    @GetMapping("/userHome")
    public String userHome(Model model) {
        // Get all phones from the DB
        ArrayList<PhoneDTO> phones = service.getPhones();
        model.addAttribute("phones", phones);
        return "userHome";
    }
     */

}


