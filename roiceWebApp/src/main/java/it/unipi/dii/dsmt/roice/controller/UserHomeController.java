package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.service.UserHomeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import java.util.ArrayList;

@Controller
public class UserHomeController {

    @Autowired
    private UserHomeService service;

    @GetMapping("/userHome")
    public String userHome(Model model) {

        // Get all phones from the DB
        ArrayList<PhoneDTO> phones = service.getPhones();
        model.addAttribute("phones", phones);

        return "userHome";
    }

}


