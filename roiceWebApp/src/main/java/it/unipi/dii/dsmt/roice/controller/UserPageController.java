package it.unipi.dii.dsmt.roice.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class UserPageController {

    @GetMapping("/userPage")
    public String userPage(Model model) {
        return "userPage";
    }

}
