package it.unipi.dii.dsmt.roice.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class SignUpController {

    @GetMapping({
            "/signup"
    })
    public String signup() {
        return "signup";
    }

}