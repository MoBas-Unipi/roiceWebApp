package it.unipi.dii.dsmt.roice.controller;

import jakarta.servlet.http.HttpSession;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class CreateAuctionController {

    @GetMapping("/phoneDetails/createAuction")
    public String showCreateAuction (Model model, HttpSession session) {
        return "createAuction";
    }
}
