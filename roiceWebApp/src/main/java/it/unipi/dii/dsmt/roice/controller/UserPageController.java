package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.AuctionWon;
import it.unipi.dii.dsmt.roice.model.Notification;
import it.unipi.dii.dsmt.roice.model.PhonePreview;
import jakarta.servlet.http.HttpSession;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

import java.time.LocalDateTime;
import java.util.Date;

@Controller
public class UserPageController {

    @GetMapping("/userPage")
    public String userPage(HttpSession session) {
        UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");
        if (currentUser == null) {
            return "redirect:/login";
        }

        currentUser.addAuctionWon(new AuctionWon("Nokia 3210",
                "https://fdn2.gsmarena.com/vv/bigpic/no3210b.gif",
                new Date(), 120
                ));

        currentUser.addNotification(new Notification("Notification",
                "Body dasdkasmldmkadlmaskdlaskddmdkalmdkasldmkasldmkasldmkaldm"));

        currentUser.addFavoritePhone(new PhonePreview("Nokia 3210",
                "https://fdn2.gsmarena.com/vv/bigpic/no3210b.gif"));

        return "userPage";
    }

}
