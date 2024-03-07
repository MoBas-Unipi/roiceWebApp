package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.AuctionWon;
import it.unipi.dii.dsmt.roice.model.Notification;
import it.unipi.dii.dsmt.roice.model.PhonePreview;
import jakarta.servlet.http.HttpSession;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import java.util.Calendar;
import java.util.Date;

@Controller
public class UserPageController {

    @GetMapping("/userPage")
    public String userPage(HttpSession session) {
        UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");

//        Calendar calendar = Calendar.getInstance();
//
//        // Set the desired date components
//        int year = 2024;
//        int month = Calendar.MARCH; // Month is zero-based, so March is represented by 2
//        int dayOfMonth = 7;
//
//        // Set the calendar with the desired date
//        calendar.set(year, month, dayOfMonth);
//
//        // Get the Date object from the Calendar instance
//        Date date = calendar.getTime();

        currentUser.addAuctionWon(new AuctionWon("Nokia 3210",
                "https://fdn2.gsmarena.com/vv/bigpic/no3210b.gif",
                new Date(), 120
                ));

        currentUser.addNotification(new Notification("Notification",
                "Body dasdkasmldmkadlmaskdlaskddmdkalmdkasldmkasldmkasldmkaldm"));

        currentUser.addfavoritePhone(new PhonePreview("Nokia 3210",
                "https://fdn2.gsmarena.com/vv/bigpic/no3210b.gif"));

        return "userPage";
    }

}
