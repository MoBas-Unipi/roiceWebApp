package it.unipi.dii.dsmt.roice.controller;

import jakarta.servlet.http.HttpSession;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;

@Controller
public class LogoutController {

    /**
     * Handles requests for user logout using GET method (used when are used links or directly the URL).
     *
     * @param session The session object to invalidate.
     * @return Redirects to the home page.
     */
    @GetMapping("/logout")
    public String logoutGet(HttpSession session) {
        // Invalidates the session
        session.invalidate();
        // Redirects to the home page
        return "redirect:/";
    }


    /**
     * Handles requests for user logout using the POST method (used when are used buttons or forms).
     *
     * @param session The session object to invalidate.
     * @return Redirects to the home page.
     */
    @PostMapping("/logout")
    public String logoutPost(HttpSession session) {
        // Invalidates the session
        session.invalidate();
        // Redirects to the home page
        return "redirect:/";
    }


}
