package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.service.UserService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import javax.validation.Valid;

@Controller
public class SignUpController {

    @Autowired
    private UserService userService;

    @GetMapping({
            "/signup"
    })
    public String signup() {
        return "signup";
    }



    @PostMapping("/signup")
    public String signup(@Valid @RequestBody UserDTO userDTO, BindingResult bindingResult, HttpSession session, Model model) {
        if (bindingResult.hasErrors()) {
            model.addAttribute("error", "Validation error occurred");
            return "signup"; // Return to the signup page
        }

        boolean registrationSuccess = userService.registerUser(userDTO);

        if (registrationSuccess) {
            session.setAttribute("currentUser", userDTO.getEmail());
            return "redirect:/home";
        } else {
            model.addAttribute("error", "Failed to register user");
            return "signup"; // Return to the signup page
        }
    }

}
