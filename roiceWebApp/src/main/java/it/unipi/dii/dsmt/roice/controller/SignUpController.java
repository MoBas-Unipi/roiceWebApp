package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.GenericUser;
import it.unipi.dii.dsmt.roice.repository.GenericUserRepository;
import it.unipi.dii.dsmt.roice.service.UserService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;

import javax.validation.Valid;
import java.util.Optional;

@Controller
public class SignUpController {

    @Autowired
    private UserService userService;

    @Autowired
    private GenericUserRepository genericUserMongo;

    @GetMapping({
            "/signup"
    })
    public String signup() {
        return "signup";
    }

    @PostMapping("/signup")
    public String signup(@Valid UserDTO userDTO, BindingResult bindingResult, HttpSession session, Model model) {
        if (bindingResult.hasErrors()) {
            model.addAttribute("error", "Validation error occurred");
            return "signup"; // Return to the signup page
        }

        Optional<GenericUser> genericUser = genericUserMongo.findByEmail(userDTO.getEmail());
        if (genericUser.isPresent()) {
            model.addAttribute("error", "E-mail already in use");
            return "signup";
        }

        boolean registrationSuccess = userService.registerUser(userDTO);

        if (registrationSuccess) {
            return "login";
        } else {
            model.addAttribute("error", "Failed to register user");
            return "signup"; // Return to the signup page
        }
    }

}
