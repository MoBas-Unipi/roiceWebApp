package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.model.GenericUser;
import it.unipi.dii.dsmt.roice.repository.GenericUserRepository;
import it.unipi.dii.dsmt.roice.service.UserService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;

import java.util.HashMap;
import java.util.Map;
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
    public String signup(@Valid UserDTO userDTO, BindingResult bindingResult, Model model) {
        if (bindingResult.hasErrors()) {
            // Collect errors for each field and add them to the error map
            Map<String, String> errorMap = new HashMap<>();
            for (FieldError error : bindingResult.getFieldErrors()) {
                errorMap.put(error.getField(), error.getDefaultMessage());
            }
            model.addAttribute("errorMap", errorMap);
            model.addAttribute("userDTO", userDTO); // Add the UserDTO object back to the model
            return "signup"; // Return to the signup page
        }

        Optional<GenericUser> genericUser = genericUserMongo.findByEmail(userDTO.getEmail());
        if (genericUser.isPresent()) {
            model.addAttribute("error", "E-mail already in use");
            return "signup";
        }

        if (userService.registerUser(userDTO)) {
            return "login";
        } else {
            model.addAttribute("error", "Failed to register user");
            return "signup"; // Return to the signup page
        }
    }

}
