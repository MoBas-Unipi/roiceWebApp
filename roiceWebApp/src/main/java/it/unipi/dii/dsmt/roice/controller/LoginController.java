package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.repository.GenericUserRepository;
import jakarta.servlet.http.HttpSession;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoginController {

	private GenericUserRepository genericUserMongo;

	@GetMapping({
			"/",
			"/login"
	})
	public String login() {
		return "login";
	}

	@PostMapping("/login")
	public String login(Model model, HttpSession session, @RequestParam("username") String username,
						@RequestParam("password") String password
						) {
		// Perform authentication logic here
		if (isValidUser(username, password)) {
			// Redirect to the home page upon successful login
			return "redirect:/home";
		} else {
			// Add error message and return to the login page
			model.addAttribute("error", "Invalid username or ");
			return "login"; // Return to the login page
		}
	}

	// Example method for authentication (replace with your actual authentication logic)
	private boolean isValidUser(String username, String password) {
		// Perform authentication logic (e.g., validate credentials against a database)
		// For simplicity, let's assume any non-empty email and password are valid
		return username != null && !username.isEmpty() && password != null && !password.isEmpty();
	}
}
