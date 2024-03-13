package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.AdminDTO;
import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.dto.mapper.UserMapper;
import it.unipi.dii.dsmt.roice.model.Admin;
import it.unipi.dii.dsmt.roice.model.GenericUser;
import it.unipi.dii.dsmt.roice.model.User;
import it.unipi.dii.dsmt.roice.repository.IGenericUserRepository;
import it.unipi.dii.dsmt.roice.utils.Security;
import jakarta.servlet.http.HttpSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.Optional;

@Controller
public class LoginController {

	@Autowired
	private IGenericUserRepository genericUserMongo;
	private final static Logger logger = LoggerFactory.getLogger(LoginController.class);

	@GetMapping({
			"/",
			"/login"
	})
	public String login() {
		return "login";
	}

	@PostMapping("/login")
	public String login(Model model, HttpSession session, @RequestParam("email") String email,
						@RequestParam("password") String password
						) {
		// Perform authentication logic here
		if (isValidUser(email, password)) {
			Optional<GenericUser> genericUser = genericUserMongo.findByEmail(email);
			if (genericUser.isEmpty()) {
				model.addAttribute("error", "Invalid email or password");
				return "login";
			} else {
				String salt = genericUser.get().getSalt();
				String hashedPassword = Security.getHashedPassword(password, salt);
				if (!genericUser.get().getHashedPassword().equals(hashedPassword)) {
					model.addAttribute("error", "Invalid email or password");
					return "login";
				}
				if (genericUser.get().get_class().equals("admin")) {
					Admin admin = (Admin) genericUser.get();
					AdminDTO adminDTO = UserMapper.toAdminDTO(admin);
					session.setAttribute("currentUser", adminDTO);
					session.setAttribute("userClass", "admin");
					return "redirect:/homePage";
				} else {
					User user = (User) genericUser.get();
					UserDTO userDTO = UserMapper.toUserDTO(user);
					session.setAttribute("currentUser", userDTO);
					session.setAttribute("userClass", "user");
					return "redirect:/homePage";
				}
			}
		} else {
			// Add error message and return to the login page
			model.addAttribute("error", "Invalid email or password");
			return "login"; // Return to the login page
		}
	}

	private boolean isValidUser(String username, String password) {
		return username != null && !username.isEmpty() && password != null && !password.isEmpty();
	}
}
