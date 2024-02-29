package it.unipi.dii.dsmt.roice.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoginController {

	@GetMapping({
			"/",
			"/login"
	})
	public String login() {
		return "login";
	}

}
