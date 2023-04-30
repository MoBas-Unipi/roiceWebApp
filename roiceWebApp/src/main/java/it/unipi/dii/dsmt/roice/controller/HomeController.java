package it.unipi.dii.dsmt.roice.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class HomeController {

	@GetMapping({
			"/",
			"/home"
	})
	public String home(@RequestParam(value = "name", defaultValue = "World",
			required = true) String name, Model model) {
		model.addAttribute("name", name);
		return "home";
	}
}
