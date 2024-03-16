package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.PhoneDTO;
import it.unipi.dii.dsmt.roice.dto.mapper.PhoneMapper;
import it.unipi.dii.dsmt.roice.model.Phone;
import it.unipi.dii.dsmt.roice.repository.IPhoneRepository;
import it.unipi.dii.dsmt.roice.service.PhoneService;
import jakarta.servlet.http.HttpSession;
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
public class AdminAddPhoneController {

    @Autowired
    private PhoneService phoneService;

    @Autowired
    private IPhoneRepository phoneRepository;

    @GetMapping({
            "/addPhone"
    })
    public String addPhone() {
        return "adminAddPhone";
    }

    @PostMapping("/addPhone")
    public String addPhone(@Valid PhoneDTO phoneDTO, BindingResult bindingResult, HttpSession session, Model model) {
        // Clear the success message if it exists
        session.removeAttribute("success");

        if (bindingResult.hasErrors()) {
            // Collect errors for each field and add them to the error map
            Map<String, String> errorMap = new HashMap<>();
            for (FieldError error : bindingResult.getFieldErrors()) {
                errorMap.put(error.getField(), error.getDefaultMessage());
            }
            model.addAttribute("errorMap", errorMap);
            return "adminAddPhone"; // Return to the signup page
        }

        Phone phone = phoneRepository.findByName(phoneDTO.getName());
        if (phone != null) {
            model.addAttribute("error", "Phone already present");
            return "adminAddPhone";
        }

        Phone newPhone = PhoneMapper.toPhone(phoneDTO);

        if (phoneService.addPhone(newPhone)) {
            session.setAttribute("success", "Phone added successfully");
            return "redirect:/addPhone"; // Redirect to clear form after successful addition
        } else {
            model.addAttribute("error", "Failed to add phone");
            return "adminAddPhone"; // Return to the signup page
        }
    }

}
