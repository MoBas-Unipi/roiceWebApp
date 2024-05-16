package it.unipi.dii.dsmt.roice.controller;

import it.unipi.dii.dsmt.roice.dto.UserDTO;
import it.unipi.dii.dsmt.roice.dto.mapper.UserMapper;
import it.unipi.dii.dsmt.roice.model.GenericUser;
import it.unipi.dii.dsmt.roice.model.User;
import it.unipi.dii.dsmt.roice.service.UserService;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

import java.util.Optional;

@Controller
public class UserPageController {

    @Autowired
    private UserService userService;

    @GetMapping("/userPage")
    public String userPage(HttpSession session) {
        UserDTO currentUser = (UserDTO) session.getAttribute("currentUser");
        if (currentUser == null) {
            return "redirect:/login";
        }
        Optional<GenericUser> genericUser = userService.findByEmail(currentUser.getEmail());
        if (genericUser.isPresent()) {
            User user = (User) genericUser.get();
            UserDTO userDTO = UserMapper.toUserDTO(user);
            session.setAttribute("currentUser", userDTO);
        }
        return "userPage";
    }

}
