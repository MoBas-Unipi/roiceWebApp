package it.unipi.dii.dsmt.roice.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class UserDTO {
    private String firstName;
    private String lastName;
    private String email;
    private String password;
    private String country;
    private String city;
    private String streetName;
    private int streetNumber;

    public UserDTO(String firstName, String lastName,
                   int streetNumber, String streetName, String city, String country, String email, String password) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.streetNumber = streetNumber;
        this.streetName = streetName;
        this.city = city;
        this.country = country;
        this.email = email;
        this.password = password;
    }
}
