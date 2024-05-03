package it.unipi.dii.dsmt.roice.dto;

import it.unipi.dii.dsmt.roice.model.AuctionWon;
import it.unipi.dii.dsmt.roice.model.PhonePreview;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
public class UserDTO {
    @NotBlank(message = "First name is required")
    @Size(max = 50, message = "First name cannot be longer than 50 characters")
    private String firstName;

    @NotBlank(message = "Last name is required")
    @Size(max = 50, message = "Last name cannot be longer than 50 characters")
    private String lastName;

    @NotBlank(message = "Email is required")
    @Email(message = "Invalid email format")
    @Size(max = 50, message = "Email cannot be longer than 50 characters")
    private String email;

    @NotBlank(message = "Password is required")
    @Size(min = 8, max = 50, message = "Password must be between 8 and 50 characters long")
    private String password;

    @NotBlank(message = "Country is required")
    @Size(max = 50, message = "Country name cannot be longer than 50 characters")
    private String country;

    @NotBlank(message = "City is required")
    @Size(max = 50, message = "City name cannot be longer than 50 characters")
    private String city;

    @NotBlank(message = "Street name is required")
    @Size(max = 50, message = "Street name cannot be longer than 50 characters")
    private String streetName;

    @Positive(message = "Street number must be positive")
    private int streetNumber;

    private List<PhonePreview> favoritePhones;
    private List<AuctionWon> auctionsWon;

    public UserDTO(String firstName, String lastName, int streetNumber, String streetName, String city, String country,
                   String email, String password, List<PhonePreview> favoritePhones, List<AuctionWon> auctionsWon) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.streetNumber = streetNumber;
        this.streetName = streetName;
        this.city = city;
        this.country = country;
        this.email = email;
        this.password = password;
        this.favoritePhones = favoritePhones;
        this.auctionsWon = auctionsWon;
    }

}
