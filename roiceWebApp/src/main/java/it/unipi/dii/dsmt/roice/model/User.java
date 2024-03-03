package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@Document(collection = "users")
@TypeAlias("user")
public class User extends GenericUser{

	private String firstName;
	private String lastName;
	private int streetNumber;
	private String streetName;
	private String city;
	private String country;
	private List<Phone> favoritePhones;
	private List<Notification> notifications;
	private List<AuctionWon> auctionsWon;

	public User(String email, String salt, String hashedPassword, String _class, String firstName,
				String lastName, int streetNumber, String streetName, String city, String country) {
		super(email, salt, hashedPassword, _class);
		this.firstName = firstName;
		this.lastName = lastName;
		this.streetNumber = streetNumber;
		this.streetName = streetName;
		this.city = city;
		this.country = country;
	}

	@Override
	public String toString() {
		return "User{" +
				"id='" + id + '\'' +
				", salt='" + salt + '\'' +
				", sha='" + hashedPassword + '\'' +
				", firstName='" + firstName + '\'' +
				", lastName='" + lastName + '\'' +
				", streetNumber='" + streetNumber + '\'' +
				", streetName='" + streetName + '\'' +
				", city='" + city + '\'' +
				", country='" + country + '\'' +
				", email='" + email + '\'' +
				'}';
	}
}
