package it.unipi.dii.dsmt.roice.model;

import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "users")
@TypeAlias("admin")
public class Admin extends GenericUser{

	public Admin() {
	}

	public Admin(String email, String salt, String hashedPassword, String _class) {
		super(email, salt, hashedPassword, _class);
	}

	public void addAuctionWon(AuctionWon auctionWon) {
	}

	@Override
	public String toString() {
		return "Admin{" +
				"id='" + id + '\'' +
				", email='" + email + '\'' +
				", salt='" + salt + '\'' +
				", hashedPassword='" + hashedPassword + '\'' +
				'}';
	}
}
