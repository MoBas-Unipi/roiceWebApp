package it.unipi.dii.dsmt.roice.model;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Setter
@Getter
@NoArgsConstructor
@Document(collection = "users")
public abstract class GenericUser {

	@Id
	protected String id;
	protected String email;
	protected String salt;
	protected String hashedPassword;
	protected String _class;

	protected GenericUser(String email, String salt, String hashedPassword, String _class) {
		this.email = email;
		this.salt = salt;
		this.hashedPassword = hashedPassword;
		this._class = _class;
	}

}

