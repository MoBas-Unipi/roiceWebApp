function validateLogin() {
    const email = document.getElementById("email").value.trim();
    const password = document.getElementById("password").value.trim();

    // Check if either email or password is empty
    if (email === "" || password === "") {
        document.getElementById("error-field").textContent = "Insert both email and password";
        document.getElementById("error-field").style.display = "block"; // Show error message
        return false; // Prevent form submission
    }

    return true; // Allow form submission if both fields are filled
}

document.getElementById("login-form").addEventListener("submit", function(event) {
    if (!validateLogin()) {
        event.preventDefault(); // Prevent form submission if validation fails
    }
});