function validateForm() {
    const fields = ["firstName", "lastName", "email", "password", "country", "city", "streetName", "streetNumber"];
    let isEmpty = false;

    // Check if any of the required fields are empty
    fields.forEach(field => {
        const value = document.getElementById(field).value.trim();
        if (value === "") {
            isEmpty = true;
            return false; // Exit forEach loop early if any field is empty
        }
    });

    // If any field is empty, display the error message within the page
    if (isEmpty) {
        document.getElementById("error-field").textContent = "Please fill out all fields";
        document.getElementById("error-field").style.display = "block"; // Show error message
        return false; // Prevent form submission
    }

    return true; // Allow form submission if all fields are filled
}
