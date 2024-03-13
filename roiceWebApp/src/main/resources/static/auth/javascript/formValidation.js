function validateForm(fields) {
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
        const errorMessage = document.getElementById("error-field");
        errorMessage.textContent = "Please fill out all fields";
        errorMessage.style.color = "red"; // Set error message color to red
        errorMessage.style.display = "block"; // Show error message
        return false; // Prevent form submission
    }

    return true; // Allow form submission if all fields are filled
}
