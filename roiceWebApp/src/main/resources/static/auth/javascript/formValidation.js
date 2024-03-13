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

        const successMessage = document.getElementById("success-message");
        successMessage.style.display = "none";

        return false; // Prevent form submission
    }

    return true; // Allow form submission if all fields are filled
}

function displayMessage(messageId, messageContent) {
    const messageElement = document.getElementById(messageId);
    if (messageElement) {
        messageElement.textContent = messageContent;
        messageElement.style.display = "block";
    }
}
