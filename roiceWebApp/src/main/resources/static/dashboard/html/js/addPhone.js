// Function to show success message and clear input fields
function showSuccessMessage() {
    // Get the value of 'success' attribute from the HTML element
    const success = document.getElementById('success-value').getAttribute('data-success');

    // Check if the success variable is 'true'
    if (success === 'true') {
        // Display the success message
        const successMessage = document.getElementById('success-message');
        successMessage.style.display = 'block';

        // Clear input fields
        const inputFields = document.querySelectorAll('.register-form input[type="text"]');
        inputFields.forEach(function(input) {
            input.value = '';
        });
    }
}

// Call the function after the document is loaded
document.addEventListener('DOMContentLoaded', function() {
    showSuccessMessage();
});
