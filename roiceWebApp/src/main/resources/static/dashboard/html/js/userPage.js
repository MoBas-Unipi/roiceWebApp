function toggleContent(buttonId, contentId) {
    const buttons = document.querySelectorAll('.btn');
    buttons.forEach(button => {
        button.classList.remove('active');
        button.style.backgroundColor = '#26c6da'; // Set default button color
        button.style.borderColor = '#00aced'; // Set default border color
    });
    document.getElementById(buttonId).classList.add('active');
    document.getElementById(buttonId).style.backgroundColor = '#00aced'; // Set active button color
    document.getElementById(buttonId).style.borderColor = '#26c6da'; // Set active border color

    const contentBlocks = document.querySelectorAll('.content-block');
    contentBlocks.forEach(block => {
        block.style.display = 'none';
    });
    document.getElementById(contentId).style.display = 'block';
}

// Event listeners for button clicks
document.getElementById('showUserInfoBtn').addEventListener('click', function() {
    toggleContent('showUserInfoBtn', 'userInfoContent');
});

document.getElementById('showWonAuctionsBtn').addEventListener('click', function() {
    toggleContent('showWonAuctionsBtn', 'wonAuctionsContent');
});

document.getElementById('showFavoritePhonesBtn').addEventListener('click', function() {
    toggleContent('showFavoritePhonesBtn', 'favoritePhonesContent');
});

// Function to set default button color
function setDefaultButtonColor() {
    document.getElementById('showUserInfoBtn').style.backgroundColor = '#00aced'; // Set default button color
    document.getElementById('showUserInfoBtn').style.borderColor = '#26c6da'; // Set default border color
}

// Call the setDefaultButtonColor function when the page loads
window.onload = setDefaultButtonColor;