function showPhoneDetails(isPhoneInFavorites, message) {
    var favoritesContainer = document.querySelector('.favorites-container');
    if (isPhoneInFavorites) {
        favoritesContainer.innerHTML = '<p id="message-field" style="color: green;">' + message + '</p>';
    } else {
        favoritesContainer.innerHTML = '<form method="post" action="/phoneDetails" id="addToFavoritesForm">' +
            '<button type="submit" class="add-to-favorites">Add to Favorites</button>' +
            '</form>' +
            '<p id="message-field" style="color: red;">' + message + '</p>';
    }
}
