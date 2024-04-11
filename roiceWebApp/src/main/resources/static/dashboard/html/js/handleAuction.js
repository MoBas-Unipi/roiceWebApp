function setCurrentDatetime() {
    var now = new Date();
    var year = now.getFullYear();
    var month = now.getMonth() + 1 < 10 ? "0" + (now.getMonth() + 1) : now.getMonth() + 1;
    var day = now.getDate() < 10 ? "0" + now.getDate() : now.getDate();
    var hours = now.getHours() < 10 ? "0" + now.getHours() : now.getHours();
    var minutes = now.getMinutes() < 10 ? "0" + now.getMinutes() : now.getMinutes();
    var currentDatetime = year + "-" + month + "-" + day + "T" + hours + ":" + minutes;
    document.getElementById("startingDate").min = currentDatetime;
    document.getElementById("endDate").min = currentDatetime;
}

function validateForm() {
    var startingDate = document.getElementById("startingDate").value;
    var endDate = document.getElementById("endDate").value;
    var minimumPrice = document.getElementById("minimumPrice").value;

    var now = new Date();

    var error = false;
    if (startingDate === "") {
        document.getElementById("startingDateError").innerText = "Starting Date is required";
        error = true;
    } else {
        document.getElementById("startingDateError").innerText = "";
    }
    if (endDate === "") {
        document.getElementById("endDateError").innerText = "End Date is required";
        error = true;
    } else {
        document.getElementById("endDateError").innerText = "";
    }
    if (minimumPrice === "") {
        document.getElementById("minimumPriceError").innerText = "Minimum Price is required";
        error = true;
    } else {
        document.getElementById("minimumPriceError").innerText = "";
    }

    if (error) {
        return false;
    }

    if (new Date(endDate) <= new Date(startingDate)) {
        document.getElementById("dateError").innerText = "End Date must be after Starting Date!";
        return false;
    }

    if (new Date(startingDate) <= now) {
        document.getElementById("dateError").innerText = "Starting Date must be in the future!";
        return false;
    }

    document.getElementById("dateError").innerText = ""; // Clear any previous date error
    return true;
}

// flatpickr setup
function setupFlatpickr() {
    setCurrentDatetime(); // Call the function again to ensure it's set before Flatpickr

    flatpickr("#startingDate", {
        enableTime: true,
        dateFormat: "Y-m-d\\  H:i",
        time_24hr: true,
        minDate: "today",
        minTime: "now"
    });

    flatpickr("#endDate", {
        enableTime: true,
        dateFormat: "Y-m-d\\  H:i",
        time_24hr: true,
        minDate: "today",
        minTime: "now"
    });
}

//TODO send the json message as tuple and extract in the erlang handle function the various atom of a tuple
function send(message) {
    if (ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify(message));
        console.log("Message sent :", message);
    } else {
        console.error("WebSocket connection is not open.");
    }
}


// Function to send a message containing the bid
function confirmBid() {
    // Get the input value from "bid-input" field
    let bidInput = document.querySelector('.bid-input').value;

    // Convert the bid input to an integer
    let bidAmount = parseFloat(bidInput);

    // Check if the bid input is not empty and is a valid integer
    if (!isNaN(bidAmount)) {
        // Send the bid to the web socket
        let message = {
            action : "send",
            value: bidAmount
        };
        send(message);
    } else {
        console.error("Invalid bid input.");
    }
}

function createErlangAuction(phoneName) {
    // Get form inputs
    var startingDate = document.getElementById("startingDate").value;
    var endDate = document.getElementById("endDate").value;

    // Convert dates to seconds
    var startSeconds = Date.parse(startingDate) / 1000;
    var endSeconds = Date.parse(endDate) / 1000;

    // Construct JSON object
    var auctionData = {
        action: "new_auction", // Set action to "new_auction"
        startSeconds: startSeconds, // Include StartDate field in seconds
        endSeconds: endSeconds, // Include EndDate field in seconds
        phoneName: phoneName // Include phoneName
    };

    // Convert JSON to string
    var jsonMessage = JSON.stringify(auctionData);

    // WebSocket endpoint URL
    var webSocketUrl = 'ws://localhost:8300';

    // Create WebSocket connection
    var socket = new WebSocket(webSocketUrl);

    // Event handler for successful connection
    socket.onopen = function(event) {
        console.log('WebSocket connection opened');

        // Send JSON message
        socket.send(jsonMessage);
    };

    // Event handler for receiving messages
    socket.onmessage = function(event) {
        console.log('Message received from server:', event.data);
        // Handle server response if needed
    };

    // Event handler for connection close
    socket.onclose = function(event) {
        console.log('WebSocket connection closed');
    };

    // Event handler for errors
    socket.onerror = function(error) {
        console.error('WebSocket error:', error);
    };
}

function validateAndCreateAuction(phoneName) {
    // Call the validateForm() function
    var isValid = validateForm();
    // If the form is valid, proceed to create auction
    if (isValid) {
        createErlangAuction(phoneName);
    }
}

