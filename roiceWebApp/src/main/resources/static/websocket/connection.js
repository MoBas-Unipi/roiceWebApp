let ws;

const server_url = "ws://localhost:8300/";

// Function to establish WebSocket connection
function connect() {

    if (!("WebSocket" in window)) {
        alert("This browser does not support WebSockets");
        return;
    }

    // Create WebSocket object
    ws = new WebSocket(server_url);
    console.log("Connected to: ", server_url);

    // Event handler for WebSocket connection open
    ws.onopen = function() {
        console.log("WebSocket connection established successfully.");
    };

    // Event handler for WebSocket messages
    ws.onmessage = function(event) {
        var received_msg = event.data;
        console.log("Received: " + received_msg);
        document.getElementById('current-bid').innerText = received_msg;
    };

    ws.onclose = function()
    {
        console.log('Connection closed');
    };

}

// Function to send a json message
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
        send(bidAmount);
    } else {
        console.error("Invalid bid input.");
    }
}



