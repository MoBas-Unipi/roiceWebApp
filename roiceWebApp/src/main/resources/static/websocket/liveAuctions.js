var wsLiveAuctions; // WebSocket object

// Function to create WebSocket connection
function createWebSocketConnection() {
    // Create WebSocket connection
    wsLiveAuctions = new WebSocket("ws://10.2.1.41:8300/");

    // Event handler for WebSocket open
    wsLiveAuctions.onopen = function(event) {
        console.log('WebSocket connection opened.');

        // Send the live auctions message
        sendLiveAuctionsMessage();
    };
    wsLiveAuctions.onmessage = function (event) {
        console.log('Message received from server:', event.data);
        location.reload(); // Reload the page
    };
    // Event handler for WebSocket close
    wsLiveAuctions.onclose = function(event) {
        console.log('WebSocket connection closed.');
    };

    // Event handler for WebSocket errors
    wsLiveAuctions.onerror = function(error) {
        console.error('WebSocket encountered error:', error);
    };
}

// Function to send live auctions message
function sendLiveAuctionsMessage() {
    // Construct the message object
    var message = {
        action: 'live_auctions'
    };

    // Send the message if the WebSocket connection is open
    if (wsLiveAuctions.readyState === WebSocket.OPEN) {
        wsLiveAuctions.send(JSON.stringify(message));
        console.log('Live Auctions message sent:', message);
    } else {
        console.error('WebSocket connection is not open.');
    }
}
