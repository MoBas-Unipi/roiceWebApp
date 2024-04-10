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

    // Event handler for WebSocket connection close
    ws.onclose = function() {
        console.log('Connection closed');
    };
}

// Function to send join auction message
function sendJoinAuctionRequest(email) {
    // Delay startTimer() execution by 3 seconds
    setTimeout(function() {
        // Retrieve email of the current user from JSP
        console.log("User email",email);
        // Inform the server that a bidder is joining the auction
        const message = {
            action: "join_auction",
            email: email
        };
        ws.send(JSON.stringify(message));
    }, 500); // Delay of 0.5 seconds
}

