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



