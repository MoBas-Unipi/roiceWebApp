let ws;

const server_url = "ws://localhost:8300/";

// variable to store the auction remaining time (received from Erlang Server)
let remainingTime = "";

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

        //----------------------BID HANDLING----------------------//
        // Extract bid value if available (if not assign N/A)
        var bidMatch = received_msg.match(/Bid:(\d+)/);
        var bidValue = bidMatch ? bidMatch[1] : "N/A";
        // If a bid message is arrived from Erlang Server
        if (bidValue !== "N/A") {
            document.querySelector('.current-bid').innerText = bidValue;
            //document.getElementById('current-bid').innerText = bidValue;
        }


        //----------------------TIMER  HANDLING----------------------//
        // Extract the auction remaining time if available (if not assign N/A)
        var timeMatch = received_msg.match(/RemainingTime:"(\d+ d \d+ h \d+ m \d+ s)"/);
        remainingTime = timeMatch ? timeMatch[1] : "N/A";
        // If a timer message is arrived from Erlang Server
        if (timeMatch !== "N/A"){
            // If the auction timer is expired call stopTimer function and set the timer to 0
            if (remainingTime === "0 d 0 h 0 m 0 s") {
                stopTimer();
            }
            document.querySelector('.time-remaining-user').innerText = remainingTime;
            //document.getElementById('time-remaining-user').innerText = remainingTime;
        }


        //----------------------WINNER HANDLING----------------------//
        // Extract the winner
        var winnerMatch = received_msg.match(/Winner:<<"([^"]+)">>/);
        var winner = winnerMatch ? winnerMatch[1] : "N/A";

        // Check if the received message indicates no bidders
        if (received_msg.includes("Auction Terminated! No bidders for this phone!")) {
            document.querySelector('.winner').innerText = "Auction Terminated! No bidders for this phone!";
            // Clear the fields related to time remaining and current bid
            document.querySelector('.time-remaining-user').innerText = "";
            document.querySelector('.current-bid').innerText = "";
        } else {
            if (winner !== "N/A") {
                document.querySelector('.winner').innerText = "Winner: " + winner;
                //document.getElementById("winner").innerText = "Winner: " + winner;
            }
        }


        //----------------------WINNING BID HANDLING----------------------//
        // Extract the winning bid
        var winningBidMatch = received_msg.match(/Winning Bid:(\d+)/);
        var winningBidValue = winningBidMatch ? winningBidMatch[1] : "N/A";
        if (winningBidValue !== "N/A") {
            document.querySelector('.winning-bid').innerText = "With a Bid of: " + winningBidValue + "$";
            //document.getElementById("winning-bid").innerText = "With a Bid of: " + winningBidValue + "$";
        }

    };

    // Event handler for WebSocket connection close
    ws.onclose = function() {
        console.log('Connection closed');
    };

    // Event handler for errors
    ws.onerror = function(error) {
        console.error('WebSocket error:', error);
    };
}



