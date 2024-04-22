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
        error = true;
    } else {
        document.getElementById("startingDateError").innerText = "";
    }
    if (endDate === "") {
        error = true;
    } else {
        document.getElementById("endDateError").innerText = "";
    }
    if (minimumPrice === "") {
        error = true;
    } else {
        document.getElementById("minimumPriceError").innerText = "";
    }

    if (error) {
        return false;
    }

    if (new Date(endDate) <= new Date(startingDate)) {
        return false;
    }

    if (new Date(startingDate) <= now) {
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
function confirmBid(email,phone_name) {
    // Check if the time remaining is not zero
    let timeRemaining = document.querySelector('.time-remaining-user').innerText;
    //let timeRemaining = document.getElementById("time-remaining-user").innerText;
    if (timeRemaining === "0 d 0 h 0 m 0 s") {
        // Time is zero, prevent bid confirmation
        document.getElementById("bidError").innerText = "Not Possible to make bids. The Auction has not started yet or it's already terminated!";
        return;
    }

    // Get the input value from "bid-input" field
    let bidInput = document.querySelector('.bid-input').value;

    // Convert the bid input to an integer
    let bidAmount = parseFloat(bidInput);

    // Check if the bid input is not empty and is a valid integer
    if (!isNaN(bidAmount)) {
        // Get current date and time as a string in ISO format
        let currentDate = new Date().toISOString();
        console.log("Bid date: ",currentDate);

        // Get the current Bid value
        var currentBidValue = document.querySelector('.current-bid').innerText;
        //var currentBidValue = document.getElementById("current-bid").innerText;

        if (bidAmount <= currentBidValue) {
            document.getElementById("bidError").innerText = "Your bid is lower than the current one!";
        }
        else {
            document.getElementById("bidError").innerText = "";
            // Send the bid to the web socket
            let message = {
                action : "send",
                phone_name : phone_name,
                email : email,
                date: currentDate,
                value: bidAmount
            };
            send(message);
        }
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

    var minimumPrice = parseInt(document.getElementById("minimumPrice").value);
    // Construct JSON object
    var auctionData = {
        action: "new_auction", // Set action to "new_auction"
        startSeconds: startSeconds, // Include StartDate field in seconds
        endSeconds: endSeconds, // Include EndDate field in seconds
        minimumPrice: minimumPrice,
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

// Function to send join auction message
function sendJoinAuctionRequest(email, phoneName) {
    setTimeout(function () {
        // Inform the server that a bidder is joining the auction
        const message = {
            action: "join_auction",
            email: email,
            phoneName: phoneName
        };
        ws.send(JSON.stringify(message));
    }, 1000);

    console.log("sendJoinAuctionRequest sent");
}


// Global declaration of getTimerId and updateTimerId
let getTimerId;
let updateTimerId;

// Function to send a get auction timer request to Erlang Server and start the local timer (by using updateTimer function)
function sendGetTimerRequest(email, phoneName) {
    // Function to send the get auction timer message
    function sendMessage() {
        const message = {
            action: "timer",
            email: email,
            phone_name: phoneName
        };
        ws.send(JSON.stringify(message));
    }

    // Send the get auction timer message after 1 seconds from the first call
    setTimeout(function () {
        sendMessage();
        // Set interval to send the message every 10 seconds
        getTimerId = setInterval(sendMessage, 10000);
    }, 1000);

    // Start the update timer function just after (5 ms) the join message
    setTimeout( function () {
        updateTimer();
    },5)
}


// Function to update the auction timer in the web page (using both local timer and updates from Erlang server)
function updateTimer() {
    var elapsedTime = 0; //elapsed time for local visualization
    var newRemainingTime = ""; //local variable to store the last erlang update

    // Set interval to update the timer every 1 second
    updateTimerId = setInterval( function () {
        // if remainingTimer is empty (no join message sent)
        if (remainingTime !== "") {
            // Extract the values of remaining days, hours, minutes and seconds from remainingTime variable
            var regex = /(\d+) d (\d+) h (\d+) m (\d+) s/;
            var matches = remainingTime.match(regex);
            var days = parseInt(matches[1]);
            var hours = parseInt(matches[2]);
            var minutes = parseInt(matches[3]);
            var seconds = parseInt(matches[4]);

            // if the received timer update from erlang server
            if (newRemainingTime !== remainingTime) {
                // If the received timer is already expired, set the timer to 0 and stop the timer
                if (days === 0 && hours === 0 && minutes === 0 && seconds === 0) {
                    document.querySelector('.time-remaining-user').innerText = days + ' d ' + hours + ' h ' + minutes + ' m ' + seconds + ' s';
                    //document.getElementById('time-remaining-user').innerText = days + ' d ' + hours + ' h ' + minutes + ' m ' + seconds + ' s';
                    stopTimer();
                }

                // If the received timer is not expired, set the timer to the current value
                document.querySelector('.time-remaining-user').innerText = days + ' d ' + hours + ' h ' + minutes + ' m ' + seconds + ' s';
                //document.getElementById('time-remaining-user').innerText = days + ' d ' + hours + ' h ' + minutes + ' m ' + seconds + ' s';
                elapsedTime = 0; //reset the elapsed time for local visualization
                // Update the local variable to store the last erlang update
                newRemainingTime = days + ' d ' + hours + ' h ' + minutes + ' m ' + seconds + ' s';
            }

            // If no timer update from erlang arrived, manage a local timer
            else {
                // Update the elapsed time for local visualization
                elapsedTime += 1000;
                // Compute the remaining time in milliseconds
                var remainingMilliseconds = (days * 24 * 60 * 60 + hours * 60 * 60 + minutes * 60 + seconds) * 1000 - elapsedTime;

                // Convert the remaining time in days, hours, minutes and seconds
                var remainingDays = Math.floor(remainingMilliseconds / (1000 * 60 * 60 * 24));
                var remainingHours = Math.floor((remainingMilliseconds % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
                var remainingMinutes = Math.floor((remainingMilliseconds % (1000 * 60 * 60)) / (1000 * 60));
                var remainingSeconds = Math.floor((remainingMilliseconds % (1000 * 60)) / 1000);

                // If the local timer is expired, set the timer to 0 and stop the timer and conclude the function
                if (remainingMilliseconds <= 0) {
                    elapsedTime = 0;
                    document.querySelector('.time-remaining-user').innerText = remainingDays + ' d ' + remainingHours + ' h ' + remainingMinutes + ' m ' + remainingSeconds + ' s';
                    //document.getElementById('time-remaining-user').innerText = remainingDays + ' d ' + remainingHours + ' h ' + remainingMinutes + ' m ' + remainingSeconds + ' s';
                    stopTimer();
                    return;
                }

                // If the local timer is not expired, update the remaining time value
                document.querySelector('.time-remaining-user').innerText = remainingDays + ' d ' + remainingHours + ' h ' + remainingMinutes + ' m ' + remainingSeconds + ' s';
                //document.getElementById('time-remaining-user').innerText = remainingDays + ' d ' + remainingHours + ' h ' + remainingMinutes + ' m ' + remainingSeconds + ' s';
            }
        }
    }, 1000);
}


// Function to stop the timer
function stopTimer() {
    clearInterval(getTimerId);
    clearInterval(updateTimerId);
    console.log("Timer stopped");
}