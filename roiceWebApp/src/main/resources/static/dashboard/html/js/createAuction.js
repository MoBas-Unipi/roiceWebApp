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
