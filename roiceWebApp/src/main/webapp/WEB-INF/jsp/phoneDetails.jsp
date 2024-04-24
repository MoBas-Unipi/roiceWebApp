<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<link rel="stylesheet" type="text/css" href="/dashboard/html/css/phoneDetails.css">
<script src="/websocket/connection.js"></script>
<script src="/websocket/handleAuction.js"></script>

<head>
    <!-- ============================================================== -->
    <!--  Header -->
    <!-- ============================================================== -->
    <jsp:include page="templates/header.jsp"/>
</head>

<body class="fix-header fix-sidebar card-no-border" onload = "connect()">
<div class="preloader">
    <svg class="circular" viewBox="25 25 50 50">
        <circle class="path" cx="50" cy="50" r="20" fill="none" stroke-width="2" stroke-miterlimit="10"/>
    </svg>
</div>
<div id="main-wrapper">
    <!-- ============================================================== -->
    <!-- Topbar header - style you can find in pages.scss -->
    <!-- ============================================================== -->
    <jsp:include page="templates/topbar.jsp"/>

    <!-- ============================================================== -->
    <!-- Left Sidebar - style you can find in sidebar.scss  -->
    <!-- ============================================================== -->
    <jsp:include page="templates/leftSidebar.jsp"/>


    <div class="container-fluid" style="overflow-y: auto;max-height: 85vh">
        <div class="row page-titles">
            <div class="col-md-5 col-8 align-self-center">
                <h3 class="text-themecolor m-b-0 m-t-0">Phone Details</h3>
                <ol class="breadcrumb">
                    <li class="breadcrumb-item"><a href="/homePage">Home</a></li>
                    <li class="breadcrumb-item active">Phone Details</li>
                </ol>
            </div>
        </div>
        <h1>Phone Details</h1>
        <!-- Display phone details -->
        <div class="phone-details-container" style="margin-left: 200px">
            <div class="phone-image">
                <img src="${phone.picture}" class="phone-image" alt="Phone Image">
            </div>
            <div class="phone-details">
                <h2>${phone.name}</h2>
                <p class="phone-detail-item">Brand: ${phone.brand}</p>
                <p class="phone-detail-item">Body: ${phone.body}</p>
                <p class="phone-detail-item">OS: ${phone.os}</p>
                <p class="phone-detail-item">Storage: ${phone.storage}</p>
                <p class="phone-detail-item">Display Size: ${phone.displaySize}</p>
                <p class="phone-detail-item">Display Resolution: ${phone.displayResolution}</p>
                <p class="phone-detail-item">Camera Pixels: ${phone.cameraPixels}</p>
                <p class="phone-detail-item">Video Pixels: ${phone.videoPixels}</p>
                <p class="phone-detail-item">RAM: ${phone.ram}</p>
                <p class="phone-detail-item">Chipset: ${phone.chipset}</p>
                <p class="phone-detail-item">Battery Size: ${phone.batterySize}</p>
                <p class="phone-detail-item">Battery Type: ${phone.batteryType}</p>
                <p class="phone-detail-item">Release Year: ${phone.releaseYear}</p>
            </div>

            <!-- User case -->
            <c:if test="${not empty phone.auction && userClass == 'user'}">
                <!-- Check if the auction is started-->
                <c:set var="currentTimeMillis" value="<%=System.currentTimeMillis() %>" />
                <c:if test="${currentTimeMillis ge phone.auction.startingDate.time and currentTimeMillis le phone.auction.endDate.time}">
                    <script>
                        // If there is an auction init websocket connection
                        var email = "${currentUser.email}";
                        var phoneName = "${phone.name}";
                        sendJoinAuctionRequest(email, phoneName);
                        sendGetTimerRequest(email,phoneName);
                    </script>
                    <!-- User Auction container -->
                    <div class="content-block" style="margin-left: 100px">
                        <h3>Auction</h3>
                        <p>Start Date: <span class="start-date">${startDate}</span></p> <!-- Placeholder for the start date -->
                        <p>End Date: <span class="end-date">${endDate}</span></p> <!-- Placeholder for end date -->
                        <p>Current Winner: <span class="current-winner"></span></p> <!-- Placeholder for current winner -->
                        <p>Current Bid: $<span class="current-bid">100</span></p> <!-- Placeholder for current bid -->
                        <p>Time Remaining: <span class="time-remaining-user">0 d 0 h 0 m 0 s</span></p><!-- Placeholder for remaining time -->
                        <input type="text" class="bid-input" placeholder="Enter your bid">
                        <button class="confirm-bid-button" onclick="confirmBid(email,phoneName)">Confirm Bid</button>
                        <span id="bidError" style="color: red; display: block;"></span>
                        <span class="winner" style="color: #239800"></span>
                        <span class="winning-bid" style="color: #239800; display: block;"></span>
                    </div>
                </c:if>

                <!-- Check if the auction has not started yet-->
                <c:if test="${not (currentTimeMillis ge phone.auction.startingDate.time and currentTimeMillis le phone.auction.endDate.time)}">
                    <div class="content-block" style="margin-left: 100px">
                        <h3>Auction</h3>
                        <p>Start Date: <span class="start-date">${startDate}</span></p> <!-- Placeholder for current bid -->
                        <p>End Date: <span class="end-date">${endDate}</span></p>
<%--                        <p>Time Remaining: <span class="time-remaining-user">0 d 0 h 0 m 0 s</span></p>--%>
<%--                        <p>Current Bid: $50</p>--%>
                        <span style="color: #ff6c02;">The Auction has not started yet</span>
                    </div>
                </c:if>
            </c:if>

            <!-- Admin case (cannot make bids, just check the auction time and users bids)-->
            <c:if test="${not empty phone.auction && userClass == 'admin'}">
                <!-- Check if the auction is started-->
                <c:set var="currentTimeMillis" value="<%=System.currentTimeMillis() %>" />
                <c:if test="${currentTimeMillis ge phone.auction.startingDate.time and currentTimeMillis le phone.auction.endDate.time}">
                    <script>
                        // If there is an auction init websocket connection
                        var email = "${currentUser.email}";
                        var phoneName = "${phone.name}";
                        sendJoinAuctionRequest(email, phoneName);
                        sendGetTimerRequest(email,phoneName);
                    </script>
                    <div class="content-block" style="margin-left: 100px">
                        <h3>Auction</h3>
                        <p>Start Date: <span class="start-date">${startDate}</span></p> <!-- Placeholder for current bid -->
                        <p>End Date: <span class="end-date">${endDate}</span></p>
                        <p>Current Winner: <span class="current-winner"></span></p> <!-- Placeholder for current winner -->
                        <p>Current Bid: $<span class="current-bid">50</span></p> <!-- Placeholder for current bid -->
                        <p>Time Remaining: <span class="time-remaining-user">0 d 0 h 0 m 0 s</span></p>
                        <span class="winner" style="color: #239800"></span>
                        <span class="winning-bid" style="color: #239800; display: block;"></span>
                    </div>
                </c:if>

                <!-- Check if the auction has not started yet-->
                <c:if test="${not (currentTimeMillis ge phone.auction.startingDate.time and currentTimeMillis le phone.auction.endDate.time)}">
                    <div class="content-block" style="margin-left: 100px">
                        <h3>Auction</h3>
                        <p>Start Date: <span class="start-date">${startDate}</span></p> <!-- Placeholder for current bid -->
                        <p>End Date: <span class="end-date">${endDate}</span></p>
<%--                        <p>Time Remaining: <span class="time-remaining-user">0 d 0 h 0 m 0 s</span></p>--%>
<%--                        <p>Current Bid: $50</p>--%>
                        <span style="color: #ff6c02;">The Auction has not started yet</span>
                    </div>
                </c:if>
            </c:if>

        </div>
        <c:if test="${not empty isPhoneInFavorites}">
            <!-- Handle the case when isPhoneInFavorites is set -->
            <div class="favorites-container">
                <c:if test="${isPhoneInFavorites}">
                    <p id="message-field" style="color: green;">${message}</p>
                    <form method="post" action="/phoneDetails/phone?phoneName=${phone.name}" id="removeFromFavoritesForm">
                        <button type="submit" class="favorites-button">Remove from Favorites</button>
                    </form>
                </c:if>
                <c:if test="${!isPhoneInFavorites}">
                    <p id="message-field" style="color: green;">${message}</p>
                    <form method="post" action="/phoneDetails" id="addToFavoritesForm">
                        <button type="submit" class="favorites-button">Add to Favorites</button>
                    </form>
                </c:if>
            </div>
        </c:if>
        <c:if test="${empty isPhoneInFavorites && !isAuctionPresent}">
            <div class="create-auction-container">
                <p id="message-create-auction" style="color: green;">${message}</p>
                <form method="get" action="/phoneDetails/createAuction" id="createAuction">
                    <button type="submit" class="create-auction-button">Create Auction</button>
                </form>
            </div>
        </c:if>
        <c:if test="${empty isPhoneInFavorites && isAuctionPresent}">
            <div class="create-auction-container">
                <p id="message-remove-auction" style="color: green;">${message}</p>
            </div>
        </c:if>
    </div>

    <footer class="footer">
        <div class="text-left copyright-text">
            <c:out value="© 2024 ROICE Web Application" />
        </div>
    </footer>
</div>
<!-- Stop the timer before leaving the page -->
<script>
    // Beforeunload event listener to stop the timer when the user leave the page
    window.addEventListener('beforeunload', function(event) {
        stopTimer();
    });
</script>
</body>
</html>
