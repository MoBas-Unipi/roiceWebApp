<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<link rel="stylesheet" type="text/css" href="/dashboard/html/css/phoneDetails.css">
<head>
    <!-- ============================================================== -->
    <!--  Header -->
    <!-- ============================================================== -->
    <jsp:include page="templates/header.jsp"/>
</head>

<script src="/dashboard/html/js/showPhoneDetails.js"></script>

<body class="fix-header fix-sidebar card-no-border">
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

    <div class="row page-titles" style="margin-left: 250px">
        <div class="col-md-5 col-8 align-self-center">
            <h3 class="text-themecolor m-b-0 m-t-0">Phone Details</h3>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href="/userHome">Home</a></li>
                <li class="breadcrumb-item active">Phone Details</li>
            </ol>
        </div>
    </div>

    <div class="all-container">
        <h1>Phone Details</h1>
        <div class="container">
            <!-- Display phone details -->
            <div class="phone-details-container">
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
            </div>
            <!-- Auction container -->
            <div class="auction-container">
                <h3>Auction</h3>
                <p>Time Remaining: <span id="time-remaining">0 d 0 h 0 m 0 s</span></p>
                <p>Current Bid: $100</p>
                <input type="text" class="bid-input" placeholder="Enter your bid">
                <button class="confirm-bid" onclick="confirmBid()">Confirm Bid</button>
            </div>
        </div>
        <div class="favorites-container">
            <script>
                showPhoneDetails(${isPhoneInFavorites}, "${message}");
            </script>
        </div>
    </div>

    <footer class="footer">
        <div class="text-left copyright-text">
            <c:out value="© 2024 ROICE Web Application" />
        </div>
    </footer>
</div>
</body>
</html>
