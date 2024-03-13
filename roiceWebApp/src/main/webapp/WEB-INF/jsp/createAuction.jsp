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
            <h3 class="text-themecolor m-b-0 m-t-0">Create Auction</h3>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href="/homePage">Home</a></li>
                <li class="breadcrumb-item"><a href="/phoneDetails?phoneName=${phone.name}">Phone Details</a></li>
                <li class="breadcrumb-item active">Create Auction</li>
            </ol>
        </div>
    </div>

    <div class="all-container">
        <h1>Create Auction</h1>
        <div class="phone-details" style="margin-left: 210px; margin-top: 20px">
            <h2>${phone.name}</h2>
        </div>
        <div class="container">
            <div class="phone-image">
                <img src="${phone.picture}" class="phone-image" alt="Phone Image">
            </div>
            <form method="post" action="/phoneDetails/createAuction?phoneName=${phone.name}" id="createAuctionForm">
                <div class="form-group">
                    <label for="startingDate">Starting Date:</label>
                    <input type="datetime-local" id="startingDate" name="startingDate" required>
                </div>
                <div class="form-group">
                    <label for="endDate">End Date:</label>
                    <input type="datetime-local" id="endDate" name="endDate" required>
                </div>
                <div class="form-group">
                    <label for="minimumPrice">Minimum Price:</label>
                    <input type="number" id="minimumPrice" name="minimumPrice" step="0.01" required>
                </div>
                <input type="hidden" name="phoneName" value="${phone.name}">
                <button type="submit" class="create-auction-button">Create Auction</button>
            </form>
        </div>
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
