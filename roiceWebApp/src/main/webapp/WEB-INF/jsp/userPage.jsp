<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">

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


    <div class="container-fluid" style="overflow-y: auto; max-height: 80vh;">
        <!-- ============================================================== -->
        <!-- Bread crumb and right sidebar toggle -->
        <!-- ============================================================== -->
        <div class="row page-titles">
            <div class="col-md-5 col-8 align-self-center">
                <h3 class="text-themecolor m-b-0 m-t-0">Profile</h3>
                <ol class="breadcrumb">
                    <li class="breadcrumb-item"><a href="/homePage">Home</a></li>
                    <li class="breadcrumb-item active">Profile</li>
                </ol>
            </div>
        </div>
        <!-- End Bread crumb and right sidebar toggle -->
        <div class="row">
            <div class="col-md-4">
                <button id="showUserInfoBtn" class="btn btn-primary btn-block active" style="background-color: #26c6da; border-color: #00aced">Personal Info</button>
            </div>
            <div class="col-md-4">
                <button id = "showWonAuctionsBtn" class="btn btn-primary btn-block" style="background-color: #26c6da; border-color: #00aced">Won Auctions</button>
            </div>
            <div class="col-md-4">
                <button id="showFavoritePhonesBtn" class="btn btn-primary btn-block" style="background-color: #26c6da; border-color: #00aced">Favorite Phones</button>
            </div>
        </div>
        <div id="contentArea" class="mt-4">
            <!-- User Information Content -->
            <div id="userInfoContent" class="content-block">
                <c:if test="${not empty currentUser}">
                    <p><strong>First Name: </strong>${currentUser.firstName}</p>
                    <p><strong>Last Name: </strong>${currentUser.lastName}</p>
                    <p><strong>Country: </strong>${currentUser.country}</p>
                    <p><strong>City: </strong>${currentUser.city}</p>
                    <p><strong>Street Name: </strong>${currentUser.streetName}</p>
                    <p><strong>Street Number: </strong>${currentUser.streetNumber}</p>
                </c:if>
            </div>
            <!-- Won Auctions Content -->
            <div id="wonAuctionsContent" class="content-block" style="display: none;">
                <c:if test="${empty currentUser.auctionsWon}">
                    <div class="row justify-content-center" id="wonAuctionsContainer">
                        <div class="col-lg-6 col-md-8 col-sm-10">
                            <div class="card">
                                <div class="card-block text-center">
                                    <!-- Display message in the center -->
                                    <p>You have not won any auctions</p>
                                </div>
                            </div>
                        </div>
                    </div>
                </c:if>
                <c:if test="${not empty currentUser.auctionsWon}">
                    <div class="row" id="wonAuctionsContainer">
                        <!-- Loop through won auctions and render them -->
                        <c:forEach var="auction" items="${currentUser.auctionsWon}">
                            <div class="col-lg-4 col-xlg-3 col-md-5">
                                <div class="card">
                                    <div class="card-block">
                                        <div class="m-t-30" style="text-align: center;">
                                            <!-- Display phone image and name -->
                                            <a href="/phoneDetails?phoneName=<c:out value="${auction.phoneName}" />">
                                                <img src="${auction.phonePicture}" class="img-rounded" width="150"/>
                                            </a>
                                            <h4 class="card-title m-t-10">
                                                <a href="/phoneDetails?phoneName=<c:out value="${auction.phoneName}" />">
                                                    <c:out value="${auction.phoneName}" />
                                                </a>
                                            </h4>


                                            <!-- Additional information -->
                                            <p><strong>Auction Won On:</strong>
                                                <fmt:formatDate value="${auction.endDate}" pattern="dd/MM/yyyy HH:mm" />
                                            </p>
                                            <p><strong>Price:</strong> ${auction.price}€</p>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </c:forEach>
                    </div>
                </c:if>
            </div>
            <!-- Favorite Phones Content -->
            <div id="favoritePhonesContent" class="content-block" style="display: none;">
                <c:if test="${empty currentUser.favoritePhones}">
                    <div class="row justify-content-center" id="favoritePhonesContainer">
                        <div class="col-lg-6 col-md-8 col-sm-10">
                            <div class="card">
                                <div class="card-block text-center">
                                    <!-- Display message in the center -->
                                    <p>You have no phones in the favorites list</p>
                                </div>
                            </div>
                        </div>
                    </div>
                </c:if>
                <div class="row" id="favoritePhonesContainer">
                    <!-- Loop through favorite phones and render them -->
                    <c:forEach var="phone" items="${currentUser.favoritePhones}">
                        <div class="col-lg-4 col-xlg-3 col-md-5">
                            <div class="card">
                                <div class="card-block">
                                    <div class="m-t-30" style="text-align: center;">
                                        <!-- Display phone image and name -->
                                        <a href="/phoneDetails?phoneName=<c:out value="${phone.name}" />">
                                            <img src="${phone.picture}" class="img-rounded" width="150"/>
                                        </a>
                                        <h4 class="card-title m-t-10">
                                            <a href="/phoneDetails?phoneName=<c:out value="${phone.name}" />">
                                                <c:out value="${phone.name}" />
                                            </a>
                                        </h4>

                                    </div>
                                </div>
                            </div>
                        </div>
                    </c:forEach>
                </div>
            </div>
        </div>
    </div>

    <footer class="footer">
        <div class="text-left copyright-text">
            <c:out value="© 2024 ROICE Web Application" />
        </div>
    </footer>

</div>

<!-- Manage buttons JS -->
<script src="/dashboard/html/js/userPage.js"></script>
</body>
</html>
