<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <!-- Tell the browser to be responsive to screen width -->
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="">
    <meta name="author" content="">
    <!-- Favicon icon -->
    <link rel="icon" type="image/png" href="favicon.png">
    <title>Roice Home</title>
    <!-- Bootstrap Core CSS -->
    <link href="/dashboard/assets/plugins/bootstrap/css/bootstrap.min.css" rel="stylesheet">
    <!-- chartist CSS -->
    <link href="/dashboard/assets/plugins/chartist-js/dist/chartist.min.css" rel="stylesheet">
    <link href="/dashboard/assets/plugins/chartist-js/dist/chartist-init.css" rel="stylesheet">
    <link href="/dashboard/assets/plugins/chartist-plugin-tooltip-master/dist/chartist-plugin-tooltip.css" rel="stylesheet">
    <!--This page css - Morris CSS -->
    <link href="/dashboard/assets/plugins/c3-master/c3.min.css" rel="stylesheet">
    <!-- Custom CSS -->
    <link href="/dashboard/html/css/style.css" rel="stylesheet">
    <!-- You can change the theme colors from here -->
    <link href="/dashboard/html/css/colors/blue.css" id="theme" rel="stylesheet">
    <!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
    <!--[if lt IE 9]>
    <script src="https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js"></script>
    <script src="https://oss.maxcdn.com/libs/respond.js/1.4.2/respond.min.js"></script>
    <![endif]-->
    <script src="/dashboard/assets/plugins/jquery/jquery.min.js"></script>
    <!-- Bootstrap tether Core JavaScript -->
    <script src="/dashboard/assets/plugins/bootstrap/js/tether.min.js"></script>
    <script src="/dashboard/assets/plugins/bootstrap/js/bootstrap.min.js"></script>
    <!-- slimscrollbar scrollbar JavaScript -->
    <script src="/dashboard/html/js/jquery.slimscroll.js"></script>
    <!--Wave Effects -->
    <script src="/dashboard/html/js/waves.js"></script>
    <!--Menu sidebar -->
    <script src="/dashboard/html/js/sidebarmenu.js"></script>
    <!--stickey kit -->
    <script src="/dashboard/assets/plugins/sticky-kit-master/dist/sticky-kit.min.js"></script>
    <!--Custom JavaScript -->
    <script src="/dashboard/html/js/custom.min.js"></script>
    <!-- chartist chart -->
    <script src="/dashboard/assets/plugins/chartist-js/dist/chartist.min.js"></script>
    <script src="/dashboard/assets/plugins/chartist-plugin-tooltip-master/dist/chartist-plugin-tooltip.min.js"></script>
    <!--c3 JavaScript -->
    <script src="/dashboard/assets/plugins/d3/d3.min.js"></script>
    <script src="/dashboard/assets/plugins/c3-master/c3.min.js"></script>
    <!-- Chart JS -->
    <script src="/dashboard/html/js/dashboard1.js"></script>

    <style>
        /* Style for the buttons when pressed */
        .btn-primary:active,
        .btn-primary:focus {
            background-color: #00aced !important; /* Change to your desired color */
            border-color: #00aced !important; /* Change to your desired color */
        }
    </style>

</head>
<body class="fix-header fix-sidebar card-no-border">
<div class="preloader">
    <svg class="circular" viewBox="25 25 50 50">
        <circle class="path" cx="50" cy="50" r="20" fill="none" stroke-width="2" stroke-miterlimit="10"/>
    </svg>
</div>
<div id="main-wrapper">
    <div>
        <header class="topbar">
            <nav class="navbar top-navbar navbar-toggleable-sm navbar-light">
                <div class="navbar-header">
                    <a class="navbar-brand" href="/userHome">
                        <b>
                            <img src="/dashboard/assets/images/logo.png" alt="homepage" class="light-logo" />
                        </b>
                        <span>
                            <img src="/dashboard/assets/images/logo-text.png" class="light-logo" alt="homepage" />
                        </span>
                    </a>
                </div>
                <div class="navbar-collapse">
                    <ul class="navbar-nav mr-auto mt-md-0">
                        <li class="nav-item"> <a class="nav-link nav-toggler hidden-md-up text-muted waves-effect waves-dark" href="javascript:void(0)"><i class="mdi mdi-menu"></i></a> </li>
                        <li class="nav-item hidden-sm-down search-box"> <a class="nav-link hidden-sm-down text-muted waves-effect waves-dark" href="javascript:void(0)"><i class="ti-search"></i></a>
                            <form class="app-search">
                                <input type="text" class="form-control" placeholder="Search & enter"> <a class="srh-btn"><i class="ti-close"></i></a> </form>
                        </li>
                    </ul>
                    <ul class="navbar-nav my-lg-0">
                        <li class="nav-item dropdown">
                            <a class="nav-link dropdown-toggle text-muted waves-effect waves-dark" href="/userPage" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                <c:if test="${not empty currentUser}">
                                    ${currentUser.firstName}
                                </c:if>
                                <c:if test="${empty currentUser}">
                                    Guest
                                </c:if>
                            </a>
                        </li>
                    </ul>
                </div>
            </nav>
        </header>
    </div>
    <div>
        <div id="sidebar">
            <aside class="left-sidebar">
                <div class="scroll-sidebar">
                    <nav class="sidebar-nav">
                        <ul id="sidebarnav">
                            <li><a class="waves-effect waves-dark" href="userHome" aria-expanded="false"><i class="mdi mdi-cellphone"></i><span class="hide-menu">Home</span></a></li>
                            <li> <a class="waves-effect waves-dark" href="userPage" aria-expanded="false"><i class="mdi mdi-account-check"></i><span class="hide-menu">Profile</span></a></li>
                            <li><a class="waves-effect waves-dark" href="#" aria-expanded="false"><i class="mdi mdi-alert-circle"></i><span class="hide-menu">Live Auctions</span></a></li>
                        </ul>
                    </nav>
                </div>
                <div class="sidebar-footer">
                    <a href="logout" class="link" data-toggle="tooltip" title="Logout" style="display: flex; align-items: center;">
                        <i class="mdi mdi-power" style="margin-right: 5px;"></i> Logout
                    </a>
                </div>
            </aside>
        </div>
    </div>
    <div class="container-fluid">
        <div class="row page-titles">
            <div class="col-md-5 col-8 align-self-center">
                <h3 class="text-themecolor m-b-0 m-t-0">Home</h3>
                <ol class="breadcrumb">
                    <li class="breadcrumb-item"><a href="/userHome">Home</a></li>
                </ol>
            </div>
        </div>
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
                    <p>User Information:</p>
                    <p>First Name: ${currentUser.firstName}</p>
                    <p>Last Name: ${currentUser.lastName}</p>
                    <p>Country: ${currentUser.country}</p>
                    <p>City: ${currentUser.city}</p>
                    <p>Street Name: ${currentUser.streetName}</p>
                    <p>Street Number: ${currentUser.streetNumber}</p>
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
                                            <img src="${auction.phonePicture}" class="img-rounded" width="150"/>
                                            <h4 class="card-title m-t-10">${auction.phoneName}</h4>
                                            <!-- Additional information -->
                                            <p>Auction Won On: ${auction.endDate}</p>
                                            <p>Price: ${auction.price}</p>
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
                                        <img src="${phone.picture}" class="img-rounded" width="150"/>
                                        <h4 class="card-title m-t-10">${phone.name}</h4>
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
        <c:out value="© 2024 ROICE Web Application" />
    </footer>
</div>

<script>
    function toggleContent(buttonId, contentId) {
        const buttons = document.querySelectorAll('.btn');
        buttons.forEach(button => {
            button.classList.remove('active');
            button.style.backgroundColor = '#26c6da'; // Set default button color
            button.style.borderColor = '#00aced'; // Set default border color
        });
        document.getElementById(buttonId).classList.add('active');
        document.getElementById(buttonId).style.backgroundColor = '#00aced'; // Set active button color
        document.getElementById(buttonId).style.borderColor = '#26c6da'; // Set active border color

        const contentBlocks = document.querySelectorAll('.content-block');
        contentBlocks.forEach(block => {
            block.style.display = 'none';
        });
        document.getElementById(contentId).style.display = 'block';
    }

    // Event listeners for button clicks
    document.getElementById('showUserInfoBtn').addEventListener('click', function() {
        toggleContent('showUserInfoBtn', 'userInfoContent');
    });

    document.getElementById('showWonAuctionsBtn').addEventListener('click', function() {
        toggleContent('showWonAuctionsBtn', 'wonAuctionsContent');
    });

    document.getElementById('showFavoritePhonesBtn').addEventListener('click', function() {
        toggleContent('showFavoritePhonesBtn', 'favoritePhonesContent');
    });

    // Function to set default button color
    function setDefaultButtonColor() {
        document.getElementById('showUserInfoBtn').style.backgroundColor = '#00aced'; // Set default button color
        document.getElementById('showUserInfoBtn').style.borderColor = '#00aced'; // Set default border color
    }

    // Call the setDefaultButtonColor function when the page loads
    window.onload = setDefaultButtonColor;
</script>

</body>
</html>
