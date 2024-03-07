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
</head>



<body class="fix-header fix-sidebar card-no-border">
<!-- ============================================================== -->
<!-- Preloader - style you can find in spinners.css -->
<!-- ============================================================== -->
<div class="preloader">
    <svg class="circular" viewBox="25 25 50 50">
        <circle class="path" cx="50" cy="50" r="20" fill="none" stroke-width="2" stroke-miterlimit="10"/>
    </svg>
</div>
<!-- ============================================================== -->
<!-- Main wrapper - style you can find in pages.scss -->
<!-- ============================================================== -->
<div id="main-wrapper">

    <!-- ============================================================== -->
    <!-- Topbar header - style you can find in pages.scss -->
    <!-- ============================================================== -->
    <div>
        <header class="topbar">
            <nav class="navbar top-navbar navbar-toggleable-sm navbar-light">
                <!-- ============================================================== -->
                <!-- Logo -->
                <!-- ============================================================== -->
                <div class="navbar-header">
                    <a class="navbar-brand" href="/userHome">
                        <!-- Logo icon -->
                        <b>
                            <!--You can put here icon as well // <i class="wi wi-sunset"></i> //-->
                            <!-- Light Logo icon -->
                            <img src="/dashboard/assets/images/logo.png" alt="homepage" class="light-logo" />
                        </b>
                        <!--End Logo icon -->
                        <!-- Logo text -->
                        <span>
                            <!-- Light Logo text -->
                            <img src="/dashboard/assets/images/logo-text.png" class="light-logo" alt="homepage" />
                        </span>
                    </a>
                </div>
                <!-- ============================================================== -->
                <!-- End Logo -->
                <!-- ============================================================== -->
                <div class="navbar-collapse">
                    <!-- ============================================================== -->
                    <!-- toggle and nav items -->
                    <!-- ============================================================== -->
                    <ul class="navbar-nav mr-auto mt-md-0">
                        <!-- This is  -->
                        <li class="nav-item"> <a class="nav-link nav-toggler hidden-md-up text-muted waves-effect waves-dark" href="javascript:void(0)"><i class="mdi mdi-menu"></i></a> </li>
                        <!-- ============================================================== -->
                        <!-- Search -->
                        <!-- ============================================================== -->
                        <li class="nav-item hidden-sm-down search-box"> <a class="nav-link hidden-sm-down text-muted waves-effect waves-dark" href="javascript:void(0)"><i class="ti-search"></i></a>
                            <form class="app-search">
                                <input type="text" class="form-control" placeholder="Search & enter"> <a class="srh-btn"><i class="ti-close"></i></a> </form>
                        </li>
                    </ul>
                    <!-- ============================================================== -->
                    <!-- User profile and search -->
                    <!-- ============================================================== -->
                    <ul class="navbar-nav my-lg-0">
                        <!-- ============================================================== -->
                        <!-- Profile -->
                        <!-- ============================================================== -->
                        <li class="nav-item dropdown">
                            <a class="nav-link dropdown-toggle text-muted waves-effect waves-dark" href="/userPage" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                <!-- Check if user is logged in -->
                                <c:if test="${not empty currentUser}">
                                    <!-- Display user's name instead of "Guest" -->
                                    ${currentUser.firstName}
                                </c:if>
                                <!-- If user is not logged in, display "Guest" -->
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
            <!-- ============================================================== -->
            <!-- Left Sidebar - style you can find in sidebar.scss  -->
            <!-- ============================================================== -->
            <aside class="left-sidebar">
                <!-- Sidebar scroll-->
                <div class="scroll-sidebar">
                    <!-- Sidebar navigation-->
                    <nav class="sidebar-nav">
                        <ul id="sidebarnav">
                            <li><a class="waves-effect waves-dark" href="userHome" aria-expanded="false"><i class="mdi mdi-cellphone"></i><span class="hide-menu">Home</span></a>
                            </li>
                            <li> <a class="waves-effect waves-dark" href="#" aria-expanded="false"><i class="mdi mdi-account-check"></i><span class="hide-menu">Profile</span></a>
                            </li>
                            <li><a class="waves-effect waves-dark" href="#" aria-expanded="false"><i class="mdi mdi-alert-circle"></i><span class="hide-menu">Live Auctions</span></a>
                            </li>
                        </ul>
                    </nav>
                    <!-- End Sidebar navigation -->
                </div>
                <!-- End Sidebar scroll-->
                <!-- Bottom points-->
                <div class="sidebar-footer">
                    <!-- item-->
                    <a href="logout" class="link" data-toggle="tooltip" title="Logout" style="display: flex; align-items: center;">
                        <i class="mdi mdi-power" style="margin-right: 5px;"></i> Logout
                    </a>
                </div>


                <!-- End Bottom points-->
            </aside>
        </div>
    </div>



    <div class="container-fluid">
        <!-- Add your breadcrumb here -->

        <div class="row page-titles">
            <div class="col-md-5 col-8 align-self-center">
                <h3 class="text-themecolor m-b-0 m-t-0">Home</h3>
                <ol class="breadcrumb">
                    <li class="breadcrumb-item"><a href="/userHome">Home</a></li>
                </ol>
            </div>
        </div>

        <!-- Buttons to display different sections -->
        <div class="row">
            <div class="col-md-4">
                <button class="btn btn-primary btn-block" onclick="showUserInfoFunction()">Show User Info</button>
            </div>
            <div class="col-md-4">
                <button class="btn btn-primary btn-block" onclick="showWonAuctions()">Won Auctions</button>
            </div>
            <div class="col-md-4">
                <button class="btn btn-primary btn-block" onclick="showFavoritePhones()">Favorite Phones</button>
            </div>
        </div>

        <!-- Content area for user information -->
        <div id="contentArea" class="mt-4">
            <!-- Display user information here when the button is pressed -->
            <c:if test="${not empty currentUser}">
                <!-- Display all user information except the lists -->
                <p>User Information:</p>
                <p>First Name: ${currentUser.firstName}</p>
                <p>Last Name: ${currentUser.lastName}</p>
                <p>Country: ${currentUser.country}</p>
                <p>City: ${currentUser.city}</p>
                <p>Street Name: ${currentUser.streetName}</p>
                <p>Street Number: ${currentUser.streetNumber}</p>
                <!-- Add more user information fields as needed -->
            </c:if>
        </div>
    </div>

    <footer class="footer">
        <c:out value="© 2024 ROICE Web Application" />
    </footer>

</div>
<!-- ============================================================== -->
<!-- End Wrapper -->
<!-- ============================================================== -->
<script>
    function showUserInfoFunction() {
        const contentArea = document.getElementById('contentArea');
        contentArea.style.display = contentArea.style.display === 'none' ? 'block' : 'none';
    }
</script>

</body>

</html>

