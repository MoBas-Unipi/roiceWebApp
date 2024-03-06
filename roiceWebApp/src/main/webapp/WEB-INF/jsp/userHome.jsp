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
                            <a class="nav-link dropdown-toggle text-muted waves-effect waves-dark" href="profile" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                Welcome ${fullName}
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
        <!-- ============================================================== -->
        <!-- Bread crumb and right sidebar toggle -->
        <!-- ============================================================== -->
        <div class="row page-titles">
            <div class="col-md-5 col-8 align-self-center">
                <h3 class="text-themecolor m-b-0 m-t-0">Home</h3>
                <ol class="breadcrumb">
                    <li class="breadcrumb-item"><a href="/userHome">Home</a></li>
                </ol>
            </div>
        </div>
        <!-- ============================================================== -->
        <!-- End Bread crumb and right sidebar toggle -->
        <!-- ============================================================== -->
        <!-- ============================================================== -->
        <!-- Start Page Content -->
        <!-- ============================================================== -->
        <!-- Row -->
        <div class="row">
            <!-- Column -->
            <c:forEach var="phone" items="${phones}">
                <div class="col-lg-4 col-xlg-3 col-md-5">
                    <div class="card">
                        <div class="card-block">
                            <!--
                            <center class="m-t-30"><img src=value"/dashboard/assets/images/phone-sample.jpg" class="img-rounded" width="150"/>
                            -->
                            <center class="m-t-30"><img src=${phone.picture} class="img-rounded" width="150"/>
                                <h4 class="card-title m-t-10"><c:out value="${phone.name}" /></h4>
                                <h4 class="card-subtitle">Brand: <c:out value="${phone.brand}" /></h4>
                            </center>
                        </div>
                    </div>
                </div>
            </c:forEach>
            <!-- Column -->
        </div>

        <!-- Row -->
        <!-- ============================================================== -->
        <!-- End Page Content -->
        <!-- ============================================================== -->
    </div>



    <!-- ============================================================== -->
    <!-- Footer - the style is in style.css   -->
    <!-- ============================================================== -->
    <footer class="footer">
        <!-- Container for footer content -->
        <div class="text-center">
            <!-- Previous page button, visible if current page is greater than 0 -->
            <c:if test="${currentPage > 0}">
                <a href="/userHome?page=${currentPage - 1}&size=${size}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                    <i class="mdi mdi-chevron-left"></i> Previous
                </a>
            </c:if>
            <!-- Next page button, visible if current page is less than total pages minus 1 -->
            <c:if test="${currentPage < totalPages - 1}">
                <a href="/userHome?page=${currentPage + 1}&size=${size}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                    Next <i class="mdi mdi-chevron-right"></i>
                </a>
            </c:if>
            <!-- Text displaying current page number and total pages -->
            <p class="small-text">Page ${currentPage + 1} of ${totalPages}</p>
            <!-- Text displaying copyright information -->
            <div class="text-left copyright-text">
                <c:out value="© 2024 ROICE Web Application" />
            </div>
        </div>
    </footer>








</div>
<!-- ============================================================== -->
<!-- End Wrapper -->
<!-- ============================================================== -->
</body>

</html>

