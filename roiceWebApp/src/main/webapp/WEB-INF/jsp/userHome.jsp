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
    <!-- Scroll to Top Button JS -->
    <script src="/dashboard/html/js/scrollToTop.js"></script>
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
    <jsp:include page="templates/topbar.jsp"/>


    <!-- ============================================================== -->
    <!-- Left Sidebar - style you can find in sidebar.scss  -->
    <!-- ============================================================== -->
    <jsp:include page="templates/left-sidebar.jsp"/>



    <!-- ============================================================== -->
    <!-- Dynamic Container - -->
    <!-- ============================================================== -->
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
        </div>
        <!-- Row -->
        <!-- ============================================================== -->
        <!-- End Page Content -->
        <!-- ============================================================== -->
    </div>


    <div class="row page-titles">
        <div class="col-md-5 col-8 align-self-center">
        </div>
    </div>


    <!-- ============================================================== -->
    <!-- Footer - the style is in style.css   -->
    <!-- ============================================================== -->
    <jsp:include page="templates/footer.jsp">
        <jsp:param name="currentPage" value="${currentPage}"/>
        <jsp:param name="totalPages" value="${totalPages}"/>
        <jsp:param name="size" value="${size}"/>
    </jsp:include>


    <!-- Scroll to top button -->
    <a href="#" class="scroll-to-top-btn"><i class="mdi mdi-arrow-up"></i></a>

</div>
<!-- ============================================================== -->
<!-- End Wrapper -->
<!-- ============================================================== -->
</body>

</html>

