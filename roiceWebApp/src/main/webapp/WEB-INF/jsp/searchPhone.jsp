<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">

<head>
    <!-- ============================================================== -->
    <!--  Header -->
    <!-- ============================================================== -->
    <jsp:include page="templates/header.jsp"/>
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
    <jsp:include page="templates/leftSidebar.jsp"/>

    <div class="row page-titles" style="margin-left: 250px">
        <div class="col-md-5 col-8 align-self-center">
            <h3 class="text-themecolor m-b-0 m-t-0">Phone Details</h3>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href="/homePage">Home</a></li>
                <li class="breadcrumb-item active">Search</li>
            </ol>
        </div>
    </div>
    <!-- ============================================================== -->
    <!-- Dynamic Container - -->
    <!-- ============================================================== -->
    <div class="container-fluid">

        <!-- Start page content -->
        <div id="phones-container" class="row">
            <!-- Column -->
            <c:forEach var="phone" items="${phones}">
                <div class="col-lg-4 col-xlg-3 col-md-5">
                    <div class="card">
                        <div class="card-block">
                            <div class="m-t-30" style="text-align: center;">
                                <!-- Redirect to phoneDetails.jsp page on click -->
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


        <!-- End page content -->
        <div class="row page-titles">
            <div class="col-md-5 col-8 align-self-center">
            </div>
        </div>

    <!-- Footer -->
    <footer class="footer">
        <div class="text-center">
            <!-- Previous page button -->
            <c:if test="${currentPage > 0}">
                <a href="/searchPhone?name=${paramName}&page=${currentPage - 1}&size=${size}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                    <i class="mdi mdi-chevron-left"></i> Previous
                </a>
            </c:if>
            <!-- Next page button -->
            <c:if test="${currentPage < totalPages - 1}">
                <a href="/searchPhone?name=${paramName}&page=${currentPage + 1}&size=${size}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                    Next <i class="mdi mdi-chevron-right"></i>
                </a>
            </c:if>
            <!-- Page information -->
            <div>
                <p class="small-text">Page ${currentPage + 1} of ${totalPages}</p>
                <!-- Copyright information -->
                <div class="text-left copyright-text">
                    <c:out value="© 2024 ROICE Web Application" />
                </div>
            </div>
        </div>
    </footer>



        <!-- Scroll to top button -->
        <a href="#" class="scroll-to-top-btn"><i class="mdi mdi-arrow-up"></i></a>

<!-- End Wrapper -->
</div>

</body>

</html>
