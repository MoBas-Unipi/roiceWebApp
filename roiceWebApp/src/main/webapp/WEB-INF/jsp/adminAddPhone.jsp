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

<!-- Include JS to check input fields -->
<script src="/auth/javascript/formValidation.js"></script>
<%--<script src="/dashboard/html/js/addPhone.js"></script>--%>
<!-- Include Font Awesome CSS -->
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css">
<!-- Include CSS style -->
<link rel="stylesheet" href="/dashboard/html/css/addPhone.css">

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
                <h3 class="text-themecolor m-b-0 m-t-0">Add Phone</h3>
                <ol class="breadcrumb">
                    <li class="breadcrumb-item"><a href="/userHome">Home</a></li>
                    <li class="breadcrumb-item active">Add phone</li>
                </ol>
            </div>
        </div>

        <div id="contentArea" class="mt-4">
            <div class="addPhone-form">
                <form action="${pageContext.request.contextPath}/addPhone" method="POST" class="register-form" id="register-form" onsubmit="return validateForm(
                        ['name', 'picture', 'batterySize', 'batteryType', 'body', 'brand', 'cameraPixels', 'chipset', 'displayResolution', 'displaySize', 'os', 'ram', 'releaseYear', 'storage', 'videoPixels'])">

                    <div class="row">
                        <div class="col-md-3">
                            <div class="form-group">
                                <label for="name"><i class="fas fa-mobile-alt"></i></label>
                                <input type="text" name="name" id="name" placeholder="Phone Name" value="${phoneDTO.name}"/>
                                <c:if test="${not empty errorMap['name']}">
                                    <div style="color: red;">${errorMap['name']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="brand"><i class="fas fa-tags"></i></label>
                                <input type="text" name="brand" id="brand" placeholder="Brand" value="${phoneDTO.brand}"/>
                                <c:if test="${not empty errorMap['brand']}">
                                    <div style="color: red;">${errorMap['brand']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="picture"><i class="fas fa-image"></i></label>
                                <input type="text" name="picture" id="picture" placeholder="Phone Picture" value="${phoneDTO.picture}"/>
                                <c:if test="${not empty errorMap['picture']}">
                                    <div style="color: red;">${errorMap['picture']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="chipset"><i class="fas fa-microchip"></i></label>
                                <input type="text" name="chipset" id="chipset" placeholder="Chipset" value="${phoneDTO.chipset}"/>
                                <c:if test="${not empty errorMap['chipset']}">
                                    <div style="color: red;">${errorMap['chipset']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="batterySize"><i class="fas fa-battery-three-quarters"></i></label>
                                <input type="text" name="batterySize" id="batterySize" placeholder="Battery Size" value="${phoneDTO.batterySize}"/>
                                <c:if test="${not empty errorMap['batterySize']}">
                                    <div style="color: red;">${errorMap['batterySize']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="batteryType"><i class="fas fa-battery-full"></i></label>
                                <input type="text" name="batteryType" id="batteryType" placeholder="Battery Type" value="${phoneDTO.batteryType}"/>
                                <c:if test="${not empty errorMap['batteryType']}">
                                    <div style="color: red;">${errorMap['batteryType']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="body"><i class="fas fa-mobile-alt"></i></label>
                                <input type="text" name="body" id="body" placeholder="Body" value="${phoneDTO.body}"/>
                                <c:if test="${not empty errorMap['body']}">
                                    <div style="color: red;">${errorMap['body']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="cameraPixels"><i class="fas fa-camera"></i></label>
                                <input type="text" name="cameraPixels" id="cameraPixels" placeholder="Camera Pixels" value="${phoneDTO.cameraPixels}"/>
                                <c:if test="${not empty errorMap['cameraPixels']}">
                                    <div style="color: red;">${errorMap['cameraPixels']}</div>
                                </c:if>
                            </div>
                        </div>

                        <div class="col-md-3">
                            <div class="form-group">
                                <label for="videoPixels"><i class="fas fa-video"></i></label>
                                <input type="text" name="videoPixels" id="videoPixels" placeholder="Video Pixels" value="${phoneDTO.videoPixels}"/>
                                <c:if test="${not empty errorMap['videoPixels']}">
                                    <div style="color: red;">${errorMap['videoPixels']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="displayResolution"><i class="fas fa-desktop"></i></label>
                                <input type="text" name="displayResolution" id="displayResolution" placeholder="Display Resolution" value="${phoneDTO.displayResolution}"/>
                                <c:if test="${not empty errorMap['displayResolution']}">
                                    <div style="color: red;">${errorMap['displayResolution']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="displaySize"><i class="fas fa-desktop"></i></label>
                                <input type="text" name="displaySize" id="displaySize" placeholder="Display Size" value="${phoneDTO.displaySize}"/>
                                <c:if test="${not empty errorMap['displaySize']}">
                                    <div style="color: red;">${errorMap['displaySize']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="os"><i class="fas fa-laptop"></i></label>
                                <input type="text" name="os" id="os" placeholder="Operating System" value="${phoneDTO.os}"/>
                                <c:if test="${not empty errorMap['os']}">
                                    <div style="color: red;">${errorMap['os']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="ram"><i class="fas fa-memory"></i></label>
                                <input type="text" name="ram" id="ram" placeholder="RAM" value="${phoneDTO.ram}"/>
                                <c:if test="${not empty errorMap['ram']}">
                                    <div style="color: red;">${errorMap['ram']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="storage"><i class="fas fa-hdd"></i></label>
                                <input type="text" name="storage" id="storage" placeholder="Storage" value="${phoneDTO.storage}"/>
                                <c:if test="${not empty errorMap['storage']}">
                                    <div style="color: red;">${errorMap['storage']}</div>
                                </c:if>
                            </div>

                            <div class="form-group">
                                <label for="releaseYear"><i class="fas fa-calendar-alt"></i></label>
                                <input type="text" name="releaseYear" id="releaseYear" placeholder="Release Year" value="${phoneDTO.releaseYear}"/>

                                <c:if test="${not empty errorMap['releaseYear']}">
                                    <c:choose>
                                        <c:when test="${errorMap['releaseYear'].contains('Failed to convert property value')}">
                                            <div style="color: red;">Release year must be a number</div>
                                        </c:when>
                                        <c:otherwise>
                                            <div style="color: red;">${errorMap['releaseYear']}</div>
                                        </c:otherwise>
                                    </c:choose>
                                </c:if>

                            </div>
                        </div>

                        <div class="col-md-1">
                            <button id="addPhoneBtn" class="btn btn-primary btn-sm" style="background-color: #26c6da; border-color: #00aced; padding: 5px 10px; font-size: 15px;">Add phone</button>
                        </div>

                        <div class="col-md-3">
                            <div id="message-container" style="color: ${not empty error or not empty errorMap ? 'red' : 'green'};">
                                <!-- Define error and success messages with display: none -->
                                <div class="error-message" id="error-message" style="display: none; margin-top: 4px"></div>
                                <div class="success-message" id="success-message" style="display: none; margin-top: 4px"></div>

                                <!-- Use choose statement to populate and display messages -->
                                <c:choose>
                                    <c:when test="${not empty error}">
                                        <script>
                                            displayMessage("error-message", "${error}");
                                        </script>
                                    </c:when>
                                    <c:when test="${not empty sessionScope.success}">
                                        <script>
                                            displayMessage("success-message", "${sessionScope.success}");
                                        </script>
                                    </c:when>
                                </c:choose>

                                <!-- Placeholder div for error-field message (if needed) -->
                                <div id="error-field" class="error-message" style="color: red; display: none; margin-top: 4px"></div>
                            </div>
                        </div>

                    </div>
                </form>
            </div>
        </div>
    </div>
</div>

<footer class="footer">
    <div class="text-left copyright-text">
        <c:out value="© 2024 ROICE Web Application" />
    </div>
</footer>

</body>
</html>
