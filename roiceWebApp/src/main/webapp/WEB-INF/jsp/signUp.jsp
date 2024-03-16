<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Sign Up</title>
    <!-- Favicon icon -->
    <link rel="icon" type="image/png" href="RoiceIcon.png">
    <!-- Font Icon -->
    <link rel="stylesheet" href="/auth/fonts/material-icon/css/material-design-iconic-font.min.css">
    <!-- Main css -->
    <link rel="stylesheet" href="/auth/css/style.css">
</head>

<script src="/auth/javascript/formValidation.js"></script>

<body>
<div class="main">
    <!-- Sign up form -->
    <section class="signup">
        <div class="container">
            <div class="signup-content">
                <div class="signup-form">
                    <h2 class="form-title">Sign up</h2>
                    <form action="${pageContext.request.contextPath}/signUp" method="POST" class="register-form" id="register-form" onsubmit="return validateForm(
                            ['firstName', 'lastName', 'email', 'password', 'country', 'city', 'streetName', 'streetNumber'])">
                    <div class="form-group">
                            <label for="firstName"><i class="zmdi zmdi-account material-icons-name"></i></label>
                            <input type="text" name="firstName" id="firstName" placeholder="First Name" value="${userDTO.firstName}"/>
                            <c:if test="${not empty errorMap['firstName']}">
                                <div style="color: red;">${errorMap['firstName']}</div>
                            </c:if>
                        </div>

                        <div class="form-group">
                            <label for="lastName"><i class="zmdi zmdi-account-o"></i></label>
                            <input type="text" name="lastName" id="lastName" placeholder="Last Name" value="${userDTO.lastName}"/>
                            <c:if test="${not empty errorMap['lastName']}">
                                <div style="color: red;">${errorMap['lastName']}</div>
                            </c:if>
                        </div>

                        <div class="form-group">
                            <label for="email"><i class="zmdi zmdi-email"></i></label>
                            <input type="email" name="email" id="email" placeholder="E-mail" value="${userDTO.email}"/>
                            <c:if test="${not empty errorMap['email']}">
                                <div style="color: red;">${errorMap['email']}</div>
                            </c:if>
                        </div>

                        <div class="form-group">
                            <label for="password"><i class="zmdi zmdi-lock"></i></label>
                            <input type="password" name="password" id="password" placeholder="Password"/>
                            <c:if test="${not empty errorMap['password']}">
                                <div style="color: red;">${errorMap['password']}</div>
                            </c:if>
                        </div>

                        <div class="form-group">
                            <label for="country"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="country" id="country" placeholder="Country" value="${userDTO.country}"/>
                            <c:if test="${not empty errorMap['country']}">
                                <div style="color: red;">${errorMap['country']}</div>
                            </c:if>
                        </div>

                        <div class="form-group">
                            <label for="city"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="city" id="city" placeholder="City" value="${userDTO.city}"/>
                            <c:if test="${not empty errorMap['city']}">
                                <div style="color: red;">${errorMap['city']}</div>
                            </c:if>
                        </div>

                        <div class="form-group">
                            <label for="streetName"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="streetName" id="streetName" placeholder="Street" value="${userDTO.streetName}"/>
                            <c:if test="${not empty errorMap['streetName']}">
                                <div style="color: red;">${errorMap['streetName']}</div>
                            </c:if>
                        </div>

                        <div class="form-group">
                            <label for="streetNumber"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="streetNumber" id="streetNumber" placeholder="Street Number"
                                   value="${not empty errorMap['streetNumber'] ? '' : userDTO.streetNumber}"/>
                            <c:if test="${not empty errorMap['streetNumber']}">
                                <c:choose>
                                    <c:when test="${errorMap['streetNumber'].contains('Failed to convert property value')}">
                                        <div style="color: red;">Street number must be a number</div>
                                    </c:when>
                                    <c:otherwise>
                                        <div style="color: red;">${errorMap['streetNumber']}</div>
                                    </c:otherwise>
                                </c:choose>
                            </c:if>
                        </div>

                        <div class="form-group form-button">
                            <input type="submit" name="signup" id="signup" class="form-submit" value="Register"/>
                        </div>

                        <div id="error-field" class="error-message" style="color: red; display: ${not empty error ? 'block' : 'none'};">
                            ${error}
                        </div>
                    </form>
                </div>
                <div class="signup-image">
                    <figure><img src="/auth/images/RoiceLogo.png" alt="sign up image"></figure>
                    <a href="/login" class="signup-image-link">I am already member</a>
                </div>
            </div>
        </div>
    </section>
</div>

</body>
</html>
