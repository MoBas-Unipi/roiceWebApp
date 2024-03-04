<%@ page contentType="text/html; charset=UTF-8"
         pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">

<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Sign Up</title>
    <!-- Favicon icon -->
    <link rel="icon" type="image/png" href="favicon.png">
    <!-- Font Icon -->
    <link rel="stylesheet" href="/auth/fonts/material-icon/css/material-design-iconic-font.min.css">
    <!-- Main css -->
    <link rel="stylesheet" href="/auth/css/style.css">
</head>

<script src="/auth/javascript/signupValidation.js"></script>

<body>
<div class="main">
    <!-- Sign up form -->
    <section class="signup">
        <div class="container">
            <div class="signup-content">
                <div class="signup-form">
                    <h2 class="form-title">Sign up</h2>
                    <form action="${pageContext.request.contextPath}/signup" method="POST" class="register-form" id="register-form" onsubmit="return validateForm()">

                        <div class="form-group">
                            <label for="firstName"><i class="zmdi zmdi-account material-icons-name"></i></label>
                            <input type="text" name="firstName" id="firstName" placeholder="First Name"/>
                            <small class="text-danger">
                                <c:if test="${not empty param.error && param.error == 'true'}">
                                    <c:out value="${param.errorMessage}"/>
                                </c:if>
                            </small>
                        </div>

                        <div class="form-group">
                            <label for="lastName"><i class="zmdi zmdi-account-o"></i></label>
                            <input type="text" name="lastName" id="lastName" placeholder="Last Name"/>
                        </div>

                        <div class="form-group">
                            <label for="email"><i class="zmdi zmdi-email"></i></label>
                            <input type="email" name="email" id="email" placeholder="Your Email"/>
                        </div>

                        <div class="form-group">
                            <label for="password"><i class="zmdi zmdi-lock"></i></label>
                            <input type="password" name="password" id="password" placeholder="Password"/>
                        </div>

                        <div class="form-group">
                            <label for="country"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="country" id="country" placeholder="Country"/>
                        </div>

                        <div class="form-group">
                            <label for="city"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="city" id="city" placeholder="City"/>
                        </div>

                        <div class="form-group">
                            <label for="street"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="street" id="street" placeholder="Street"/>
                        </div>

                        <div class="form-group">
                            <label for="streetNumber"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="streetNumber" id="streetNumber" placeholder="Street Number"/>
                        </div>

                        <div class="form-group form-button">
                            <input type="submit" name="signup" id="signup" class="form-submit" value="Register"/>
                        </div>
                        <div id="error-field" class="error-message" style="color: red; display:none;"></div>
                    </form>
                </div>
                <div class="signup-image">
                    <figure><img src="/auth/images/signup-image.jpg" alt="sing up image"></figure>
                    <a href="/login" class="signup-image-link">I am already member</a>
                </div>
            </div>
        </div>
    </section>
</div>

<!-- JS -->
<%--<script src="/auth/vendor/jquery/jquery.min.js"></script>--%>


</body>
</html>
