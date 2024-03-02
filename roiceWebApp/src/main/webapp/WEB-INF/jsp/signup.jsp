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
<body>

<div class="main">
    <!-- Sign up form -->
    <section class="signup">
        <div class="container">
            <div class="signup-content">
                <div class="signup-form">
                    <h2 class="form-title">Sign up</h2>
                    <form action="${pageContext.request.contextPath}/signup" method="POST" class="register-form" id="register-form">
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
                            <label for="agencyName"><i class="zmdi zmdi-layers"></i></label>
                            <input type="text" name="agencyName" id="agencyName" placeholder="Agency Name"/>
                        </div>

                        <div class="form-group">
                            <label for="agencyDetails"><i class="zmdi zmdi-assignment"></i></label>
                            <input type="text" name="agencyDetails" id="agencyDetails" placeholder="Agency Details"/>
                        </div>

                        <div class="form-group">
                            <label for="mobileNumber"><i class="zmdi zmdi-phone"></i></label>
                            <input type="text" name="mobileNumber" id="mobileNumber" placeholder="Contact Number"/>
                        </div>

                        <div class="form-group">
                            <input type="checkbox" name="agree-term" id="agree-term" class="agree-term" />
                            <label for="agree-term" class="label-agree-term"><span><span></span></span>I agree all statements in  <a href="#" class="term-service">Terms of service</a></label>
                        </div>
                        <div class="form-group form-button">
                            <input type="submit" name="signup" id="signup" class="form-submit" value="Register"/>
                        </div>
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
<script src="/auth/vendor/jquery/jquery.min.js"></script>
</body>
</html>
