<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Login</title>

    <!-- Favicon icon -->
    <link rel="icon" type="image/png" href="favicon.png">

    <!-- Font Icon -->
    <link rel="stylesheet" href="/auth/fonts/material-icon/css/material-design-iconic-font.min.css">

    <!-- Main css -->
    <link rel="stylesheet" href="/auth/css/style.css">
</head>
<body>

<div class="main">
    <!-- Sing in Form -->
    <section class="sign-in">
        <div class="container">
            <div class="signin-content">
                <div class="signin-image">
                    <figure><img src="/auth/images/signin-image.jpg" alt="sing up image"></figure>
                    <a href="/signup" class="signup-image-link">Create an account</a>
                </div>

                <div class="signin-form">
                    <h2 class="form-title">Login</h2>
                    <form method="POST" class="register-form" id="login-form" action="${pageContext.request.contextPath}/login">
                        <div class="form-group">
                            <label for="username"><i class="zmdi zmdi-account material-icons-name"></i></label>
                            <input type="text" name="username" id="username" placeholder="username"/>
                        </div>
                        <div class="form-group">
                            <label for="password"><i class="zmdi zmdi-lock"></i></label>
                            <input type="password" name="password" id="password" placeholder="Password"/>
                        </div>
                        <div class="form-group form-button">
                            <input type="submit" name="signin" id="signin" class="form-submit" value="Log in"/>
                        </div>
                    </form>
                    <!-- Error message -->
                    <div id="errors" class="text-danger" style="display: none;">
                        <c:if test="${not empty param.error}">
                            Invalid username or password
                        </c:if>
                    </div>
                    <!-- End of error message -->
                </div>
            </div>
        </div>
    </section>
</div>

<!-- JS -->
<script src="/auth/vendor/jquery/jquery.min.js"></script>
<script>
    $(document).ready(function() {
        $('#login-form').submit(function(event) {
            var username = $('#username').val().trim();
            var password = $('#password').val().trim();

            if (username === '' || password === '') {
                $('#errors').show();
                event.preventDefault(); // Prevent form submission
            }
        });
    });
</script>
</body>
</html>
