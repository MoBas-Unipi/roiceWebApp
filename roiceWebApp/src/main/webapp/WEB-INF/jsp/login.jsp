<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Roice</title>

    <!-- Favicon icon -->
    <link rel="icon" type="image/png" href="/dashboard/assets/images/RoiceIcon.png">

    <!-- Font Icon -->
    <link rel="stylesheet" href="/auth/fonts/material-icon/css/material-design-iconic-font.min.css">

    <!-- Main css -->
    <link rel="stylesheet" href="/auth/css/style.css">
</head>

<script src="/auth/javascript/loginValidation.js"></script>

<body>

<div class="main">
    <!-- Sing in Form -->
    <section class="sign-in">
        <div class="container">
            <div class="signin-content">
                <div class="signin-image">
                    <figure><img src="/auth/images/RoiceLogo.png" alt="sign up image"></figure>
                    <a href="/signUp" class="signup-image-link">Create an account</a>
                </div>

                <div class="signin-form">
                    <h2 class="form-title">Login</h2>
                    <form method="POST" class="register-form" id="login-form" onsubmit="return validateLogin()" action="${pageContext.request.contextPath}/login">
                        <div class="form-group">
                            <label for="email"><i class="zmdi zmdi-account material-icons-name"></i></label>
                            <input type="text" name="email" id="email" placeholder="e-mail"/>
                        </div>
                        <div class="form-group">
                            <label for="password"><i class="zmdi zmdi-lock"></i></label>
                            <input type="password" name="password" id="password" placeholder="password"/>
                        </div>
                        <div class="form-group form-button">
                            <input type="submit" name="signin" id="signin" class="form-submit" value="Log in"/>
                        </div>
                        <!-- Error message -->
                        <p id="error-field" style="color: red;">${error}</p>
                        <!-- End of error message -->
                    </form>
                </div>
            </div>
        </div>
    </section>
</div>
</body>
</html>
