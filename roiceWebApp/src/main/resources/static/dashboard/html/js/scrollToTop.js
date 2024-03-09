$(document).ready(function(){
    $(window).scroll(function(){
        if ($(this).scrollTop() > 100) {
            $('.scroll-to-top-btn').fadeIn();
        } else {
            $('.scroll-to-top-btn').fadeOut();
        }
    });

    $('.scroll-to-top-btn').click(function(){
        $('html, body').animate({scrollTop : 0},800);
        return false;
    });
});
