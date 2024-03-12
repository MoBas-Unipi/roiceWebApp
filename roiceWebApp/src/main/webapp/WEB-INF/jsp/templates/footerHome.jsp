<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">

<!-- ============================================================== -->
<!-- Footer - the style is in style.css   -->
<!-- ============================================================== -->
<footer class="footer">
    <div class="text-center">
        <!-- Previous page button -->
        <c:if test="${currentPage > 0}">
            <a href="/homePage?page=${currentPage - 1}&size=${size}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                <i class="mdi mdi-chevron-left"></i> Previous
            </a>
        </c:if>

        <!-- Next page button -->
        <c:if test="${currentPage < totalPages - 1}">
            <a href="/homePage?page=${currentPage + 1}&size=${size}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                Next <i class="mdi mdi-chevron-right"></i>
            </a>
        </c:if>

        <div>
            <p class="small-text">Page ${currentPage + 1} of ${totalPages}</p>
            <div class="text-left copyright-text">
                <c:out value="© 2024 ROICE Web Application" />
            </div>
        </div>
    </div>
</footer>


</html>