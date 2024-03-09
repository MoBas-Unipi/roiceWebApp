<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">

<!-- ============================================================== -->
<!-- Footer - the style is in style.css   -->
<!-- ============================================================== -->
<footer class="footer">
    <!-- Container for footer content -->
    <div class="text-center">
        <!-- Previous page button, visible if current page is greater than 0 -->
        <c:if test="${currentPage > 0}">
            <a href="/userHome?page=${currentPage - 1}&size=${size}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                <i class="mdi mdi-chevron-left"></i> Previous
            </a>
        </c:if>
        <!-- Next page button, visible if current page is less than total pages minus 1 -->
        <c:if test="${currentPage < totalPages - 1}">
            <a href="/userHome?page=${currentPage + 1}&size=${size}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                Next <i class="mdi mdi-chevron-right"></i>
            </a>
        </c:if>
        <div>
            <!-- Text displaying current page number and total pages -->
            <p class="small-text">Page ${currentPage + 1} of ${totalPages}</p>
            <!-- Text displaying copyright information -->
            <div class="text-left copyright-text">
                <c:out value="© 2024 ROICE Web Application" />
            </div>
        </div>
    </div>
</footer>

</html>