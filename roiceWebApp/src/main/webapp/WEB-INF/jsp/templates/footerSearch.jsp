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
            <c:url var="prevUrl" value="${baseUrl}">
                <c:param name="name" value="${paramName}" />
                <c:param name="page" value="${currentPage - 1}" />
                <c:param name="size" value="${size}" />
            </c:url>
            <a href="${prevUrl}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                <i class="mdi mdi-chevron-left"></i> Previous
            </a>
        </c:if>
        <!-- Next page button -->
        <c:if test="${currentPage < totalPages - 1}">
            <c:url var="nextUrl" value="${baseUrl}">
                <c:param name="name" value="${paramName}" />
                <c:param name="page" value="${currentPage + 1}" />
                <c:param name="size" value="${size}" />
            </c:url>
            <a href="${nextUrl}" class="btn btn-blue waves-effect waves-dark" aria-expanded="false">
                Next <i class="mdi mdi-chevron-right"></i>
            </a>
        </c:if>
        <!-- Page information -->
        <div>
            <c:choose>
                <c:when test="${totalPages > 0}">
                    <p class="small-text">Page ${currentPage + 1} of ${totalPages}</p>
                </c:when>
                <c:otherwise>
                    <p class="small-text">Page 0 of 0</p>
                </c:otherwise>
            </c:choose>
            <!-- Copyright information -->
            <div class="text-left copyright-text">
                <c:out value="© 2024 ROICE Web Application" />
            </div>
        </div>
    </div>
</footer>

</html>

