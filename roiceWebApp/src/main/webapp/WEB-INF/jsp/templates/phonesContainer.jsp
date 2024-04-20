<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">


    <!-- ============================================================== -->
    <!-- Start Page Content -->
    <!-- ============================================================== -->
    <!-- Row -->
    <div id="phones-container" class="row">
        <!-- Column -->
        <c:forEach var="phone" items="${phones}">
            <div class="col-lg-4 col-xlg-3 col-md-5">
                <div class="card">
                    <div class="card-block">
                        <div class="m-t-30" style="text-align: center;">
                            <!-- Redirect to phoneDetails.jsp page on click -->
                            <c:url var="phoneDetailsUrl" value="/phoneDetails">
                                <c:param name="phoneName" value="${phone.name}" />
                            </c:url>
                            <a href="${phoneDetailsUrl}">
                                <img src="${phone.picture}" class="img-rounded" width="150"/>
                            </a>
                            <h4 class="card-title m-t-10">
                                <a href="${phoneDetailsUrl}">
                                    <c:out value="${phone.name}" />
                                </a>
                            </h4>
                        </div>
                    </div>
                </div>
            </div>
        </c:forEach>
    </div>
    <!-- Row -->
    <!-- ============================================================== -->
    <!-- End Page Content -->
    <!-- ============================================================== -->

</html>
