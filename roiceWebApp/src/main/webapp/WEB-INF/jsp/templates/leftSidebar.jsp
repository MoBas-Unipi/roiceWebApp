<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri="jakarta.tags.core" prefix="c" %>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">

<!-- ============================================================== -->
<!-- Left Sidebar - style you can find in sidebar.scss  -->
<!-- ============================================================== -->
<div>
    <div id="sidebar">
        <aside class="left-sidebar">
            <!-- Sidebar scroll-->
            <div class="scroll-sidebar">
                <!-- Sidebar navigation-->
                <nav class="sidebar-nav">
                    <ul id="sidebarnav">
                        <li><a class="waves-effect waves-dark" href="/homePage" aria-expanded="false"><i class="mdi mdi-cellphone"></i><span class="hide-menu">Home</span></a></li>
                        <c:choose>
                            <c:when test="${userClass eq 'user'}">
                                <li> <a class="waves-effect waves-dark" href="/userPage" aria-expanded="false"><i class="mdi mdi-account-check"></i><span class="hide-menu">Profile</span></a></li>
                            </c:when>
                            <c:when test="${userClass eq 'admin'}">
                                <li><a class="waves-effect waves-dark" href="#" aria-expanded="false"><i class="mdi mdi-plus-circle"></i><span class="hide-menu">Add Phone</span></a></li>
                                <li><a class="waves-effect waves-dark" href="#" aria-expanded="false"><i class="mdi mdi-timer"></i><span class="hide-menu">Add Auction</span></a></li>
                            </c:when>
                        </c:choose>
                        <li><a class="waves-effect waves-dark" href="#" aria-expanded="false"><i class="mdi mdi-alert-circle"></i><span class="hide-menu">Live Auctions</span></a>
                        </li>
                    </ul>
                </nav>
                <!-- End Sidebar navigation -->
            </div>
            <!-- End Sidebar scroll-->
            <!-- Bottom points-->
            <div class="sidebar-footer">
                <!-- item-->
                <a href="/logout" class="link" data-toggle="tooltip" title="Logout" style="display: flex; align-items: center;">
                    <i class="mdi mdi-power" style="margin-right: 5px;"></i> Logout
                </a>
            </div>

            <!-- End Bottom points-->
        </aside>
    </div>
</div>

</html>