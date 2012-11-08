<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>


<%-- start web service invocation --%>
<%

        String taskListAction = request.getParameter("taskListAction");
        String taskIdStr = request.getParameter("taskId");
        long taskId = Long.parseLong(taskIdStr);
        String reAssignUser = request.getParameter("reAssignUser");
        String reAssignGroup = request.getParameter("reAssignGroup");

%>


<c:if test="${param.taskListAction =='checkout'}">
    <jsp:include page="checkoutTask.jsp" >
        <jsp:param name="taskId" value="<%=taskId%>" />
    </jsp:include>
</c:if>
<c:if test="${param.taskListAction =='complete'}">
    <jsp:include page="completeTask.jsp" >
        <jsp:param name="taskId" value="<%=taskId%>" />
    </jsp:include>
</c:if>
<c:if test="${param.taskListAction =='reassign'}">
    <jsp:include page="reassignTask.jsp" >
        <jsp:param name="taskId" value="<%=taskId%>" />
        <jsp:param name="reAssignUser" value="<%=reAssignUser%>" />
        <jsp:param name="reAssignGroup" value="<%=reAssignGroup%>" />
    </jsp:include>
</c:if>
<%-- end web service invocation --%>


