<%@page contentType="application/xml"%>
<%@page pageEncoding="UTF-8"%>

<%-- start web service invocation --%>
<%@page import="java.util.List"%>
<%@page import="com.sun.workflow.client.*"%>

<%!
    public boolean isNull(String text) {
        if (text == null || text.trim().length() == 0) {
            return true;
        } else {
            return false;
        }
    }
%>

<%-- start web service invocation --%>
<%

        try {
            String userId = request.getUserPrincipal().getName();
            String taskIdStr = request.getParameter("taskId");
            long taskId = Long.parseLong(taskIdStr);
            String reAssignUser = request.getParameter("reAssignUser");
            String reAssignGroup = request.getParameter("reAssignGroup");
            if (!isNull(reAssignUser)) {
                reAssignUser = reAssignUser.trim();
            } else {
                reAssignUser = null;
            }
            if (!isNull(reAssignUser)) {
                reAssignGroup = reAssignGroup.trim();
            } else {
                reAssignGroup = null;
            }
            com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
            com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();

            com.sun.workflow.client.ResultCodeType result = port.reassignTask(taskId, reAssignGroup, reAssignUser, "", "", userId);
            out.println("<Result>" + result.value() + "</Result>");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
%>
<%-- end web service invocation --%>
