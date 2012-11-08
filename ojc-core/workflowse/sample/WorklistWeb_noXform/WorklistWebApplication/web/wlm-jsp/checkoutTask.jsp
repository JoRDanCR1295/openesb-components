<%@page pageEncoding="UTF-8"%>
<%-- start web service invocation --%>
    <%
      try {
        String userId = request.getUserPrincipal().getName();
        String taskIdStr = request.getParameter("taskId");
        long taskId = Long.parseLong(taskIdStr);
        
	com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
	com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
	com.sun.workflow.client.ResultCodeType result = port.claimTask(taskId,userId);
	out.print("<Result>"+result.value()+"</Result>");
    } catch (Exception ex) {
	ex.printStackTrace();
    }
 %>
    <%-- end web service invocation --%>