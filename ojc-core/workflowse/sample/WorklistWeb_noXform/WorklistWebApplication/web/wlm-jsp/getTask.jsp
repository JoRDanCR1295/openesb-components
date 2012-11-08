<%@page contentType="application/xml"%>
<%@page pageEncoding="UTF-8"%>

<%-- start web service invocation --%>
<%@page import="java.util.List"%>
<%@page import="com.sun.workflow.client.*"%>
<%@page import="com.sun.workflow.xml.*"%>


<%!
    public boolean isNull(String text) {
        if (text == null || text.trim().length() == 0) {
            return true;
        } else {
            return false;
        }
    }
%>

<%
        try {
            String userId = request.getUserPrincipal().getName();
            String  taskIdStr = request.getParameter("taskId");
            long taskId = Long.parseLong(taskIdStr);


            com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
            com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
            // TODO process result here
            com.sun.workflow.client.TaskType task = port.getTask(taskId, userId);

            out.print("<taskDetails>");



            if (task != null) {
                out.print("<task>");
                out.print("<id>");
                out.print(task.getTaskId());
                out.print("</id>");

                out.print("<title>");
                out.print(task.getTitle() == null ? "" : task.getTitle());
                out.print("</title>");

                out.print("<createDate>");
                out.print(task.getSubmittedDate().toString());
                out.print("</createDate>");

                out.print("<priority>");
                out.print(task.getPriority());
                out.print("</priority>");

                out.print("<status>");
                out.print(task.getStatus().value());
                out.print("</status>");

                out.print("<assignedTo>");
                out.print(task.getAssignedTo() == null ? "" : task.getAssignedTo());
                out.print("</assignedTo>");

                out.print("<owner>");
                out.print(task.getClaimedBy() == null ? "" : task.getClaimedBy());
                out.print("</owner>");

                out.print("<deadline>");
                out.print(task.getDeadline() == null ? "" : task.getDeadline().toString ());
                out.print("</deadline>");

                /**  Use the following method to get keywords
                    String keywords = task.getKeywords();
                    List<String> keywordsList = com.sun.workflow.xml.KeywordsHelper.getKeywords(keywords);
                    out.print("<OrderId>");
                    out.print(keywordsList.get(0));
                    out.print("</OrderId1>");
                     ...
                     //Also, need to add each keyword tag (e.g. OrderId) to wlmEntry.jsp's myColumnHeaders, myDataSource.responseSchema
                **/

                out.print("</task>");
            }
            out.print("</taskDetails>");
        } catch (Exception ex) {
            out.print(ex.getMessage());
        }
%>
<%-- end web service invocation --%>


