<%@page pageEncoding="UTF-8"%>
<%@page import="com.sun.workflow.xml.Util" %>
<%@page import="javax.xml.parsers.DocumentBuilder"%>
<%@page import="com.sun.workflow.client.*"%>
<%@page import="javax.xml.parsers.DocumentBuilderFactory"%>
<%@page import="org.w3c.dom.*"%>

<html>
    <head>
        <link rel="stylesheet" type="text/css" href="../form.css">
        <link rel="stylesheet" type="text/css" href="../yui_2_5_2/assets/yui.css" >
        <%
            String taskInput = "";
            try {
                String userId = request.getUserPrincipal().getName();
                String taskIdStr = request.getParameter("taskId");
                long taskId = Long.parseLong(taskIdStr);

                com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
                com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
                java.lang.Object result = port.getTaskInput(taskId, userId);
                if (result != null && result instanceof Node) {
                    Node taskInputNode = (Node) result;
                    taskInput = Util.toXml(taskInputNode, "UTF-8", true);
                }

            } catch (Exception ex) {
                ex.printStackTrace();
            }
        %>
        <title> Task Input Details </title>
    </head>
    <body class="yui-skin-sam">
    	<div id="content">
	    <form id="details-form" name="details-form" method="post">

    		<fieldset>

        		<legend>Request</legend>

        		<div>
      			      <textarea cols="50" rows="15" style="border: 0px #000000 solid;" name="inputxml" readonly="readonly"><%=taskInput%></textarea></br>

        		</div>

    		</fieldset>
           </form>
     	</div>
    </body>
</html>