<%@page pageEncoding="UTF-8" %>
<%@page import="com.sun.workflow.xml.Util" %>
<%@page import="javax.xml.parsers.DocumentBuilder"%>
<%@page import="com.sun.workflow.client.*"%>
<%@page import="javax.xml.parsers.DocumentBuilderFactory"%>

<%@page import="org.w3c.dom.*"%>
        <%
            String taskOutput = "";
            String userId = "";
            String taskIdStr = "";
            String saveForm= "";
            String resultString = "";
            String failed = "";
            long taskId = 0;
            com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
            com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
            
            try {
            	request.setCharacterEncoding("UTF-8");
                userId = request.getUserPrincipal().getName();
                taskIdStr = request.getParameter("taskId");
                taskId = Long.parseLong(taskIdStr);
                saveForm =  request.getParameter("saveForm");
                
                if (saveForm != null && saveForm.equals("Save")) {
                	String toSet = request.getParameter("output");
                	Node xmlNode = Util.toNode (toSet);               	
                 	service = new com.sun.workflow.client.TaskCommonService();
                	port = service.getTaskCommonPort();
                	com.sun.workflow.client.ResultCodeType result = port.setTaskOutput(taskId, xmlNode, userId);
                	resultString = result.value();
                	if (result != com.sun.workflow.client.ResultCodeType.SUCCESS) {
                		failed = "true";
                	} else {
                	    	failed = "false";
                	}
                }

            } catch (Exception ex) {
            	failed = "true";
                resultString = ex.getMessage ();
            }


            try {
                java.lang.Object result = port.getTaskOutput(taskId, userId);
                if (result != null && result instanceof Node) {
                    Node taskOutputNode = (Node) result;
                    taskOutput = Util.toXml(taskOutputNode, "UTF-8", true);
                }

            } catch (Exception ex) {
                ex.printStackTrace();
            }
        %>  

<html>
    <head>
        <link rel="stylesheet" type="text/css" href="../form.css">
        <script type="text/javascript" src="../yui_2_5_2/yuiloader/yuiloader-beta-min.js"></script>
	<script type="text/javascript" src="../yui_2_5_2/dom/dom-min.js"></script>
	<script type="text/javascript" src="../yui_2_5_2/event/event-min.js"></script>
	<script type="text/javascript" src="../yui_2_5_2/element/element-beta-min.js"></script>
	<script type="text/javascript" src="../yui_2_5_2/button/button-min.js"></script>
	<script type="text/javascript" src="../yui_2_5_2/container/container-min.js"></script>

 	<!-- Sam Skin CSS -->
 	<link rel="stylesheet" type="text/css" href="../yui_2_5_2/container/assets/skins/sam/container.css">
 	<link rel="stylesheet" type="text/css" href="../yui_2_5_2/assets/yui.css" >

 
 	<!-- OPTIONAL: You only need the YUI Button CSS if you're including YUI Button, mentioned below. -->
 	<link rel="stylesheet" type="text/css" href="../yui_2_5_2/button/assets/skins/sam/button.css">
 
 	<!-- Dependencies -->
 	<script type="text/javascript" src="../yui_2_5_2/yahoo-dom-event/yahoo-dom-event.js"></script>
 
 	<!-- OPTIONAL: Animation (only required if using ContainerEffect) -->
 	<script type="text/javascript" src="../yui_2_5_2/yahoo-dom-event/animation/animation-min.js"></script>
 
 	<!-- OPTIONAL: Connection (only required if using asynchronous form submission) -->
 	<script type="text/javascript" src="../yui_2_5_2/yahoo-dom-event/connection/connection-min.js"></script>
 
 	<!-- OPTIONAL: Drag & Drop (only required if enabling Drag & Drop) -->
 	<script type="text/javascript" src="../yui_2_5_2/yahoo-dom-eventdragdrop/dragdrop-min.js"></script>
 
 	<!-- OPTIONAL: YUI Button (these 2 files only required if you want SimpleDialog to use YUI Buttons, instead of HTML Buttons) -->
 	<script type="text/javascript" src="../yui_2_5_2/yahoo-dom-event/element/element-min.js"></script>
 	<script type="text/javascript" src="../yui_2_5_2/yahoo-dom-event/button/button-min.js"></script>
 
  
 	<!-- Fonts CSS - Recommended but not required -->
 	<link rel="stylesheet" type="text/css" href="./yui_2_5_2/fonts/fonts-min.css">
 

     
        <title> Task Output Details </title>
    </head>
    <body class="yui-skin-sam">   
    	<div id="content">
	    <form id="details-form" name="details-form" method="post" action="taskDetailsOut.jsp">

    		<fieldset>

        		<legend>Response</legend>

        		<div>
      			      <textarea cols="50" rows="12" style="border: 0px #000000 solid;" name="output"><%=taskOutput%></textarea></br>
        		</div>
            		<div>
                	     <!-- <span id="submitbutton3" class="yui-button yui-submit-button">
                	       <span class="first-child"><input id="saveForm" type="submit" name="saveForm" value="Save">
                	       </span>
                	      </span> -->
                	      <input type="submit" id="saveForm" name="saveForm" value="Save"/>
                	</div>
                	<input type="hidden" name="taskId" value="<%=taskId%>" />

    		</fieldset>   
           </form>
    		
     	</div>
     	
  	<script type="text/javascript">
		var oSubmitButton1 = new YAHOO.widget.Button("saveForm", { value: "Save", label: "Save", isDefault:true});
 	</script>

	
        <script type="text/javascript">
         	YAHOO.namespace("example.container");
         	
         	
                function init() {

         	var isFailed =  "<%=failed%>" ;
         	
         	var processresult = YAHOO.widget.SimpleDialog.ICON_INFO;
         	
         	if (isFailed == "true") {
         	     processresult = YAHOO.widget.SimpleDialog.ICON_WARN;
         	}
         	
		var handleClose = function() {
			this.hide();
		};
         	
		// Instantiate the Dialog
		YAHOO.example.container.resultdialog = 
			new YAHOO.widget.SimpleDialog("simpledialog1", { width: "300px",
			   fixedcenter: true,
			   visible: false,
			   draggable: false,
			   close: true,
			   text: "<%=resultString%>",
			   icon: YAHOO.widget.SimpleDialog.ICON_INFO,
			   constraintoviewport: true,
			   buttons: [ { text:"Close", handler:handleClose, isDefault:true }]
			 } );
			 
		 
            		 if (isFailed != "") {
             		        YAHOO.example.container.resultdialog.render ("content");
             		 	YAHOO.example.container.resultdialog.show ();
             		 }  
             	}
             	YAHOO.util.Event.addListener(window, "load", init);
        </script> 
        
    </body>
</html>