<!--////////////////////////////////////////////////////////
// Copyright (c) 2005, Microsystems.
// All Rights Reserved
//
// This program, and all routines referenced herein,
// are the proprietary properties and trade secrets of
// Sun Microsystems
//
// Except as provided for by license agreement, this
// program shall not be duplicated, used or disclosed
// without the written consent, signed by an officer of
// Sun Microsystems.
////////////////////////////////////////////////////////-->



<%@ page import="com.sun.jbi.cam.common.GenericConstants,
   java.util.Map,
   java.util.HashMap" %>
   
<%

    
    String appName = "";
    String assemblyName = null;
    String name = 
            request.getParameter(GenericConstants.COMPONENT_NAME);
    String type = 
            request.getParameter(GenericConstants.COMPONENT_TYPE);
    if(type.equalsIgnoreCase(GenericConstants.SA_TYPE)) {
        appName = name;
        session.setAttribute(GenericConstants.APP_NAME,appName);
    } else {
        appName = (String)session.getAttribute(GenericConstants.APP_NAME);
    }
    String cname = 
            request.getParameter(GenericConstants.COMPONENT_CNAME);
    String ctype = 
            request.getParameter(GenericConstants.COMPONENT_CTYPE);
    String tName = 
            request.getParameter(GenericConstants.COMPONENT_TNAME);
 
   session.setAttribute(GenericConstants.COMPONENT_NAME,name);
   session.setAttribute(GenericConstants.COMPONENT_TYPE,type);
   session.setAttribute(GenericConstants.COMPONENT_CNAME,cname);
   session.setAttribute(GenericConstants.COMPONENT_CTYPE,ctype);
   session.setAttribute(GenericConstants.COMPONENT_TNAME,tName);

%>



<html>
<SCRIPT LANGUAGE="JavaScript" TYPE="text/javascript">
<!--
var cappName = "<%=appName%>";
var varName = "<%=name%>";
var varType = "<%=type%>";
var varcName = "<%=cname%>";
var varcType = "<%=ctype%>";
var vartName = "<%=tName%>";
var hasSVGSupport = false;
var useVBMethod = false;
var browserName = "";



function isASVInstalled()
{
    try{
        var asv = new ActiveXObject("Adobe.SVGCtl");
        return true;
    }
    catch(e){
    }
    return false;
}    

function redirectToPage() {
  if (navigator.userAgent.indexOf("Firefox") != -1) {
	hasSVGSupport = true;
	browserName = "Firefox";
   } else {
	if (navigator.mimeTypes != null && navigator.mimeTypes.length > 0) {
		browserName = "Mozilla";
		if (navigator.mimeTypes["image/svg-xml"] != null) {
                        alert("Mozilla -  image/svg-xml");
			hasSVGSupport = true;
		}
	} else {
		hasSVGSupport = isASVInstalled();
		browserName = "Internet Explorer";
	}
    }

    

   var browser = navigator.userAgent.toLowerCase();
   // get the workspace window
   var workspaces = document.getElementsByName("workspaceFrame");
   if(workspaces.length == 0) {
      // check the parent
      workspaces =parent.document.getElementsByName("workspaceFrame");
   }
   var workspace = workspaces[0];

   if((browserName == "Internet Explorer") ) {
       browser =  "&browser=msie"    
    } else {
       browser =  "&browser=firefox"    
    }
    if(hasSVGSupport ==  false) {
       workspace.src = 
           "/cam/faces/manager/framework/renderer/SVGViewerDownload.jsp?browser=" + browserName;
   
       return;
    } else {
       workspace.src = 
          "/cam/faces/manager/framework/renderer/SASplitViewer.jsp?name=" + 
             varName+ "&type=" +varType+"&cname=" + varcName + "&ctype=" +
             varcType + "&appName=" + cappName+browser + "&tname="+vartName;
   }
}
//-->
</SCRIPT>    

<body onLoad="redirectToPage()">
    
</body>
</html>
