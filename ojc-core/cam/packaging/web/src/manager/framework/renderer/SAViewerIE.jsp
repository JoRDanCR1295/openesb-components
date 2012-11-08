<%@ taglib uri="/tags/svgembed" prefix="svg" %>


<%@ page import="com.sun.jbi.cam.common.GenericConstants,
   com.sun.jbi.cam.manager.framework.renderers.svg.JBIDeploymentDescriptorProcessor,
   com.sun.jbi.cam.manager.framework.generic.ServiceUnitsBean,
   java.util.List,
   com.sun.jbi.cam.model.management.JBIServiceUnitStatus" 
%>
   
<%
    String targetName = 
            request.getParameter(GenericConstants.COMPONENT_TNAME);
    String assemblyName = 
            request.getParameter(GenericConstants.SERVICE_ASSEMBLY_NAME);
    String isVisibleVal = (String)session.getAttribute("legendVisible");
    // default is visible if the attribute does not exist
    boolean isVisible = (isVisibleVal=="true" || isVisibleVal == null)
            ? true : false;
    ServiceUnitsBean susBean =  new ServiceUnitsBean();
    List<JBIServiceUnitStatus> suStatusList = 
            susBean.getSAServiceUnitStatusList(assemblyName,targetName);
    JBIDeploymentDescriptorProcessor ddp = new 
            JBIDeploymentDescriptorProcessor(isVisible,suStatusList,targetName);
    String svgNewStr  = ddp.processJBIAssemblyDescriptors(assemblyName,targetName);
%>


<html>
<head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<SCRIPT LANGUAGE="JavaScript" TYPE="text/javascript">
<!--
// since we can create ActiveXObject inside the SVG script we
// call the following method from the SVG script and perform
//  the operation in the frame that hold the ASV
var xmlhttpobject;
var lastURL;

function retrieveStatisticContentsIE(url) {
    lastURL = url;
    if(lastURL=="") {
       return;
    } 

    xmlhttpobject = new ActiveXObject("Microsoft.XMLHTTP");
    if (xmlhttpobject) {
        xmlhttpobject.onreadystatechange = displayStatisticContentsIE;
        xmlhttpobject.open("GET", url, true);
        xmlhttpobject.send();
    }
}


function displayStatisticContentsIE() {		
    if (xmlhttpobject.readyState == 4) { // Complete
        if (xmlhttpobject.status == 200) { // OK response
            var svgObj = document.embeds[0].window;
            svgObj.parseResponse(xmlhttpobject.responseText);
            svgObj.positionStatisticTextBox();
            setTimeout('getnextstatisticSampleIE()',1000);
        } else {
                alert("Problem: " + xmlhttpobject.statusText);
        }
    }
}

function getnextstatisticSampleIE() { 
    retrieveStatisticContentsIE(lastURL);
};


//-->
</SCRIPT>
</head>
<body style="margin:0px;background-color:#FFFFFF;border-left:1px solid #003163;border-right:1px solid #003163;" >
    <!-- div id="svgdiv" style="width:10px;height:10px;background-color:transparent;" -->
        <svg:embed name="svg"  width="100%" height="100%">
            <%= svgNewStr %>
        </svg:embed>  
    <!-- /div -->

</body>
</html>
