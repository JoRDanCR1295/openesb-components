<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <webuijsf:head title="Composite Application Manager" />
                <webuijsf:frameSet id="topFrame"  rows="105,*" >
                     <webuijsf:frame name="bannerFrame" toolTip="topFrame" url="/faces/manager/framework/core/pages/banner.jsp" frameBorder="false" noResize="true" scrolling="no"/>
                     <webuijsf:frameSet id="bottomFrame" cols="30%,*" >
                         <webuijsf:frame name="treeFrame" url="/faces/manager/framework/core/pages/tree.jsp" frameBorder="true" noResize="false" scrolling="auto"/>
                         <webuijsf:frame name="workspaceFrame" url="/faces/manager/framework/commonTasks.jsp" frameBorder="true" noResize="false" scrolling="auto"/>
                     </webuijsf:frameSet>
                </webuijsf:frameSet>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>
