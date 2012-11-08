<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <webuijsf:head title="Composite Application Manager" />
                <webuijsf:frameSet id="bpInstancesSvgFrameSet"  rows="50%,*"  >
                     <webuijsf:frame name="svgFrame" frameBorder="false" url="/faces/tabs/svg.jsp"  noResize="false"/>
                     <webuijsf:frame name="instancesFrame" toolTip="topFrame" url="/faces/tabs/instancesTable.jsp" frameBorder="false" noResize="true" scrolling="false" />
                </webuijsf:frameSet>

                
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>


<!--
<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="#{msgs.generic_Statistics}" />
                <webuijsf:panelGroup id="myPanelGroup1">
                    <webuijsf:table id="bpInstances_table" binding="#{BPInstancesBean.table}"/>
                </webuijsf:panelGroup>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>
-->
