<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>

            <webuijsf:form id="form1"> 

                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="Composite Application Manager" />

                <webuijsf:tabSet id="tabs" selected="tabBlank" binding="#{AspectsTabsBean.tabSet}"  >
                    
                    <!---
                    <webuijsf:tab id="tabBlank" text="" url="http://www.sun.com" target="tabBottomFrame" />
                    <webuijsf:tab id="tabStatistics" text="#{msgs.generic_Statistics}" url="/faces/manager/framework/generic/statistics.jsp" target="tabBottomFrame" />
                    <webuijsf:tab id="tabControl" text="#{msgs.generic_Control}"  url="/faces/manager/framework/generic/control.jsp" target="tabBottomFrame" />
                    <webuijsf:tab id="tabConfiguration" text="#{msgs.generic_Configuration}" url="/faces/manager/framework/generic/configuration.jsp" target="tabBottomFrame" />
                    -->
                    
                </webuijsf:tabSet >                

            </webuijsf:form> 

            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>

