<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>

            <webuijsf:form id="form1"> 

                <f:loadBundle basename="com.sun.jbi.cam.plugins.aspects.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="Composite Application Manager" />

                <webuijsf:tabSet id="tabs" selected="tabBlank" binding="#{TabsBean.tabSet}" >
                    
                    <!--
                    <webuijsf:tab id="tabBlank" text="" />
                    <webuijsf:tab id="tabStatistics" text="#{msgs.generic_Statistics}" 
                         actionExpression="#{TabsBean.tab1Clicked}" />
                    <webuijsf:tab id="tabControl" text="#{msgs.generic_Control}"  
                         actionExpression="#{TabsBean.tab2Clicked}" />
                    <webuijsf:tab id="tabConfiguration" text="#{msgs.generic_Configuration}" 
                         actionExpression="#{TabsBean.tab3Clicked}" />
                    -->
                    
                </webuijsf:tabSet >                
                
                <!-- code for iframe
                <p align="center"> 
                    <strong><webuijsf:staticText escape="false" text="#{TabsBean.message}" /></strong> 
                </p>                 
                <webuijsf:iframe url="#{TabsBean.url}" height="800px" width="100%"/>
                -->
                
            </webuijsf:form> 

            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>


