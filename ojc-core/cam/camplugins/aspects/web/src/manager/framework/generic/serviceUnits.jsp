<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <f:loadBundle basename="com.sun.jbi.cam.plugins.aspects.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="#{msgs.generic_ServiceUnits}" />
                
                <webuijsf:contentPageTitle id="pagetitle" title="#{ServiceUnitsBean.title}" helpText="" />    
                <br></br>
                
                <webuijsf:table id="table1" style="margin-left:10px;margin-right:10px;">
                    
                        <!-- Title -->
                        <f:facet name="title">
                         <webuijsf:staticText text="#{ServiceUnitsBean.tableTitle}"/>
                        </f:facet>                    
                    
                    <webuijsf:tableRowGroup id="rowGroup1"
                            sourceData="#{ServiceUnitsBean.list}" sourceVar="su">
                
                        <webuijsf:tableColumn id="name"
                            headerText="#{msgs.serviceUnits_name}" rowHeader="true">
                            <webuijsf:hyperlink text="#{su.value.name}" url="#{su.value.url}" target="workspaceFrame" />
                        </webuijsf:tableColumn>
                        <webuijsf:tableColumn id="desc" headerText="#{msgs.serviceUnits_desc}" 
                            rowHeader="true">
                            <webuijsf:staticText text="#{su.value.desc}"/>
                        </webuijsf:tableColumn>

                        <webuijsf:tableColumn id="status" headerText="#{msgs.serviceUnits_status}" 
                            rowHeader="true">
                            <webuijsf:staticText text="#{su.value.status}"/>
                        </webuijsf:tableColumn>
                        
                    </webuijsf:tableRowGroup>
                </webuijsf:table>                        
                    
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>


