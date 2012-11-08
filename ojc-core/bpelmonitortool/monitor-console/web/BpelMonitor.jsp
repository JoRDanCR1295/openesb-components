<?xml version="1.0" encoding="UTF-8"?>
<!-- 
    Document   : BPELMonitor1
    Created on : May 7, 2009, 1:32:53 PM
    Author     : mbhasin
-->
<jsp:root version="2.1" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:ice="http://www.icesoft.com/icefaces/component" xmlns:jsp="http://java.sun.com/JSP/Page">
    <jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
    <f:view>
        <html id="outputHtml1">
            <head id="outputHead1">
                <ice:outputStyle href="./resources/stylesheet.css" id="outputStyle1"/>
                <ice:outputStyle href="./xmlhttp/css/xp/xp.css" id="outputStyle2"/>
            </head>
            <body id="outputBody1" style="-rave-layout: grid">
                <div style="left: 0px; top: 110px; position: absolute">
                    <jsp:directive.include file="LeftNavigation.jspf"/>
                </div>
                <div style="left: 0px; top: 0px; position: absolute">
                    <jsp:directive.include file="TopFrame.jspf"/>
                </div>
                <ice:form id="form1">
                    <ice:panelGroup id="panelGroup1" style="border-width: 1px; border-color: rgb(204, 204, 204); border-left-style: solid; border-top-style: solid; height: 704px; left: 237px; top: 110px; position: absolute; width: 1100px">
                        <ice:dataTable id="dataTable1" rows="10" value="#{BpelMonitor.dataTable1Model}" var="entry">
                            <ice:column id="column1">
                                <ice:outputText id="linkAction1" value="#{entry.applicaitonName}"/>
                                <f:facet name="header">
                                    <ice:outputText id="outputText2" value="Application Name"/>
                                </f:facet>
                                <ice:rowSelector id="rowSelector1" selectionListener="#{BpelMonitor.rowSelector1_processAction}"/>
                            </ice:column>
                            <ice:column id="column2">
                                <ice:outputText id="linkAction2" value="#{entry.businessProcessName}"/>
                                <f:facet name="header">
                                    <ice:outputText id="outputText4" value="Business Process"/>
                                </f:facet>
                            </ice:column>
                            <ice:column id="column4">
                                <ice:outputText id="outputText1" value="#{entry.liveInstnces}"/>
                                <f:facet name="header">
                                    <ice:outputText id="outputText3" value="Live Instances"/>
                                </f:facet>
                            </ice:column>
                            <ice:column id="column3">
                                <ice:outputText id="outputText5" value="#{entry.lastInstanceProcessedTime}"/>
                                <f:facet name="header">
                                    <ice:outputText id="outputText6" value="Last Instance Process Time"/>
                                </f:facet>
                            </ice:column>
                            <ice:column id="column7">
                                <ice:outputText id="outputText11" value="#{entry.kpi}"/>
                                <f:facet name="header">
                                    <ice:outputText id="outputText12" value="KPI"/>
                                </f:facet>
                            </ice:column>
                            <ice:column id="column5">
                                <ice:outputText id="outputText7" value="#{entry.suspendedInstances}"/>
                                <f:facet name="header">
                                    <ice:outputText id="outputText8" value="Suspended Instances"/>
                                </f:facet>
                            </ice:column>
                            <f:facet name="header"/>
                            <ice:column id="column6">
                                <ice:outputText id="outputText9" value="#{entry.faultedInstances}"/>
                                <f:facet name="header">
                                    <ice:outputText id="outputText10" value="Faulted Instances"/>
                                </f:facet>
                            </ice:column>
                            <f:facet name="footer"/>
                            <ice:column id="column8">
                                <ice:outputText id="outputText13" value="#{entry.terminatedInstances}"/>
                                <f:facet name="header">
                                    <ice:outputText id="outputText14" value="Terminated Instances"/>
                                </f:facet>
                            </ice:column>
                            <f:facet name="footer">
                                <ice:panelGroup id="panelGroup2" style="display: block; text-align: center" styleClass="list-paging-footer">
                                    <ice:commandButton action="#{BpelMonitor.dataTable1_firstPageAction}" id="dataTable1FooterFirstButton" immediate="true" value="|&lt;"/>
                                    <ice:commandButton action="#{BpelMonitor.dataTable1_previousPageAction}" id="dataTable1FooterPreviousButton"
                                        immediate="true" value="&lt;-"/>
                                    <ice:commandButton action="#{BpelMonitor.dataTable1_nextPageAction}" id="dataTable1FooterNextButton" immediate="true" value="-&gt;"/>
                                    <ice:commandButton action="#{BpelMonitor.dataTable1_lastPageAction}" id="dataTable1FooterLastButton" immediate="true" value="&gt;|"/>
                                </ice:panelGroup>
                            </f:facet>
                        </ice:dataTable>
                    </ice:panelGroup>
                </ice:form>
            </body>
        </html>
    </f:view>
</jsp:root>
