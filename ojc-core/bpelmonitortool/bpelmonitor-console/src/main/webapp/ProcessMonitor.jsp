<?xml version="1.0" encoding="UTF-8"?>
<!-- 
    Document   : ProcessMonitor
    Created on : May 6, 2009, 12:05:23 PM
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
                <div style="left: 0px; top: 0px; position: absolute">
                    <jsp:directive.include file="TopFrame.jspf"/>
                </div>
                <div style="left: 0x; top: 110px; position: absolute">
                    <jsp:directive.include file="LeftNavigation.jspf"/>
                </div>
                <ice:form id="form1">
                    <ice:panelGroup id="panelGroup1" style="border: 1px solid rgb(204, 204, 204); height: 406px; left: 237px; top: 110px; position: absolute; width: 708px">
                        <ice:outputText id="outputText2" style="left: 192px; top: 72px; position: absolute; width: 166px" value="#{SessionBean1.selectedProcess.applicaitonName}"/>
                        <ice:outputLabel id="outputLabel1" style="left: 72px; top: 24px; position: absolute" value="Business Process:"/>
                        <ice:outputLabel id="outputLabel2" style="left: 48px; top: 72px; position: absolute" value="Composite application:"/>
                        <ice:outputLabel id="outputLabel3" style="left: 24px; top: 144px; position: absolute" value="Live Instances:"/>
                        <ice:panelGroup contextValue="liveInstances" panelTooltip="displayOptions">
                            <ice:outputText id="liveInstOutputText" style="left: 192px; top: 144px; position: absolute;" value="#{SessionBean1.selectedProcess.liveInstnces}"/>
                        </ice:panelGroup>
                        <ice:outputLabel id="outputLabel11" style="height: 21px; left: 25px; top: 240px; position: absolute; width: 140px" value="Completed Instances"/>
                        <ice:panelGroup contextValue="completedInstances" panelTooltip="displayOptions">
                            <ice:outputText id="completedInstancesText" style="left: 192px; top: 240px; position: absolute; width: 166px" value="#{SessionBean1.selectedProcess.completedInstances}"/>
                        </ice:panelGroup>
                        <ice:outputLabel id="outputLabel4" style="left: 24px; top: 168px; position: absolute" value="Suspended Instances:"/>
                        <ice:panelGroup contextValue="suspendedInstances" panelTooltip="displayOptions">
                            <ice:outputText id="outputText4" style="left: 192px; top: 168px; position: absolute;" value="#{SessionBean1.selectedProcess.suspendedInstances}"/>
                        </ice:panelGroup>
                        <ice:outputLabel id="outputLabel5" style="left: 24px; top: 192px; position: absolute" value="Faulted Instances:"/>
                        <ice:panelGroup contextValue="faultedInstances" panelTooltip="displayOptions">
                            <ice:outputText id="outputText5" style="left: 192px; top: 192px; position: absolute;" value="#{SessionBean1.selectedProcess.faultedInstances}"/>
                        </ice:panelGroup>
                        <ice:outputLabel id="outputLabel6" style="left: 24px; top: 216px; position: absolute" value="Terminated Instances:"/>
                        <ice:panelGroup contextValue="terminatedInstances" panelTooltip="displayOptions">
                            <ice:outputText id="outputText6" style="left: 192px; top: 216px; position: absolute;" value="#{SessionBean1.selectedProcess.terminatedInstances}"/>
                        </ice:panelGroup>
                        <ice:outputLabel id="outputLabel7" style="left: 26px; top: 276px; position: absolute" value="Last Instance Process Time"/>
                        <ice:outputText id="outputText1" style="left: 194px; top: 276px; position: absolute; width: 166px" value="#{SessionBean1.selectedProcess.lastInstanceProcessedTime}"/>
                        <ice:outputLabel id="outputLabel8" style="left: 24px; top: 312px; position: absolute" value="KPI"/>
                        <ice:outputText id="outputText7" style="left: 192px; top: 312px; position: absolute; width: 166px" value="#{SessionBean1.selectedProcess.kpi}"/>
                        <ice:outputLabel id="outputLabel9" style="left: 96px; top: 48px; position: absolute" value="Namespace:"/>
                        <ice:outputText id="outputText8" style="left: 192px; top: 48px; position: absolute; width: 478px" value="#{SessionBean1.selectedProcess.businessProcessQName.namespaceURI}"/>
                        <ice:outputText id="outputText9" style="left: 192px; top: 24px; position: absolute; width: 478px" value="#{SessionBean1.selectedProcess.businessProcessQName.localPart}"/>
                        <!-- toolTips -->
                        <ice:panelTooltip displayListener="#{ProcessMonitor.optionsListener}" hideOn="mousedown" hoverDelay="10" id="displayOptions" style="font-size: 11px; left: 0px; top: 0px; position: relative; height: 100px; width: 135px">
                            <f:facet name="header">
                                <ice:outputText value="#{ProcessMonitor.optionsHeader}"/>
                            </f:facet>
                            <f:facet name="body">
                                <ice:panelGroup style="padding: 2px;">
                                    <ice:commandLink actionListener="#{ProcessMonitor.commandLinkListener}" id="mostRecent" value="#{ProcessMonitor.mostRecentInstanceDisplay}"/>
                                    <br/>
                                    <ice:commandLink actionListener="#{ProcessMonitor.commandLinkListener}" id="oldest" value="#{ProcessMonitor.oldestInstanceDisplay}"/>
                                    <br/>
                                    <ice:commandLink actionListener="#{ProcessMonitor.commandLinkListener}" id="listAll" value="#{ProcessMonitor.allAvailableDisplay}"/>
                                    <br/>
                                    <ice:commandLink actionListener="#{ProcessMonitor.commandLinkListener}" id="search" value="#{ProcessMonitor.searchInstancesDisplay}"/>
                                </ice:panelGroup>
                            </f:facet>
                        </ice:panelTooltip>
                    </ice:panelGroup>
                    <ice:panelGroup id="panelGroup2" style="left: 237px; top: 521px; position: absolute; width: 100%">
                        <ice:panelTabSet id="panelTabSet1" tabPlacement="Top">
                            <ice:panelTab id="panelTab1" label="Graphical">
                                <ice:panelLayout id="panelLayout1">
                                    <ice:outputMedia>
                                        <f:param name="src" value="#{SVG.fileName}"/>
                                        <f:param name="type" value="image/svg+xml"/>
                                        <f:param name="height" value="#{SVG.height}"/>
                                        <f:param name="width" value="#{SVG.width}"/>
                                        <f:param name="pluginspage" value="http://www.adobe.com/svg/viewer/install/"/>
                                    </ice:outputMedia>
                                </ice:panelLayout>
                            </ice:panelTab>
                            <ice:panelTab id="panelTab2" label="Textual">
                                <ice:panelLayout id="panelLayout2" style="width: 126; height: 122;"/>
                            </ice:panelTab>
                        </ice:panelTabSet>
                    </ice:panelGroup>
                </ice:form>
            </body>
        </html>
    </f:view>
</jsp:root>
