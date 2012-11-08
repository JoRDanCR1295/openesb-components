<?xml version="1.0" encoding="UTF-8"?>
<!-- 
    Document   : Instances
    Created on : May 8, 2009, 2:06:21 PM
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
        <div style="left: 0px; top: 110px; position: absolute">
            <jsp:directive.include file="LeftNavigation.jspf"/>
        </div>
        <ice:form id="form1">
        <ice:panelGroup id="panelGroup1" style="border-width: 1px; border-color: rgb(204, 204, 204); border-left-style: solid; border-top-style: solid; height: 704px; left: 237px; top: 110px; position: absolute; width: 1100px">
        <ice:panelGroup>
        <ice:selectOneMenu id="selectBusinessProcess" partialSubmit="true" style="left: 166px; top: 22px; position: absolute; width: 312px"
                           value="#{Instances.selectOneMenu1Bean.selectedInt}" valueChangeListener="#{Instances.selectBusinessProcess_processValueChange}">
            <f:selectItems id="selectOneMenu1selectItems" value="#{Instances.selectOneMenu1DefaultItems}"/>
        </ice:selectOneMenu>
        <ice:commandLink action="#{Instances.processHomeLink_action}" id="processHomeLink"
                         style="color: blue; font-size: 12px; left: 486px; top: 27px; position: absolute; width: 119px" value="Process Home" visible="true"/>
        <ice:outputLabel id="outputLabel1" style="font-weight: bold; left: 48px; top: 20px; position: absolute; width: 117px" value="Business Process:"/>
        <ice:outputLabel id="outputLabel2" style="height: 28px; left: 82px; top: 48px; position: absolute; width: 77px" value="Namespace:"/>

        <ice:outputText id="bpNameAsString" style="left: 168px; top: 48px; position: absolute; width: 478px" value="#{SessionBean1.instanceFilter.bpName.namespaceURI}"/>
        <ice:selectOneMenu id="instanceFilterSelectMenu" partialSubmit="true"
                           style="height: 24px; left: 22px; top: 118px; position: absolute; width: 170px" value="#{Instances.selectOneMenu2Bean.selectedItem}">
            <f:selectItems id="selectOneMenu2selectItems" value="#{Instances.selectOneMenu2DefaultItems}"/>
        </ice:selectOneMenu>
        <ice:selectOneMenu id="instanceStatusFilterMenu" partialSubmit="true"
                           style="height: 24px; left: 200px; top: 118px; position: absolute; width: 170px" value="#{Instances.selectOneMenu3Bean.selectedItem}" valueChangeListener="#{Instances.instanceStatusFilterMenu_processValueChange}">
            <f:selectItems id="selectOneMenu3selectItems" value="#{Instances.selectOneMenu3DefaultItems}"/>
        </ice:selectOneMenu>
        <ice:commandButton id = "suspendAll" value="Suspend All" style="left: 400px; top: 118px; position: absolute;"
                           actionListener="#{Instances.suspendResumeTerminateAll_processAction}">
        </ice:commandButton>
        <ice:commandButton id = "resumeAll" value="Resume All" style="left: 500px; top: 118px; position: absolute;"
                           actionListener="#{Instances.suspendResumeTerminateAll_processAction}">
        </ice:commandButton>
        <ice:commandButton id = "terminateAll" value="Terminate All" style="left: 600px; top: 118px; position: absolute;"
                           actionListener="#{Instances.suspendResumeTerminateAll_processAction}">
        </ice:commandButton>

        <script>
            function toggleSelectAllRows() {
                var rows = document.getElementById('form1:instancesDataTable').rows;
                var currentState = rows[0].cells[0].childNodes[0].checked;
                //alert(currentState);
                var i;
                for (i in rows) {
                    i++;
                    if (currentState) {
                        rows[i].cells[0].childNodes[0].checked=true;
                    } else if (!currentState) {
                        rows[i].cells[0].childNodes[0].checked=false;
                    }
                }
            }

            function resetSelectAll() {
                //alert('test');
                document.getElementById('form1:instancesDataTable').rows[0].cells[0].childNodes[0].checked =false;
            }
        </script>

        <ice:dataTable id="instancesDataTable" rows="10" style="left: 15px; top: 136px; position: absolute; width: 1100px"
                       value="#{Instances.dataTable1CachedRowSetWrapperDataModel}" var="currentRow">
        <ice:column id="column0">
            <f:facet name="header">
                <ice:selectBooleanCheckbox id="selectBoxHeader" onclick="toggleSelectAllRows();"/>
            </f:facet>
            <ice:selectBooleanCheckbox id="selectBox" immediate="true" value="#{currentRow['SELECTED']}"></ice:selectBooleanCheckbox>
        </ice:column>

        <ice:column id="column1">
            <ice:panelGroup contextValue="engineId" >
                <ice:outputText id="outputText1" value="#{currentRow['ENGINEID']}"/>
            </ice:panelGroup>
            <f:facet name="header">
                <ice:outputText id="outputText2" value="ENGINEID"/>
            </f:facet>
            <ice:rowSelector id="rowSelector2"  immediate="true" toggleOnInput="false"
                             selectionListener="#{Instances.rowSelector2_processAction}"/>
        </ice:column>
        <f:facet name="header"/>
        <ice:column id="column2">
            <ice:outputText id="outputText3" value="#{currentRow['INSTANCEID']}"/>
            <f:facet name="header">
                <ice:outputText id="outputText4" value="INSTANCEID"/>
            </f:facet>
        </ice:column>
        <f:facet name="header"/>
        <ice:column id="column5">
            <ice:outputText id="outputText9" value="#{currentRow['STARTTIME']}"/>
            <f:facet name="header">
                <ice:outputText id="outputText10" value="STARTTIME"/>
            </f:facet>
        </ice:column>
        <ice:column id="column6">
            <ice:outputText id="outputText11" value="#{currentRow['ENDTIME']}"/>
            <f:facet name="header">
                <ice:outputText id="outputText12" value="ENDTIME"/>
            </f:facet>
        </ice:column>
        <ice:column id="column7">
            <ice:outputText id="outputText13" value="#{currentRow['UPDATEDTIME']}"/>
            <f:facet name="header">
                <ice:outputText id="outputText14" value="UPDATEDTIME"/>
            </f:facet>
        </ice:column>
        <ice:column id="column4">
            <ice:outputText id="outputText7" value="#{currentRow['STATUS']}"/>
            <f:facet name="header">
                <ice:outputText id="outputText8" value="STATUS"/>
            </f:facet>
        </ice:column>

        <f:facet name="footer">
        <ice:panelGroup id="panelGroup1a" style="display: block; text-align: left" >
        <ice:commandButton id = "suspend" value="Suspend Selected" actionListener="#{Instances.suspendResumeTerminate_processAction}"/>
        <ice:commandButton id = "resume" value="Resume Selected" actionListener="#{Instances.suspendResumeTerminate_processAction}"/>
        <ice:commandButton id = "terminate" value="Terminate Selected" actionListener="#{Instances.suspendResumeTerminate_processAction}"/>
        <ice:panelGroup id="panelGroup1b" style="display: block; text-align: right" >
        <!-- Display counts about the table and the currently displayed page -->
                 <ice:dataPaginator id="dataScroll_2" for="instancesDataTable"

                                               rowsCountVar="rowsCount"
                                               displayedRowsCountVar="displayedRowsCount"
                                               firstRowIndexVar="firstRowIndex"
                                               lastRowIndexVar="lastRowIndex"
                                               pageCountVar="pageCount"
                           pageIndexVar="pageIndex">
            <ice:outputFormat
                value=" {0} instances found, displaying {1} instance(s), from {2} to {3}. Page {4} / {5}."
                styleClass="standard">
            <f:param value="#{rowsCount}"/>
            <f:param value="#{displayedRowsCount}"/>
            <f:param value="#{firstRowIndex}"/>
            <f:param value="#{lastRowIndex}"/>
            <f:param value="#{pageIndex}"/>
            <f:param value="#{pageCount}"/>
            </ice:outputFormat>
        </ice:dataPaginator>
        </ice:panelGroup>
        </ice:panelGroup>
        </f:facet>
        </ice:dataTable>
        <!-- Paginator with page controls -->
                <ice:dataPaginator id="dataScroll_3"
                style="left: 780px; top: 116px; position: absolute;"
                                   for="instancesDataTable"
                                   paginator="true"
                                   fastStep="3"
                           paginatorMaxPages="5">
            <f:facet name="first">
                <ice:graphicImage onclick="resetSelectAll();"
                    value="/resources/icons/arrow-first.gif"
                    style="border:none;"
                    title="First Page"/>
            </f:facet>
            <f:facet name="last">
                <ice:graphicImage onclick="resetSelectAll();"
                    value="/resources/icons/arrow-last.gif"
                    style="border:none;"
                    title="Last Page"/>
            </f:facet>
            <f:facet name="previous">
                <ice:graphicImage onclick="resetSelectAll();"
                    value="/resources/icons/arrow-previous.gif"
                    style="border:none;"
                    title="Previous Page"/>
            </f:facet>
            <f:facet name="next">
                <ice:graphicImage onclick="resetSelectAll();"
                    value="/resources/icons/arrow-next.gif"
                    style="border:none;"
                    title="Next Page"/>
            </f:facet>
            <f:facet name="fastforward">
                <ice:graphicImage onclick="resetSelectAll();"
                                    value="/resources/icons/arrow-ff.gif"
                                  style="border:none;"
                                  title="Fast Forward"/>
            </f:facet>
            <f:facet name="fastrewind">
                <ice:graphicImage
                            onclick="resetSelectAll();"
                            value="/resources/icons/arrow-fr.gif"
                                  style="border:none;"
                                  title="Fast Backwards"/>
            </f:facet>
        </ice:dataPaginator>





        </ice:panelGroup>
        <ice:panelGroup id="panelGroup2" style="top:520px; left: 0px; position: relative; width: 100%">
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
        </ice:panelGroup>
        <!-- toolTips -->
                    <!-- NOTE: Wanted to implement tooltip such that when user moves mouse over
                     a row, a popup would display the variable data for the instance. However,
                     could not find mouse over event that would convey the row info to the backend.
                     hence the following code is not used, but can be used if the above is resolved -->
                    <ice:panelTooltip hideOn="mousedown"
                            hoverDelay="10"
                            id="displayOptions"
                          style="margin: 0px; padding: 0px; font-size: 10px; left: 0px; top: 0px; position: relative;">
            <f:facet name="body">
                <ice:panelGroup style="margin: 0px; padding: 0px;">
                    <ice:dataTable id="variablesDataTable" value="#{Instances.variablesDataTableModel}" var="currentRow" width="400px">
                        <ice:column id="column8">
                            <ice:outputText id="outputText15" style="font-size: 10px;" value="#{currentRow['VARNAME']}"/>
                            <f:facet name="header">
                                <ice:outputText id="outputText16" style="font-size: 10px;" value="Var Name"/>
                            </f:facet>
                        </ice:column>
                        <ice:column id="column9">
                            <ice:outputText id="outputText17" style="font-size: 10px;" value="#{currentRow['VARVALUE']}"/>
                            <f:facet name="header">
                                <ice:outputText id="outputText18" style="font-size: 10px;" value="Scope"/>
                            </f:facet>
                        </ice:column>
                    </ice:dataTable>
                </ice:panelGroup>
            </f:facet>
        </ice:panelTooltip>

        <ice:panelPopup  id="popup" modal="false" visible="#{Instances.showVariableDataPopup}"
                         draggable="true"
                         style="width: 40%; margin: 0px; padding: 0px; font-size: 10px; left: 300px; top: 300px; position: relative;"
                         resizable="true">
            <f:facet name="body">
                <ice:panelGroup>
                    <ice:dataTable id="variablesDataTable1" value="#{Instances.variableListDataModel}" var="entry" width="100%">
                        <ice:column id="column10" style="font-size: 10px; width:30%">
                            <ice:outputText id="outputText19" style="font-size: 10px;" value="#{entry.variableName}"/>
                            <f:facet name="header">
                                <ice:outputText id="outputText20" style="font-size: 10px;" value="Variable Name"/>
                            </f:facet>
                        </ice:column>
                        <ice:column id="column11">
                            <ice:outputText id="outputText21" style="font-size: 10px;" value="#{entry.variableValue}"/>
                            <f:facet name="header">
                                <ice:outputText id="outputText22" style="font-size: 10px;" value="Variable Value"/>
                            </f:facet>
                        </ice:column>
                    </ice:dataTable>
                    <ice:commandButton id="modalPnlCloseButton" type="submit"
                                       value="Close"
                                       actionListener="#{Instances.closePopup}"/>
                </ice:panelGroup>
            </f:facet>
        </ice:panelPopup>
        </ice:form>

    </body>
</html>
</f:view>
</jsp:root>
