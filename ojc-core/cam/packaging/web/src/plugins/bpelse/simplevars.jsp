<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" 
xmlns:h="http://java.sun.com/jsf/html" 
xmlns:jsp="http://java.sun.com/JSP/Page" 
xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                <f:loadBundle basename="com.sun.jbi.cam.plugins.bpelse.table.util.Resources" var="bptablemsgs" />
                
                 <webuijsf:head title="BPEL Instances">
                      <webuijsf:script url="js/select.js"/>
                      <webuijsf:script url="js/filter.js"/>
                 </webuijsf:head>
                 
                 <webuijsf:form id="form1">
                    <webuijsf:contentPageTitle id="pagetitle" title="BPEL Instances" helpText="Table below shows a list of Business Process Instances" />
                    <br />
                    <div class="ConMgn"> 
                        
              <!-- Instances Table -->
              <webuijsf:table id="table1"
                deselectMultipleButton="true"
                deselectMultipleButtonOnClick="setTimeout('disableActions()', 0)"
                paginateButton="true"
                paginationControls="true"
                selectMultipleButton="true"
                selectMultipleButtonOnClick="setTimeout('disableActions()', 0)"
                title="BPEL Instances">
                <webuijsf:tableRowGroup id="rowGroup1"
                  binding="#{state.groupA.tableRowGroup}"
                  rows="5"
                  selected="#{state.groupA.select.selectedState}"
                  sourceData="#{state.groupA.numericVars}"
                  sourceVar="var">
                  <webuijsf:tableColumn id="col0"
                    selectId="select"
                    sort="#{state.groupA.select.selectedState}">
                    <webuijsf:checkbox id="select"
                      onClick="setTimeout('initAllRows(); disableActions()', 0)"
                      selected="#{state.groupA.select.selected}"
                      selectedValue="#{state.groupA.select.selectedValue}"/>
                  </webuijsf:tableColumn>
                  <webuijsf:tableColumn id="col1" 
                    alignKey="id" headerText="BP Id" rowHeader="true">
                    <webuijsf:staticText text="#{var.value.bpid}"/>
                  </webuijsf:tableColumn>
                  <webuijsf:tableColumn id="col2" alignKey="varid" headerText="Var Id">
                    <webuijsf:staticText text="#{var.value.id}"/>
                  </webuijsf:tableColumn>
                  <webuijsf:tableColumn id="col3" alignKey="value" headerText="Value">
                    <webuijsf:staticText text="#{var.value.numericValue}"/>
                  </webuijsf:tableColumn>
                </webuijsf:tableRowGroup>

                <!--
                <f:facet name="actionsTop">
                  <f:subview id="actionsTop">
                    <jsp:include page="actionsTop.jsp"/>
                  </f:subview>
                </f:facet>
                <f:facet name="actionsBottom">
                  <f:subview id="actionsBottom">
                    <jsp:include page="actionsBottom.jsp"/>
                  </f:subview>
                </f:facet>
                -->
                
                <!-- Filter -->
                <f:facet name="filter">
                  <webuijsf:dropDown submitForm="true" id="filter"
                    actionExpression="#{state.groupAFilter.applyBasicFilter}" 
                    items="#{state.groupAFilter.filterOptions}"
                    onChange="if (filterMenuChanged() == false) return false"
                    selected="#{state.groupAFilter.basicFilter}"/>
                </f:facet>
                <!-- Filter Panel -->
                <f:facet name="filterPanel">
                  <f:subview id="filterPanel">
                    <jsp:include page="filterPanel.jsp"/>
                  </f:subview>
                </f:facet>                
              </webuijsf:table>

                        <!--
                        <webuijsf:table id="table1" binding="#{state.table}" 
                            filterPanelFocusId="form1:table1:filterPanel:customFilter_field"
                            filterText="#{state.groupA.filter.filterText}"
                            width="95%">
                      
                            <f:facet name="filter">
                              <webuijsf:dropDown submitForm="true" id="filter"
                                actionExpression="#{state.groupA.filter.applyBasicFilter}" 
                                items="#{state.groupA.filter.filterOptions}"
                                onChange="if (filterMenuChanged() == false) return false"
                                selected="#{state.groupA.filter.basicFilter}"/>
                            </f:facet>
                            
                      
                            <f:facet name="actionsTop">
                              <f:subview id="actionsTop">
                                <jsp:include page="actionsTop.jsp"/>
                              </f:subview>
                            </f:facet>
                      
                            <f:facet name="filterPanel">
                              <f:subview id="filterPanel">
                                <jsp:include page="filterPanel.jsp"/>
                              </f:subview>
                            </f:facet>
                      </webuijsf:table>
                      -->

                    </div>
                </webuijsf:form>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>
