<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <webuijsf:form style="margin-left:10px;margin-right:10px;" >

                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="#{msgs.generic_Statistics}" />

                    <table>
                      <tr>
                        <td align="left" valign="middle" width="90%" >
                            <webuijsf:contentPageTitle id="pagetitle" title="#{StatisticsBean.title}" helpText="#{msgs.statistics_helpInline}" />
                        </td>
                        <td align="right" valign="middle" width="10%">
                            <webuijsf:button id="buttonGraph" text="#{msgs.generic_showGraph}" actionExpression="#{StatisticsBean.showGraph}" />
                        </td>
                      </tr>
                    </table>

                    <br></br>
      
                    <webuijsf:table id="table1" > 

                        <!-- Title -->
                        <f:facet name="title">
                         <webuijsf:staticText text="#{StatisticsBean.tableTitle}"/>
                        </f:facet>

                        <webuijsf:tableRowGroup id="rowHeaderDef"
                            sourceData="#{StatisticsBean.provisioningStatistics}" sourceVar="stats" 
                            collapsed="true" >

                            <webuijsf:tableColumn id="endPoint"
                                headerText="#{msgs.statistics_endpoint}" rowHeader="true" >
                                <webuijsf:staticText text="#{stats.value.name}"/>
                            </webuijsf:tableColumn>

                            <webuijsf:tableColumn id="receivedRequests"
                                headerText="#{msgs.statistics_receivedRequests}" rowHeader="true">
                                <webuijsf:staticText text="#{stats.value.value}"/>
                            </webuijsf:tableColumn>
                            
                            <webuijsf:tableColumn id="receivedReplies" headerText="#{msgs.statistics_receivedReplies}" rowHeader="true">
                                <webuijsf:staticText text="#{stats.value.receivedReplies}"/>
                            </webuijsf:tableColumn>
                            
                            <webuijsf:tableColumn id="receivedErrors" headerText="#{msgs.statistics_receivedErrors}" rowHeader="true">
                                <webuijsf:staticText text="#{stats.value.receivedErrors}"/>
                            </webuijsf:tableColumn>
                            
                            <webuijsf:tableColumn id="receivedDones" headerText="#{msgs.statistics_receivedDones}" rowHeader="true">
                                <webuijsf:staticText text="#{stats.value.receivedDones}"/>
                            </webuijsf:tableColumn>
                            
                            <webuijsf:tableColumn id="sentRequests"
                                headerText="#{msgs.statistics_sentRequests}" rowHeader="true">
                                <webuijsf:staticText text="#{stats.value.sentRequests}"/>
                            </webuijsf:tableColumn>
                            
                            <webuijsf:tableColumn id="sentReplies" headerText="#{msgs.statistics_sentReplies}" rowHeader="true">
                                <webuijsf:staticText text="#{stats.value.sentReplies}"/>
                            </webuijsf:tableColumn>
                            
                            <webuijsf:tableColumn id="sentErrors" headerText="#{msgs.statistics_sentErrors}" rowHeader="true">
                                <webuijsf:staticText text="#{stats.value.sentErrors}"/>
                            </webuijsf:tableColumn>
                            
                            <webuijsf:tableColumn id="sentDones" headerText="#{msgs.statistics_sentDones}" rowHeader="true">
                                <webuijsf:staticText text="#{stats.value.sentDones}"/>
                            </webuijsf:tableColumn>
                           
                        </webuijsf:tableRowGroup>

                        <webuijsf:tableRowGroup id="provisioningGroup" sourceData="#{StatisticsBean.provisioningStatistics}" sourceVar="stats" 
                          groupToggleButton="true">

                           <!-- Row group header -->
                           <f:facet name="header">
                            <webuijsf:panelGroup id="groupHeader">
                              <webuijsf:markup tag="span" extraAttributes="class='TblGrpLft'">
                                <webuijsf:staticText styleClass="TblGrpTxt" text="#{msgs.statistics_provisioningEndpoint}"/>
                              </webuijsf:markup>
                              <webuijsf:markup tag="span" extraAttributes="class='TblGrpRt'">
                                <webuijsf:staticText styleClass="TblGrpMsgTxt" text=""/>
                              </webuijsf:markup>
                            </webuijsf:panelGroup>
                           </f:facet>  

                          <webuijsf:tableColumn id="endpoint"
                            extraHeaderHtml="nowrap='nowrap'"
                            rowHeader="true" >
                            <webuijsf:staticText text="#{stats.value.endpointShort}" toolTip="#{stats.value.namespace}" />
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedRequests" >
                            <webuijsf:staticText text="#{stats.value.receivedRequests}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedReplies" >
                            <webuijsf:staticText text="#{stats.value.receivedReplies}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedErrors" >
                            <webuijsf:staticText text="#{stats.value.receivedErrors}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedDones" >
                            <webuijsf:staticText text="#{stats.value.receivedDones}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentRequests">
                            <webuijsf:staticText text="#{stats.value.sentRequests}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentReplies" >
                            <webuijsf:staticText text="#{stats.value.sentReplies}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentErrors" >
                            <webuijsf:staticText text="#{stats.value.sentErrors}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentDones" >
                            <webuijsf:staticText text="#{stats.value.sentDones}"/>
                          </webuijsf:tableColumn>
                        </webuijsf:tableRowGroup>

                        <webuijsf:tableRowGroup id="consumingGroup"
                          
                          sourceData="#{StatisticsBean.consumingStatistics}" sourceVar="stats"
                          groupToggleButton="true">

                          <!-- Row group header -->
                          <f:facet name="header">
                            <webuijsf:panelGroup id="groupHeader">
                              <webuijsf:markup tag="span" extraAttributes="class='TblGrpLft'">
                                <webuijsf:staticText styleClass="TblGrpTxt" text="#{msgs.statistics_consumingEndpoint}"/>
                              </webuijsf:markup>
                              <webuijsf:markup tag="span" extraAttributes="class='TblGrpRt'">
                                <webuijsf:staticText styleClass="TblGrpMsgTxt" text=""/>
                              </webuijsf:markup>
                            </webuijsf:panelGroup>
                          </f:facet>
                          
                          <webuijsf:tableColumn id="endpoint"
                            extraHeaderHtml="nowrap='nowrap'" >
                            <webuijsf:staticText text="#{stats.value.endpointShort}" toolTip="#{stats.value.namespace}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedRequests" >
                            <webuijsf:staticText text="#{stats.value.receivedRequests}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedReplies" >
                            <webuijsf:staticText text="#{stats.value.receivedReplies}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedErrors" >
                            <webuijsf:staticText text="#{stats.value.receivedErrors}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedDones" >
                            <webuijsf:staticText text="#{stats.value.receivedDones}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentRequests">
                            <webuijsf:staticText text="#{stats.value.sentRequests}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentReplies" >
                            <webuijsf:staticText text="#{stats.value.sentReplies}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentErrors" >
                            <webuijsf:staticText text="#{stats.value.sentErrors}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentDones" >
                            <webuijsf:staticText text="#{stats.value.sentDones}"/>
                          </webuijsf:tableColumn>
                        </webuijsf:tableRowGroup>

                        <webuijsf:tableRowGroup id="totalsGroup" groupToggleButton="true"
                          sourceData="#{StatisticsBean.totalsStatistics}" sourceVar="stats" >
                          <!-- Row group header -->
                          <f:facet name="header">
                            <webuijsf:panelGroup id="totalsHeader">
                              <webuijsf:markup tag="span" extraAttributes="class='TblGrpLft'">
                                <webuijsf:staticText styleClass="TblGrpTxt" text="#{msgs.statistics_totals}"/>
                              </webuijsf:markup>
                              <webuijsf:markup tag="span" extraAttributes="class='TblGrpRt'">
                                <webuijsf:staticText styleClass="TblGrpMsgTxt" text=""/>
                              </webuijsf:markup>
                            </webuijsf:panelGroup>
                          </f:facet>

                          <webuijsf:tableColumn id="endpoint"
                            extraHeaderHtml="nowrap='nowrap'" >
                            <webuijsf:staticText text="#{stats.value.endpoint}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedRequests" >
                            <webuijsf:staticText text="#{stats.value.receivedRequests}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedReplies" >
                            <webuijsf:staticText text="#{stats.value.receivedReplies}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedErrors" >
                            <webuijsf:staticText text="#{stats.value.receivedErrors}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="receivedDones" >
                            <webuijsf:staticText text="#{stats.value.receivedDones}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentRequests">
                            <webuijsf:staticText text="#{stats.value.sentRequests}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentReplies" >
                            <webuijsf:staticText text="#{stats.value.sentReplies}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentErrors" >
                            <webuijsf:staticText text="#{stats.value.sentErrors}"/>
                          </webuijsf:tableColumn>
                          <webuijsf:tableColumn id="sentDones" >
                            <webuijsf:staticText text="#{stats.value.sentDones}"/>
                          </webuijsf:tableColumn>

                        </webuijsf:tableRowGroup>


                    </webuijsf:table>

               </webuijsf:form>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>
