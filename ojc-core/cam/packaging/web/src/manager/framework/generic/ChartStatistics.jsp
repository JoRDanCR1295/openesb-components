<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" 
          xmlns:f="http://java.sun.com/jsf/core" 
          xmlns:h="http://java.sun.com/jsf/html" 
          xmlns:jsp="http://java.sun.com/JSP/Page" 
          xmlns:webuijsf="http://www.sun.com/webui/webuijsf"
          xmlns:c="http://sourceforge.net/projects/jsf-comp">
    
    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <webuijsf:form style="margin-left:10px;margin-right:10px;" >
                    
                    <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />

                    <table>
                      <tr>
                        <td align="left" valign="middle" width="90%" >
                            <webuijsf:head title="#{msgs.generic_ChartStatistics}" />
                        </td>
                        <td align="right" valign="middle" width="10%">
                            <webuijsf:button id="buttonTable" text="#{msgs.generic_showTable}" actionExpression="#{ChartStatistics.showTable}" />
                        </td>
                      </tr>
                    </table>
                    
                    <webuijsf:contentPageTitle id="pagetitle" title="#{msgs.chart_statistics_title}" helpText="" />                    
                    <br></br>
                    <!-- Chart Type Menu Option -->     
                    <!-- Use HTML table for layout.  Note that if we had included this
                    content within the body of contentPageTitle, then we would need
                    to wrap the HTML markup in the f:verbatim tag.  webuijsf:markup could
                    also be used but that is more heavyweight (slower). -->
                    <table>                    
                        <tr>
                            <td>
                                <webuijsf:dropDown id="ChartTypeMenuOption"     
                                                   immediate="true"
                                                   submitForm="true"
                                                   forgetValue="true"
                                                   selected="#{ChartStatistics.chartTypeMenuSelectedOption}"
                                                   items="#{ChartStatistics.chartMenuTypeOptions}"
                                                   actionListenerExpression="#{ChartStatistics.processChartMenuSelection}" /> 
                            </td>
                            <td>
                                <webuijsf:checkbox id="EffectCheckbox" 
                                                   valueChangeListenerExpression="#{ChartStatistics.checkBoxChanged}"
                                                   onChange="submit()"
                                                   label="#{ChartStatistics.threeDimensional}"
                                                   immediate="true"
                                                   selected="#{ChartStatistics.isThreeDimensionalEffect}"/>
                            </td>
                            <td>
                                <webuijsf:checkbox id="OrientationCheckbox" 
                                                   valueChangeListenerExpression="#{ChartStatistics.checkBoxChanged}"
                                                   onChange="submit()"
                                                   label="#{ChartStatistics.orientationHorizontal}"
                                                   immediate="true"
                                                   selected="#{ChartStatistics.isHorizontalOrientation}"/>
                            </td>
                            <td>
                                <webuijsf:dropDown id="TotalsChartTypeMenuOption"     
                                                   immediate="true"
                                                   submitForm="true"
                                                   forgetValue="true"
                                                   selected="#{ChartStatistics.totalsChartTypeMenuSelectedOption}"
                                                   items="#{ChartStatistics.totalsChartMenuTypeOptions}"
                                                   actionListenerExpression="#{ChartStatistics.processTotalsChartMenuSelection}" /> 
                            </td>
                            
                        </tr>   
                    </table>

                    <br></br>
                        
                    <table>
                        <tr>
                            <td>
                                <c:chart id="consumingChart" 
                                         outline="true"
                                         background="#FFFFCC"
                                         width="500"
                                         height="400"
                                         legend="true"
                                         datasource="#{ChartStatistics.consumingEndpointsBarChartData}"
                                         type="#{ChartStatistics.chartMenuType}" 
                                         is3d="#{ChartStatistics.isThreeDimensionalEffect}" 
                                         orientation="#{ChartStatistics.graphOrientation}"
                                         title="#{msgs.statistics_consumingEndpoint}">
                                </c:chart>
                            </td>
                            <td> </td>
                            <td>
                                <c:chart id="totalsConsumingChart" 
                                         outline="true"
                                         background="#FFFFCC"
                                         width="500"
                                         height="400"
                                         legend="true"
                                         datasource="#{ChartStatistics.consumingEndpointTotalsPieChartData}" 
                                         type="#{ChartStatistics.totalsChartMenuType}" 
                                         is3d="#{ChartStatistics.isThreeDimensionalEffect}"
                                         title="#{msgs.statistics_consumingEndpointTotals}">
                                </c:chart>                            
                            </td>
                        </tr>
                    </table>

                    <br></br>
                        
                    <table>
                        <tr>
                            <td>
                                
                                <c:chart id="provisioningChart" 
                                         outline="true"
                                         background="#FFFFCC"
                                         width="500"
                                         height="400"
                                         legend="true"
                                         datasource="#{ChartStatistics.provisioningEndpointsBarChartData}"
                                         type="#{ChartStatistics.chartMenuType}"
                                         is3d="#{ChartStatistics.isThreeDimensionalEffect}"
                                         orientation="#{ChartStatistics.graphOrientation}"
                                         title="#{msgs.statistics_provisioningEndpoint}">
                                </c:chart>
                            </td>
                            <td> </td>
                            <td>
                                <c:chart id="totalsProvisioningChart" 
                                         outline="true"
                                         background="#FFFFCC"
                                         width="500"
                                         height="400"
                                         legend="true"
                                         datasource="#{ChartStatistics.provisioningEndpointTotalsPieChartData}" 
                                         type="#{ChartStatistics.totalsChartMenuType}" 
                                         is3d="#{ChartStatistics.isThreeDimensionalEffect}"
                                         title="#{msgs.statistics_provisioningEndpointTotals}">
                                </c:chart>                            
                            </td>                                            
                        </tr>
                    </table>
                    <br></br>
                </webuijsf:form>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>
    
</jsp:root>
