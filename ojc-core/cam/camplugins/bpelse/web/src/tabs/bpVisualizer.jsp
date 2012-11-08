<?xml version="1.0" encoding="UTF-8"?>
<!--
#
# The contents of this file are subject to the terms 
# of the Common Development and Distribution License 
# (the License).  You may not use this file except in
# compliance with the License.
# 
# You can obtain a copy of the license at 
# https://glassfish.dev.java.net/public/CDDLv1.0.html or
# glassfish/bootstrap/legal/CDDLv1.0.txt.
# See the License for the specific language governing 
# permissions and limitations under the License.
# 
# When distributing Covered Code, include this CDDL 
# Header Notice in each file and include the License file 
# at glassfish/bootstrap/legal/CDDLv1.0.txt.  
# If applicable, add the following below the CDDL Header, 
# with the fields enclosed by brackets [] replaced by
# you own identifying information: 
# "Portions Copyrighted [year] [name of copyright owner]"
# 
# Copyright 2007 Sun Microsystems, Inc. All rights reserved.
#
-->


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
                <webuijsf:form id="bpvisualizeForm" 
                               style="margin-left:10px;margin-right:10px;" >
                    
                    <f:loadBundle basename="com.sun.jbi.cam.plugins.bpelse.common.resources.Bundle" var="msgs" />
                    <webuijsf:head title="#{msgs.bpvisualizer_Charting_label}" />
                    <table>
                      <tr>
                          <td>
                            <webuijsf:staticText  style="font-weight: bold;
                                                         font-size: 16pt" 
                                          text="#{BPVisualizer.pageTitle}"/>
                          </td>
                      </tr>
                    </table>
                            
                    <!-- webuijsf:contentPageTitle id="pagetitle" title="#{msgs.chart_statistics_title}" helpText="" / -->                    
                     <!-- Chart Type Menu Option -->     
                    <!-- Use HTML table for layout.  Note that if we had included this
                    content within the body of contentPageTitle, then we would need
                    to wrap the HTML markup in the f:verbatim tag.  webuijsf:markup could
                    also be used but that is more heavyweight (slower). -->
                    <table cellpadding="4">
                        <jsp:scriptlet>
                            // Get the BPVisualizerBean instnace from FacesContext
                            boolean showCustomSelection = false;
                            javax.faces.context.FacesContext context = 
                            javax.faces.context.FacesContext.getCurrentInstance();
                            
                            javax.faces.el.ValueBinding bpVisualValueBinding = 
                            context.getApplication().createValueBinding(com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPVisualizerBean.BEAN_NAME);
                            
                            com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPVisualizerBean visualizerBean = 
                            ( com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPVisualizerBean) bpVisualValueBinding.getValue(context);        
                            boolean isAggregated = visualizerBean.isAggregatedView();
                            String compName= 
                            (String) session.getAttribute(com.sun.jbi.cam.common.GenericConstants.COMPONENT_NAME);
                            String saName= 
                            (String) session.getAttribute(com.sun.jbi.cam.common.GenericConstants.COMPONENT_PNAME);
                            String selectedChart =   visualizerBean.getChartSelectionMenuSelectedOption();
                            if(!isAggregated &amp;&amp; 
                            selectedChart.equals(visualizerBean.BP_SERVICE_UNIT_CUSTOM)) {
                            showCustomSelection = true;
                            }
                            if(compName.equals(saName)) { 
                        </jsp:scriptlet>
                        <tr>   
                            <td align="left"  colspan="2"> 
                                <webuijsf:helpInline style="font-weight: normal"
                                              type="page" 
                                              text="#{BPVisualizer.inLineHelpText}" />
                            </td> 
                        </tr> 
                        <tr >   
                            <td align="left" width="90%">
                                <webuijsf:hiddenField />
                            </td>
                            <td  width="10%" align="right">
                                <webuijsf:button id="bpViewSelector" text="#{BPVisualizer.viewActionText}" actionExpression="#{BPVisualizer.switchView}" />
                            </td>
                        </tr>   
                    </table>   
                    <table>   
                     <jsp:scriptlet>
                         if(!isAggregated) { 
                      </jsp:scriptlet>
                           <tr>
                            <td width="20%"> 
                                 <webuijsf:label text="#{BPVisualizer.instanceSelectorLabel}" />
                            </td> 
                            <td> 
                                <webuijsf:dropDown id="bpelInstanceSelector"
                                       immediate ="true"
                                       submitForm="true"
                                       forgetValue="false" 
                                       selected="#{BPVisualizer.serviceUnitInstanceSelected}"
                                       items="#{BPVisualizer.serviceUnitInstanceSelectionOptions}"
                                       actionListenerExpression="#{BPVisualizer.processServiceUnitInstanceSelection}" />
                             </td> 
                         </tr>   

                        <jsp:scriptlet>
                          } 
                        </jsp:scriptlet>
                   <jsp:scriptlet>
                          }
                   </jsp:scriptlet>
                     <tr >
                            <td width="20%"> 
                                 <webuijsf:label text="#{BPVisualizer.chartSelectorLabel}" />
                            </td> 
                             <td >
                                 <webuijsf:dropDown id="ChartSelectionMenuOption"     
                                                   immediate="true"
                                                   submitForm="true"
                                                   forgetValue="true"
                                                   selected="#{BPVisualizer.chartSelectionMenuSelectedOption}"
                                                   items="#{BPVisualizer.chartSelectionMenuOptions}"
                                                   actionListenerExpression="#{BPVisualizer.processChartSelectionMenuSelection}" /> 
                            </td>
                       </tr>
                  </table>   
                   <hr/>
                   <!-- webuijsf:pageSeparator id="customseparator" / -->
                   <jsp:scriptlet>
                      javax.faces.context.FacesContext BPCustomcontext = 
                       javax.faces.context.FacesContext.getCurrentInstance();
                   javax.faces.el.ValueBinding bpCustomValueBinding = 
                        BPCustomcontext.getApplication().createValueBinding(com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPCustomChartBean.BEAN_NAME);
                    com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPCustomChartBean bpCustomBean = 
                     ( com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPCustomChartBean) bpCustomValueBinding.getValue(BPCustomcontext);        
                    if(showCustomSelection) { 
                         bpCustomBean.checkServiceUnitChanged();
                         if(bpCustomBean.hasSimpleVars() == false) {
                   </jsp:scriptlet>
                     <br></br>
                     <br></br>
                     <br></br>
                      <table>    
                         <tr>   
                            <td width="100%" align="left"> 
                            <webuijsf:alert id="nodatamsg" type="warning" 
                              summary="#{msgs.bpvisualizer_custom_no_plot_data_title}" 
                              detail="#{msgs.bpvisualizer_custom_no_plot_data}"/>
                            </td> 
                         </tr>   
                    </table>    
                   <jsp:scriptlet>
                           
                    }  else {                   
                           
                   </jsp:scriptlet>
                   <table>    
                         <tr>   
                            <td width="100%" align="left"> 
                                 <webuijsf:label  style="font-weight: normal"
                                   text="#{msgs.bpvisualizer_custom_select_short_desc}" />
                            </td> 
                         </tr>   
                    </table>    
                   <table   cellpadding="5">    
                        <tr >
                            <td >
                                <webuijsf:dropDown id="bpelSelectionMenuOption"     
                                                   label ="#{msgs.bpvisualizer_custom_bpelprocess_label}"
                                                   immediate="true"
                                                   submitForm="true"
                                                   forgetValue="true"
                                                   selected="#{CustomChartBean.bpelProcessSelectedOption}"
                                                   items="#{CustomChartBean.bpelProcessSelectionMenuOptions}"
                                                   actionListenerExpression="#{CustomChartBean.processBpelProcessSelectionMenuSelection}" /> 
                            </td>
                        </tr>    
                        <tr >
                            <td >
                                 <webuijsf:dropDown id="xSelectionMenuOption"     
                                                   label ="#{msgs.bpvisualizer_custom_xSelect_label}"
                                                   immediate="true"
                                                   submitForm="true"
                                                   forgetValue="true"
                                                   selected="#{CustomChartBean.axis_X_SelectedOption}"
                                                   items="#{CustomChartBean.axis_X_SelectionMenuOptions}"
                                                   actionListenerExpression="#{CustomChartBean.process_X_AxisSelectionMenuSelection}" /> 
                            </td>
                       <jsp:scriptlet>
                           if(bpCustomBean.isXVariableHasLimit()) {
                              if(bpCustomBean.getCurrentXVaribleType() != 
                                  com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPCustomChartBean.VAR_TYPE.DATETIME) { 
                       </jsp:scriptlet>
                            <td >
                                <webuijsf:textField label="#{msgs.bpvisualizer_custom_lowerlimit_label}"
                                       id="xLowLimit" 
                                       text="#{CustomChartBean.lowerLimit_X_Axis}"
                                       disabled="#{CustomChartBean.XVariableLimitsDisabled}"/>
                                                    
                            </td>
                           <td  >
                                <webuijsf:textField label="#{msgs.bpvisualizer_custom_upperlimit_label}"  
                                        id="xHighLimit" 
                                        text="#{CustomChartBean.upperLimit_X_Axis}"
                                        disabled="#{CustomChartBean.XVariableLimitsDisabled}"/>
                                                    
                            </td>
                        <jsp:scriptlet>
                            } else {
                        </jsp:scriptlet>
                            <td >
                                <webuijsf:calendar label="#{msgs.bpvisualizer_custom_lowerlimit_label}"
                                       id="xDateLowLimit" 
                                       disabled="#{CustomChartBean.XVariableLimitsDisabled}"
                                       selectedDate="#{CustomChartBean.dateLowerLimit_X_Axis}"/>
                                                    
                            </td>
                           <td  >
                                <webuijsf:calendar label="#{msgs.bpvisualizer_custom_upperlimit_label}"  
                                        id="xDateHighLimit" 
                                        disabled="#{CustomChartBean.XVariableLimitsDisabled}"
                                        selectedDate="#{CustomChartBean.dateUpperLimit_X_Axis}"/>
                                                  
                            </td>
                        <jsp:scriptlet>
                            } 
                         }
                        </jsp:scriptlet>
                          </tr>
                        <tr>
                            <td  >
                                 <webuijsf:dropDown id="ySelectionMenuOption" 
                                                   label ="#{msgs.bpvisualizer_custom_ySelect_label}"
                                                   immediate="true"
                                                   submitForm="true"
                                                   forgetValue="true" 
                                                   selected="#{CustomChartBean.axis_Y_SelectedOption}"
                                                   items="#{CustomChartBean.axis_Y_SelectionMenuOptions}"
                                                   actionListenerExpression="#{CustomChartBean.process_Y_AxisSelectionMenuSelection}" /> 
                           </td>
                        <jsp:scriptlet>
                           if(bpCustomBean.isYVariableHasLimit()) {
                              if(bpCustomBean.getCurrentYVaribleType() != 
                                  com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPCustomChartBean.VAR_TYPE.DATETIME) { 
                       </jsp:scriptlet>
                          <td >
                                <webuijsf:textField label="#{msgs.bpvisualizer_custom_lowerlimit_label}" 
                                            id="yLowLimit" 
                                            text="#{CustomChartBean.lowerLimit_Y_Axis}"
                                            disabled="#{CustomChartBean.YVariableLimitsDisabled}"/>

                                                    
                            </td>
                            <td  >
                                <webuijsf:textField label="#{msgs.bpvisualizer_custom_upperlimit_label}" 
                                            id="yHighLimit" 
                                            text="#{CustomChartBean.upperLimit_Y_Axis}"
                                            disabled="#{CustomChartBean.YVariableLimitsDisabled}"/>

                            </td>
                         <jsp:scriptlet>
                            } else {
                        </jsp:scriptlet>
                            <td >
                                <webuijsf:calendar label="#{msgs.bpvisualizer_custom_lowerlimit_label}"
                                       id="yDateLowLimit" 
                                       disabled="#{CustomChartBean.YVariableLimitsDisabled}"
                                      selectedDate="#{CustomChartBean.dateLowerLimit_Y_Axis}"/>
                                                    
                            </td>
                           <td  >
                                <webuijsf:calendar label="#{msgs.bpvisualizer_custom_upperlimit_label}"  
                                        id="yDateHighLimit" 
                                        disabled="#{CustomChartBean.YVariableLimitsDisabled}"
                                        selectedDate="#{CustomChartBean.dateUpperLimit_Y_Axis}"/>
                                                    
                            </td>
                        <jsp:scriptlet>
                            } 
                        }
                        </jsp:scriptlet>
                          </tr>
                        </table>
                         <table >   
                            <tr>   
                           <td align="left" width="50%">
                                <webuijsf:button id="customviewplot" 
                                  text="#{msgs.bpvisualizer_Bpel_plot_chart}"  
                                  actionExpression="#{CustomChartBean.plotCustomChart}" />
                             </td>
                           <td align="left" width="50%">
                                <webuijsf:button id="clearselection" 
                                  text="#{msgs.bpvisualizer_Bpel_clear_plot_selection}"  
                                  actionExpression="#{CustomChartBean.clearCustomSelection}"/>
                             </td>
                         </tr>   
                  </table>    
                   <hr/>
                   <!-- webuijsf:pageSeparator id="chartseparator" / -->
                    <jsp:scriptlet>
                        }
                     } 
                      boolean plotChartRequest = bpCustomBean.wasCutomPlotRequested();
                      if((!selectedChart.equals(visualizerBean.SELECT_CHART_STRING) &amp;&amp;
                         !selectedChart.equals(visualizerBean.BP_SERVICE_UNIT_CUSTOM))
                         || (selectedChart.equals(visualizerBean.BP_SERVICE_UNIT_CUSTOM)
                          &amp;&amp; plotChartRequest)) {
                  </jsp:scriptlet>
                 <table>    
                        <tr>
                            <td>
                                <webuijsf:dropDown id="ChartTypeMenuOption"     
                                                   immediate="true"
                                                   submitForm="true"
                                                   forgetValue="true"
                                                   selected="#{BPVisualizer.chartTypeMenuSelectedOption}"
                                                   items="#{BPVisualizer.chartMenuTypeOptions}"
                                                   actionListenerExpression="#{BPVisualizer.processChartTypeMenuSelection}" /> 
                            </td>
                            <td>
                                <webuijsf:checkbox id="EffectCheckbox" 
                                                   valueChangeListenerExpression="#{BPVisualizer.checkBoxChanged}"
                                                   onClick="submit()"
                                                   label="#{BPVisualizer.threeDimensional}"
                                                   immediate="true"
                                                   selected="#{BPVisualizer.isThreeDimensionalEffect}"/>
                            </td>
                            <td>
                                <webuijsf:checkbox id="OrientationCheckbox" 
                                                   valueChangeListenerExpression="#{BPVisualizer.checkBoxChanged}"
                                                   onClick="submit()"
                                                   label="#{BPVisualizer.orientationHorizontal}"
                                                   immediate="true"
                                                   selected="#{BPVisualizer.isHorizontalOrientation}"/>
                            </td>
                        </tr>   
                    </table>
                    <br></br>
                    
                    <!-- webuijsf:staticText id="statistics_consumingEndpoint" text="#{msgs.statistics_consumingEndpoint}"/ -->

                    <c:chart id="BPChart" 
                             rendered="#{BPVisualizer.chartSelected}"
                             datasource="#{BPVisualizer.chartData}"
                             type="#{BPVisualizer.chartMenuType}" 
                             is3d="#{BPVisualizer.isThreeDimensionalEffect}" 
                             orientation="#{BPVisualizer.graphOrientation}"
                             outline="true"
                             width="500"
                             height="400"
                             legend="true"
                             title="#{BPVisualizer.chartTitle}"
                             xlabel="#{BPVisualizer.chartXLabel}"
                             ylabel="#{BPVisualizer.chartYLabel}"
                             >
                    </c:chart>
                    
                    <br></br>
                    <!--webuijsf:staticText id="statistics_provisioningEndpoint" text="#{msgs.statistics_provisioningEndpoint}"/-->
                   <jsp:scriptlet>
                      } else if(bpCustomBean.hasValidData() == false){
                   </jsp:scriptlet>
                          <table>    
                         <tr>   
                            <td width="100%" align="left"> 
                            <webuijsf:alert id="nodatamsg" type="warning" 
                              summary="#{msgs.bpvisualizer_custom_validation_error}" 
                              detail="#{CustomChartBean.validationErrorMessage}"/>
                            </td> 
                         </tr>   
                    </table>    
               
                  <jsp:scriptlet>
                      } 
                   </jsp:scriptlet>
                     
                </webuijsf:form>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>
    
</jsp:root>
