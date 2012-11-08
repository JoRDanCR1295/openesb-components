<jsp:root version="1.2" 
          xmlns:f="http://java.sun.com/jsf/core" 
          xmlns:h="http://java.sun.com/jsf/html" 
          xmlns:jsp="http://java.sun.com/JSP/Page" 
          xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    
    <jsp:directive.page contentType="text/html" /> 
    
    <f:view>
        
        <webuijsf:page id="page1" >
            
            <webuijsf:html>
                
                <webuijsf:body style="margin:10px;font-family:MS Sans Serif,Geneva,sans-serif;">
                    
                    <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                    
                    <webuijsf:head title="#{msgs.simulator_title}" />
                    
                    <webuijsf:form id="form1">
                        
                        <webuijsf:contentPageTitle title="#{msgs.simulator_title}" helpText="#{msgs.simulator_desc}" />
                        <f:verbatim><![CDATA[<br><br>]]></f:verbatim>
                        
                        <table border="0" cellpadding="2" >
                            
                            <tr valign="top">
                                <td align="right">
                                    <webuijsf:staticText id="statictext1" style="font-weight:bold" text="#{msgs.simulator_destinationUri}" />
                                    <f:verbatim><![CDATA[&nbsp;]]></f:verbatim>
                                    <webuijsf:image id="image1" url="/theme/com/sun/webui/jsf/suntheme/images/other/required.gif"/>
                                </td>
                                <td>
                                    <webuijsf:textField id="destinationUri"  
                                                        text="#{SimulatorBean.destinationUri}" 
                                                        toolTip="#{msgs.simulator_destinationUriTooltip}"
                                                        required="true"
                                                        columns="80"
                                                        style="width:430px;"
                                    />
                                </td>
                            </tr>
                            <tr valign="top">
                                <td align="right">
                                    <webuijsf:staticText id="statictext2" style="font-weight:bold" text="#{msgs.simulator_soapAction}" />
                                    <f:verbatim><![CDATA[&nbsp;&nbsp;&nbsp;]]></f:verbatim>
                                    <webuijsf:image id="image3" url="/theme/com/sun/webui/jsf/suntheme/images/other/dot.gif"/>
                                </td>
                                <td>
                                    <webuijsf:textField id="soapAction"  
                                                        text="#{SimulatorBean.soapAction}" 
                                                        toolTip="#{msgs.simulator_soapActionTooltip}"
                                                        columns="80"
                                                        style="width:430px;"
                                    />
                                </td>
                            </tr>
                            <tr valign="top"> 
                                <td align="right">
                                    <webuijsf:staticText id="statictext3" style="font-weight:bold" text="#{msgs.simulator_soapPayload}" />
                                    <f:verbatim><![CDATA[&nbsp;]]></f:verbatim>
                                    <webuijsf:image id="image2" url="/theme/com/sun/webui/jsf/suntheme/images/other/required.gif"/>
                                </td>
                                <td>
                                    <webuijsf:textArea id="soapPayload"  
                                                       text="#{SimulatorBean.soapPayload}" 
                                                       toolTip="#{msgs.simulator_soapPayloadTooltip}"
                                                       required="true"
                                                       rows="15" columns="75"
                                                       style="width:430px;"
                                    />
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2">
                                    <f:verbatim><![CDATA[<br><br>]]></f:verbatim>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <f:verbatim><![CDATA[<br>]]></f:verbatim>
                                </td>
                                <td>
                                    <webuijsf:button id="resetButton" text="#{msgs.simulator_reset}" primary="true" reset="true" />
                                    <f:verbatim><![CDATA[&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;]]></f:verbatim>
                                    <webuijsf:button id="clearButton" text="#{msgs.simulator_clear}" actionExpression="#{SimulatorBean.clear}" primary="true" />
                                    <f:verbatim><![CDATA[&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;]]></f:verbatim>
                                    <webuijsf:button id="submitButton" text="#{msgs.simulator_submit}" actionExpression="#{SimulatorBean.submit}" primary="true" />
                                    <f:verbatim><![CDATA[&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;]]></f:verbatim>
                                </td>
                            </tr>
                            
                            <tr>
                                <td colspan="2">
                                    <f:verbatim><![CDATA[<br><br>]]></f:verbatim>
                                </td>
                            </tr>
                            
                            <tr valign="top"> 
                                <td align="right">
                                    <webuijsf:staticText id="resultLabel" style="font-weight:bold" text="#{msgs.simulator_result}" />
                                    <f:verbatim><![CDATA[&nbsp;]]></f:verbatim>
                                </td>
                                <td>
                                    <webuijsf:textArea id="resultTextArea"  
                                                       text="#{SimulatorBean.soapResult}" 
                                                       toolTip="#{msgs.simulator_resultTooltip}"
                                                       rows="15" columns="75"
                                                       style="width:430px;"
                                    />
                                </td>
                            </tr>
                            
                        </table>
                        
                    </webuijsf:form>             
                </webuijsf:body>
            </webuijsf:html>
        </webuijsf:page>
        
    </f:view>
    
</jsp:root>
