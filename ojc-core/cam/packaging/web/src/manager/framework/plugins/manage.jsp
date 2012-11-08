<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    
    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="#{msgs.plugins_managePlugins}" />
                
                <webuijsf:form id="deployform" >
                    
                    <table>
                        
                        <tr style="height:5px"><td></td></tr>
                        
                        <tr><td colspan="2">
                           <webuijsf:contentPageTitle id="pagetitle" title="#{msgs.plugins_manager_pageTitle}" helpText="#{msgs.plugins_manager_pageDesc}" />
                        </td></tr>
                        
                        <tr style="height:5px"><td></td></tr>
                        <tr style="height:10px"><td></td></tr>
                        
                        <tr><td>
                            <!--
                                <webuijsf:textField id="filename" required="true" style="margin-left:10px;font-weight:bold" label="#{msgs.plugins_uploadFile}" 
                                   immediate="true" text="#{PluginsManagerBean.fileName}" columns="60" /> 
                             -->
                            </td>
                            <td>
                                <!--
                                <webuijsf:textField id="targetname" required="true" style="margin-left:10px;font-weight:bold" label="#{msgs.plugins_targetName}" 
                                                    text="#{PluginsManagerBean.targetName}" columns="40" /> 
                                -->
                                
                      <webuijsf:upload id="upload" style="margin-left:10px"
                                uploadedFile = "#{PluginsManagerBean.uploadedFile}"
                                required="true"
                                label="#{msgs.plugins_uploadFile}"
                                toolTip="#{msgs.plugins_uploadFileTooltip}"
                                 >
                      </webuijsf:upload>
				 
                        </td></tr>
                        <tr style="height:20px"><td></td></tr>
                        
                        <!-- File Uploader Button -->
                        <tr><td>
                                <webuijsf:button primary="true" text="#{msgs.plugins_deploy}" id="deploy" style="margin-left:10px"
                                                 actionExpression="#{PluginsManagerBean.deploy}" actionListenerExpression="#{PluginsManagerBean.actionListener}"
                                                 toolTip="#{msgs.plugins_deploy_toolTip}" >
                                </webuijsf:button>
                        </td></tr>
                        
                    </table>
                </webuijsf:form>

                <webuijsf:contentPageTitle id="tabletitle" title="" helpText="#{msgs.plugins_manager_tableDesc}" />
                
                <webuijsf:form id="pluginsform" >
                    
                <webuijsf:table id="pluginsTable" 
                                clearSortButton="true"
                                sortPanelToggleButton="true"
                                style="margin-top:10px;margin-left:10px;margin-right:10px;">
                    <!-- Title -->
                    <f:facet name="title">
                        <webuijsf:staticText text="#{msgs.plugins_managerTitle}"/>
                    </f:facet>
                    
                    <webuijsf:tableRowGroup id="rowGroup1"
                                            sourceData="#{PluginsManagerBean.plugins}" sourceVar="plugins">
                        
                        <webuijsf:tableColumn id="name"
                                              headerText="#{msgs.plugins_managerName}" rowHeader="true">
                            <webuijsf:staticText text="#{plugins.value.name}"/>
                        </webuijsf:tableColumn>
                        <webuijsf:tableColumn id="actions" 
                                              headerText="#{msgs.plugins_managerActions}" rowHeader="true">
                            <webuijsf:hyperlink id="undeployAction" style="margin:5px"
                                                actionExpression="#{PluginsManagerBean.undeploy}"
                                                text="#{msgs.plugins_manager_undeployAction}">
                            </webuijsf:hyperlink>  
                        </webuijsf:tableColumn>
                        
                    </webuijsf:tableRowGroup>
                </webuijsf:table>     
                
                </webuijsf:form>
                
            </webuijsf:html>
        </webuijsf:page>
    </f:view>
    
</jsp:root>



