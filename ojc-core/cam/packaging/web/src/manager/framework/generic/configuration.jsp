<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    
    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <webuijsf:body>
                    <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                    <webuijsf:head title="#{msgs.generic_Configuration}" />
                    
                    <webuijsf:contentPageTitle id="pagetitle" title="#{ConfigurationBean.title}" helpText="#{msgs.configuration_helpInline}" />    
                    
                    <br></br>
                    
                    <webuijsf:form id="form" style="margin-left:10px;margin-right:10px;">
                        
                        <webuijsf:button id="Save" text="#{msgs.configuration_save}" actionExpression="#{ConfigurationBean.save}" 
                                    primary="true" rendered="#{ConfigurationBean.renderButtons}"/>
                        <webuijsf:button id="actionReset" reset="true" text="#{msgs.configuration_reset}" 
                                    rendered="#{ConfigurationBean.renderButtons}"/>
                        
                        <br></br>
                        <!-- Alert -->
                        <webuijsf:alert id="alert"
                                        summary="#{ConfigurationBean.alertSummary}"
                                        type="#{ConfigurationBean.alertType}"
                                        rendered="#{ConfigurationBean.alertMessageRendered}"
                                        detail="#{ConfigurationBean.alertMessage}" />
                        <br></br>
                        
                        <webuijsf:propertySheet id="propSheet" jumpLinks="true"  binding="#{ConfigurationBean.propertySheet}" >
                            
                        </webuijsf:propertySheet>                    
                        
                        
                    </webuijsf:form>
                </webuijsf:body>           
            </webuijsf:html>
        </webuijsf:page>
    </f:view>
    
</jsp:root>
