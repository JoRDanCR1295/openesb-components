<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" 
          xmlns:h="http://java.sun.com/jsf/html" 
          xmlns:jsp="http://java.sun.com/JSP/Page" 
          xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    
    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" 
                        pageEncoding="UTF-8"/>
    
    <jsp:scriptlet> 

       String name = request.getParameter("name");
       String type = request.getParameter("type");
       String ctype = request.getParameter("ctype");
       String cname = request.getParameter("cname");
       String pname = request.getParameter("pname");
       String tname = request.getParameter("tname");
       String serviceUnitName = request.getParameter("serviceUnitName");

       session.setAttribute("name",name);
       session.setAttribute("type",type);
       session.setAttribute("ctype",ctype);
       session.setAttribute("cname",cname);
       session.setAttribute("pname",pname);
       session.setAttribute("tname",tname);
       session.setAttribute("serviceUnitName",serviceUnitName);

    </jsp:scriptlet>                         
                        
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                    
                <webuijsf:body>
                    <f:loadBundle basename="com.sun.jbi.cam.plugins.aspects.common.resources.Bundle" var="msgs" />
                    <webuijsf:head title="#{msgs.generic_Configuration}" />
                    
                    <webuijsf:contentPageTitle id="pagetitle" title="#{ConfigurationBean.title}" helpText="" />    
                    
                    <webuijsf:form id="form" style="margin-left:10px;margin-right:10px;">
                        
                        <webuijsf:button id="Save" text="#{msgs.configuration_save}" actionExpression="#{ConfigurationBean.save}" primary="true" />
                        <webuijsf:button id="actionReset" reset="true" text="#{msgs.configuration_reset}"/>
                        
                    <br></br>
                    <!-- Alert -->
                    <webuijsf:alert id="alert"
                        summary="Alert Message"
                        type="info"
                        rendered="#{ConfigurationBean.alertRendered}"
                        detail="#{ConfigurationBean.alertDetail}" />
                    <br></br>
                        
                        <webuijsf:propertySheet id="propSheet" jumpLinks="true"  binding="#{ConfigurationBean.propertySheet}" >
                            
                        </webuijsf:propertySheet>                    
                        
                        
                    </webuijsf:form>
                </webuijsf:body>           
            </webuijsf:html>
        </webuijsf:page>
    </f:view>
    
</jsp:root>
