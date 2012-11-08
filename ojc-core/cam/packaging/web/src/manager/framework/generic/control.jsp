<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
             <webuijsf:form id="form1" style="margin-left:10px;margin-right:10px;">
                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="#{msgs.generic_Control}" />

               <webuijsf:contentPageTitle id="pagetitle" title="#{ControlBean.title}" helpText="" />    
               <br></br>

               <webuijsf:table id="table1" >
                   
                        <!-- Title -->
                        <f:facet name="title">
                         <webuijsf:staticText text="#{ControlBean.tableTitle}"/>
                        </f:facet>
                        
                    <webuijsf:tableRowGroup id="rowGroup1"
                            sourceData="#{ControlBean.components}" sourceVar="control">
                
                        <webuijsf:tableColumn id="name"
                            headerText="#{msgs.control_name}" rowHeader="true">
                            <webuijsf:staticText text="#{control.value.name}"/>
                        </webuijsf:tableColumn>
                        <webuijsf:tableColumn id="state" headerText="#{msgs.control_state}" 
                            rowHeader="true">
                            <webuijsf:staticText text="#{control.value.status}"/>
                        </webuijsf:tableColumn>
                        <webuijsf:tableColumn id="actions" embeddedActions="true" headerText="#{msgs.control_actions}" 
                            rowHeader="false" >
                            <!--<webuijsf:staticText text="#{control.value.actions}"/>-->
                            
                           <webuijsf:hyperlink id="startAction" style="margin:5px"
                                  actionExpression="#{ControlBean.start}"
                                  text="#{msgs.control_startAction}">
                                </webuijsf:hyperlink>
                                <webuijsf:hyperlink id="stopAction" style="margin:5px"
                                  actionExpression="#{ControlBean.stop}"
                                  text="#{msgs.control_stopAction}">
                                </webuijsf:hyperlink>                            
                                <webuijsf:hyperlink id="shutdownAction" style="margin:5px"
                                  actionExpression="#{ControlBean.shutdown}"
                                  text="#{msgs.control_shutdownAction}">
                                </webuijsf:hyperlink>                            
                            
                        </webuijsf:tableColumn>
                        

                    </webuijsf:tableRowGroup>
                </webuijsf:table>  
            </webuijsf:form>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>


