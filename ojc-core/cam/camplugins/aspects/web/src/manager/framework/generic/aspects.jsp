
<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
        <f:view>
            
        <webuijsf:page frame="true">
            <webuijsf:html>
                <f:loadBundle basename="com.sun.jbi.cam.plugins.aspects.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="#{msgs.aspects_title}" />
                
                 <webuijsf:contentPageTitle id="pagetitle" title="#{msgs.aspects_pageTitle}" helpText="#{msgs.aspects_pageDesc}" />
                
                 <webuijsf:button id="Save" text="#{msgs.aspects_advice}" 
                        style="margin:10px;" 
                        actionExpression="#{AspectsBean.advice}" primary="true" />
                 
                 
                 <webuijsf:table id="saTable" 
                    clearSortButton="true"
                    sortPanelToggleButton="true"
                    style="margin-top:10px;margin-left:10px;margin-right:10px;">
                                        
                    <!-- Title -->
                    <f:facet name="title">
                         <webuijsf:staticText text="#{msgs.aspects_tableTitle}"/>
                    </f:facet>

                    <webuijsf:tableRowGroup id="rowGroup1"
                            sourceData="#{AspectsBean.serviceAssemblies}" sourceVar="aspects">
                
                        <webuijsf:tableColumn id="name"
                            headerText="#{msgs.aspects_name}" rowHeader="true">
                            <webuijsf:staticText text="#{aspects.value.name}"/>
                        </webuijsf:tableColumn>
                        <webuijsf:tableColumn id="desc"
                            headerText="#{msgs.aspects_desc}" rowHeader="true">
                            <webuijsf:staticText text="#{aspects.value.desc}"/>
                        </webuijsf:tableColumn>
                        <webuijsf:tableColumn id="status"
                            headerText="#{msgs.aspects_status}" rowHeader="true">
                            <webuijsf:staticText text="#{aspects.value.status}"/>
                        </webuijsf:tableColumn>

                    </webuijsf:tableRowGroup>
                </webuijsf:table>  
                
            </webuijsf:html>
        </webuijsf:page>
    </f:view>
    
</jsp:root>


