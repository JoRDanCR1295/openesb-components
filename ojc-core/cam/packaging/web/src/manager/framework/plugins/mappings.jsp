
<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
        <f:view>
            
        <webuijsf:page frame="true">
            <webuijsf:html>
                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="#{msgs.plugins_managePlugins}" />
                
                 <webuijsf:contentPageTitle id="pagetitle" title="#{msgs.plugins_mappings_pageTitle}" helpText="#{msgs.plugins_mappings_pageDesc}" />
                
                 <webuijsf:table id="mappingsTable" 
                    clearSortButton="true"
                    sortPanelToggleButton="true"
                    style="margin-top:10px;margin-left:10px;margin-right:10px;">
                                         <!-- Title -->
                    <f:facet name="title">
                         <webuijsf:staticText text="#{msgs.plugins_mappingsTitle}"/>
                    </f:facet>

                    <webuijsf:tableRowGroup id="rowGroup1"
                            sourceData="#{PluginsManagerBean.mappings}" sourceVar="plugins">
                
                        <webuijsf:tableColumn id="mappingsType"
                            sort="first" alignKey="first"
                            headerText="#{msgs.plugins_mappingsType}" rowHeader="true">
                            <webuijsf:staticText text="#{plugins.value.type}"/>
                        </webuijsf:tableColumn>
                        <webuijsf:tableColumn id="mappingsUrl" 
                            sort="last" alignKey="last"
                            headerText="#{msgs.plugins_mappingsUrl}" 
                            rowHeader="true">
                            <webuijsf:staticText text="#{plugins.value.url}"/>
                        </webuijsf:tableColumn>

                    </webuijsf:tableRowGroup>
                </webuijsf:table>  
                
            </webuijsf:html>
        </webuijsf:page>
    </f:view>
    
</jsp:root>


