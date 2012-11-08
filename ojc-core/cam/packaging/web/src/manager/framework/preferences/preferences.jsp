<jsp:root version="1.2" 
          xmlns:f="http://java.sun.com/jsf/core" 
          xmlns:h="http://java.sun.com/jsf/html" 
          xmlns:jsp="http://java.sun.com/JSP/Page" 
          xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    
    <jsp:directive.page contentType="text/html" /> 
    
    <f:view>
        
        <webuijsf:page id="page1" >
            <webuijsf:html>
                
                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                
                <webuijsf:head title="#{msgs.pref_title}" />
                
                <webuijsf:body style="margin:10px;font-family:MS Sans Serif,Geneva,sans-serif;">
                    
                    <webuijsf:form id="form1">
                        
                        <webuijsf:contentPageTitle title="#{msgs.pref_title}" helpText="#{msgs.pref_desc}" />
                        
                        <f:verbatim><![CDATA[<br><br>]]></f:verbatim>
                        
                        <webuijsf:button id="saveButton" text="#{msgs.pref_save}"   
                                         actionExpression="#{PreferencesBean.save}" primary="true" style="margin-left:10px;" />
                        <webuijsf:button id="resetButton" reset="true" />
                        
                        <webuijsf:propertySheet id="propSheet" jumpLinks="true" >
                            
                            <webuijsf:propertySheetSection id="propertSectionTextField" label="">
                                <webuijsf:property id="propertyRefreshInterval"  labelAlign="left" noWrap="true" overlapLabel="false" label="#{msgs.pref_refreshInterval}"  >
                                    
                                    <webuijsf:textField id="refreshInterval"
                                                        text="#{PreferencesBean.refreshInterval}" 
                                                        toolTip="#{msgs.pref_refreshIntervalTooltip}"
                                                        required="false" 
                                    >
                                    </webuijsf:textField>
                                    <webuijsf:helpInline id="fieldHelp" type="inlineHelp" text="#{msgs.pref_helpInline}" style="margin-bottom:10px" />
                                    
                                </webuijsf:property>
                                <webuijsf:property id="propertyAutoRefresh"  labelAlign="left" noWrap="true" overlapLabel="false" label="#{msgs.pref_autoRefresh}" >
                                    <webuijsf:checkbox id="autoRefresh" 
                                                       toolTip="#{msgs.pref_autoRefreshTooltip}"
                                                       selected="#{PreferencesBean.autoRefresh}"
                                    >
                                    </webuijsf:checkbox>						        
                                </webuijsf:property>
                                <f:verbatim><![CDATA[<br><br>]]></f:verbatim>
                            </webuijsf:propertySheetSection>
                            
                        </webuijsf:propertySheet> 
                        
                    </webuijsf:form>             
                </webuijsf:body>
                
            </webuijsf:html>
        </webuijsf:page>
        
    </f:view>
    
</jsp:root>
