<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    
    <f:view> 
        <webuijsf:page> 
            <webuijsf:html> 
                <webuijsf:head title="Tabs Example Page" /> 
                <webuijsf:body styleClass="DefBdy"> 
                    <webuijsf:form id="tabsexample"> 
                        
                        <webuijsf:tabSet id="MyTabs" selected="tab1" >
                            <webuijsf:tab id="tab1" text="Tab 1" actionExpression="#{TabsBean.tab1Clicked}" 
                                />
                            <webuijsf:tab id="tab2" text="Tab 2" actionExpression="#{TabsBean.tab2Clicked}" 
                                />
                            <webuijsf:tab id="tab3" text="Tab 3" actionExpression="#{TabsBean.tab3Clicked}" 
                               />
                        </webuijsf:tabSet >
                        <p align="center"> 
                            <strong><webuijsf:staticText escape="false" text="#{TabsBean.message}" /></strong> 
                        </p> 
                        
                    </webuijsf:form> 
                </webuijsf:body> 
            </webuijsf:html> 
        </webuijsf:page> 
    </f:view> 
    
    
</jsp:root>
