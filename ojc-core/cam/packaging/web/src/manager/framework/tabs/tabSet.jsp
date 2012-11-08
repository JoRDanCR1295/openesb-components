<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

<f:view> 
  <webuijsf:page> 
    <webuijsf:html> 
    <webuijsf:head title="Tabs Example Page" /> 
    <webuijsf:body styleClass="DefBdy"> 
      <webuijsf:form id="tabsexample"> 

        <webuijsf:tabSet binding="#{TabsBean.sportsTabs}" /> 
        <p align="center"> 
          <strong><webuijsf:staticText escape="false" text="#{TabsBean.message}" /></strong> 
         </p> 

      </webuijsf:form> 
    </webuijsf:body> 
    </webuijsf:html> 
  </webuijsf:page> 
</f:view> 


</jsp:root>
