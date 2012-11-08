<jsp:root version="2.0" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html"
xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
 <jsp:directive.page contentType="text/html"/>
 <f:view>    
  <webuijsf:page>
    <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
 
    <webuijsf:html id="html">  
      <webuijsf:head id="head" title="Common Tasks">                        
        <webuijsf:link rel="shortcut icon" url="/images/favicon.ico" type="image/x-icon" />
      </webuijsf:head>
      <webuijsf:body id="body" styleClass="#{themeStyles.CTS_BACKGROUND}">
        <webuijsf:form id="form1">  

       <webuijsf:commonTasksSection id="ctp1">
        
         <webuijsf:commonTasksGroup id="taskFirstTime" title="#{msgs.commontasks_overviewTitle}">
            <webuijsf:commonTask id="task2" text="#{msgs.commontasks_overviewHeading}" toolTip="#{msgs.commontasks_overviewTooltip}"
                 actionExpression="#{CommonTasksBean.showOverview}"  
                 infoTitle="#{msgs.commontasks_overviewHeading}"
                 infoText="#{msgs.commontasks_overviewDesc}" icon="CTS_OVERVIEW"/>
        </webuijsf:commonTasksGroup>

        <webuijsf:commonTasksGroup id="taskDeployments"  title="#{msgs.commontasks_deploymentsTitle}">
            <webuijsf:commonTask id="task3" text="#{msgs.commontasks_deploymentsHeading}"  toolTip="#{msgs.commontasks_deploymentsHeading}"
                 actionExpression="#{CommonTasksBean.showDeployments}"  
                 infoTitle="#{msgs.commontasks_deploymentsHeading}" 
                 infoText="#{msgs.commontasks_deploymentsDesc}">
            </webuijsf:commonTask>
        </webuijsf:commonTasksGroup>

        <webuijsf:commonTasksGroup id="taskTests"  title="#{msgs.commontasks_testsTitle}">
            <webuijsf:commonTask id="task4" text="#{msgs.commontasks_testsHeading}" 
                 toolTip="#{msgs.commontasks_testsTooltip}"
                 actionExpression="#{CommonTasksBean.showTests}" 
                 infoLinkText="" infoTitle="#{msgs.commontasks_testsHeading}" 
                 infoText="#{msgs.commontasks_testsDesc}" >
            </webuijsf:commonTask>
        </webuijsf:commonTasksGroup>
        
        <webuijsf:commonTasksGroup id="taskConfiguration" title="#{msgs.commontasks_configurationTitle}">
           <webuijsf:commonTask id="task5" text="#{msgs.commontasks_configurationHeading}" 
                 actionExpression="#{CommonTasksBean.showConfigurations}" 
                 infoTitle="#{msgs.commontasks_configurationHeading}"
                 toolTip="#{msgs.commontasks_configurationTooltip}" infoText="#{msgs.commontasks_configurationDesc}" >
           </webuijsf:commonTask>
        </webuijsf:commonTasksGroup>

        <webuijsf:commonTasksGroup id="taskAspects" title="#{msgs.commontasks_aspectsTitle}">
           <webuijsf:commonTask id="task6" text="#{msgs.commontasks_aspectsHeading}" 
                 actionExpression="#{CommonTasksBean.showAspects}" 
                 infoTitle="#{msgs.commontasks_aspectsHeading}"
                 toolTip="#{msgs.commontasks_aspectsTooltip}" infoText="#{msgs.commontasks_aspectsDesc}" >
           </webuijsf:commonTask>
        </webuijsf:commonTasksGroup>

         </webuijsf:commonTasksSection>
       </webuijsf:form>
      </webuijsf:body>
          
    </webuijsf:html>  
  </webuijsf:page>
 </f:view>
</jsp:root>
