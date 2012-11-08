<jsp:root version="1.2" 
          xmlns:f="http://java.sun.com/jsf/core" 
          xmlns:h="http://java.sun.com/jsf/html" 
          xmlns:jsp="http://java.sun.com/JSP/Page" 
          xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
          
<jsp:directive.page contentType="text/html" /> 


<f:view>

  <webuijsf:page id="page1">
    <webuijsf:html id="html1" >
      <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
        <webuijsf:head id="head1" title="#{msgs.masthead_title}">
            <webuijsf:meta httpEquiv="refresh" content="#{TreeBuilderBean.refreshRate}" 
                  />
        </webuijsf:head>
        <webuijsf:form id="treeForm">
        <webuijsf:tree id="tree" url="#" style="width:40em;">
        
            <f:facet name="content">
                <h:panelGroup>
                    <!--
                    <webuijsf:imageHyperlink id="refresh" imageURL="/manager/framework/images/refresh.gif" actionExpression="#{TreeBuilderBean.refresh}" hspace="3" toolTip="#{msgs.tree_refresh}" />-->
            
                    <webuijsf:button id="refresh" text="#{msgs.tree_refresh}" toolTip="#{msgs.tree_refreshTooltip}" mini="true" actionExpression="#{TreeBuilderBean.refresh}" />
                    <span style="display:inline; margin-left:10px;margin-right:10px;">|</span>

                    <webuijsf:hyperlink id="commonTasks" text="Common Tasks" toolTip="Common Tasks" url="/faces/manager/framework/commonTasks.jsp" target="workspaceFrame" />
                    <!--
                    <webuijsf:imageHyperlink id="preferences" imageURL="/manager/framework/images/configuration.gif" target="workspaceFrame" url="/faces/manager/framework/preferences/preferences.jsp" hspace="3" toolTip="#{msgs.tree_preferences}" />
                    <webuijsf:imageHyperlink id="simulator" imageURL="/manager/framework/images/simulator.gif" target="workspaceFrame" url="/faces/manager/framework/simulator/simulator.jsp" hspace="3" toolTip="#{msgs.tree_simulator}" />
                    <webuijsf:imageHyperlink id="plugins" imageURL="/manager/framework/images/plugins.gif" target="workspaceFrame" url="/faces/manager/framework/plugins/tabsFrame.jsp" hspace="3" toolTip="#{msgs.tree_plugins}" />
                    -->
                </h:panelGroup>
            </f:facet>
    
            <webuijsf:treeNode id="rootNode" binding="#{TreeBuilderBean.rootNode}" />  
    
       </webuijsf:tree>   
       </webuijsf:form>     
  </webuijsf:html>
  </webuijsf:page>

</f:view>
</jsp:root>
