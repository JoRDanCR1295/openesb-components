<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %> 
<%@taglib uri="http://www.sun.com/webui/webuijsf" prefix="webuijsf" %>

<f:view>
  <webuijsf:page>
    <webuijsf:head title="#{JavaHelpBean.tipsHeadTitle}" />
    <webuijsf:body styleClass="#{JavaHelpBean.bodyClassName}">
      <webuijsf:form id="tips">              
       <h2><webuijsf:staticText text="#{JavaHelpBean.tipsTitle}" /></h2>

<p><webuijsf:staticText text="#{JavaHelpBean.tipsImprove}" /></p>
<ul>
 <li><webuijsf:staticText text="#{JavaHelpBean.tipsImprove1}" /></li>

 <li><webuijsf:staticText text="#{JavaHelpBean.tipsImprove2}" /></li>
   
 <li><webuijsf:staticText text="#{JavaHelpBean.tipsImprove3}" /></li>
   
 <li><webuijsf:staticText text="#{JavaHelpBean.tipsImprove4}" /></li>
</ul>
<p>
<b><webuijsf:staticText text="#{JavaHelpBean.tipsNote}" /></b> <webuijsf:staticText text="#{JavaHelpBean.tipsNoteDetails}" />
</p>  
<p><webuijsf:staticText text="#{JavaHelpBean.tipsSearch}" /></p>

<ul>
  <li><webuijsf:staticText text="#{JavaHelpBean.tipsSearch1}" /></li>
  <li><webuijsf:staticText text="#{JavaHelpBean.tipsSearch2}" /></li>
  <li><webuijsf:staticText text="#{JavaHelpBean.tipsSearch3}" /></li>
  <li><webuijsf:staticText text="#{JavaHelpBean.tipsSearch4}" /></li>
</ul>
      </webuijsf:form>      
    </webuijsf:body> 
  </webuijsf:page>
</f:view>
