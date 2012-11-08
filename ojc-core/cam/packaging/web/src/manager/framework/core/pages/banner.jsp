<jsp:root version="1.2" 
          xmlns:f="http://java.sun.com/jsf/core" 
          xmlns:h="http://java.sun.com/jsf/html" 
          xmlns:jsp="http://java.sun.com/JSP/Page" 
          xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
          
<jsp:directive.page contentType="text/html" /> 

<jsp:directive.page import="com.sun.jbi.cam.common.resources.Messages" />

<jsp:scriptlet>
    // get logout message
    String logoutMsg = Messages.getString("banner_logoutConfirm");
</jsp:scriptlet>

<script type="text/javascript">
    var msg = "<jsp:expression>logoutMsg</jsp:expression>";
    function confirmLogout() {
        if ( !confirm(msg) )  {
            return false;
        }
        return true;
    }
</script>

<f:view>

    <webuijsf:page>
    <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />

    <webuijsf:html>
        <webuijsf:head id="banner" title="Banner" />
        <webuijsf:body>
            <webuijsf:form id="form3">
               <webuijsf:masthead id="Masthead" productImageURL="/manager/framework/images/cam_title.gif"
                   productImageDescription="#{msgs.title}" >
                 
                 <f:facet name="versionLink" >
                    <webuijsf:hyperlink id="version" immediate="true" />
                 </f:facet>

                 <f:facet name="logoutLink">
                    <webuijsf:hyperlink id="logout" 
                        text="#{msgs.banner_logout}" 
                        toolTip="#{msgs.banner_logoutTooltip}" 
                        actionExpression="#{BannerBean.logout}"
                        onClick="if ( !confirmLogout() ) return false;"
                        immediate="true" />
                 </f:facet>   

                 <f:facet name="helpLink" >
                    <webuijsf:helpWindow 
                        helpFile="camoverview.html"
                        windowTitle="#{msgs.banner_helpWindowTitle}"
                        toolTip="#{msgs.banner_helpTooltip}"/>
                 </f:facet>
                 
                 <f:facet name="statusArea">
                   <webuijsf:panelGroup id="statusPanel" separator="">
                     <div class="MstTmeDiv">
                     <webuijsf:timeStamp />
                     </div>
                   </webuijsf:panelGroup>
                 </f:facet>
               </webuijsf:masthead> 
             </webuijsf:form>
       </webuijsf:body>
    </webuijsf:html>
</webuijsf:page>
    
</f:view>

</jsp:root>


