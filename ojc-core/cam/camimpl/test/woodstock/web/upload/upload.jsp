<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    <jsp:directive.page contentType="text/html"/>
    <f:view>
        <webuijsf:page>
            <webuijsf:html>
            
                <webuijsf:head/>
              
                <webuijsf:body>
                                                 
                <webuijsf:form id="uploaderForm">
                       
                         <div class="ConMgn">
                         <table><tr style="height:5px"><td>
                         </td></tr>
                         <tr style="height:10px"><td></td></tr>
                                     
                         <!-- File Uploader -->
                         <tr><td>
                            <webuijsf:upload id="upload"  
                            uploadedFile = "#{UploadBean.uploadedFile}"
                            required="true"
                            label="Upload file:"
                            />
                         <f:verbatim><![CDATA[ &nbsp; ]]></f:verbatim>
                         </td></tr>
                         <tr style="height:20px"><td></td></tr>
                         
                            <!-- File Uploader Button -->
                         <tr><td>
                           <webuijsf:button primary="true"  text="Upload" id="button"  
                                   actionExpression="#{UploadBean.actionHandler}" />
                         </td></tr>
                         
                         <tr><td>
                           <webuijsf:messageGroup id="messageGroup1" showDetail="true"/>
                         </td></tr>

                         </table>
                         </div>
                         
                   </webuijsf:form>
               </webuijsf:body> 
            </webuijsf:html>  
        </webuijsf:page>
    </f:view>
</jsp:root>
