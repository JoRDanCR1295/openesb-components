<jsp:root version="2.0" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
  <jsp:directive.page contentType="text/html" />

<!-- Actions (Top) -->

<webuijsf:button id="actionDelete"
    actionExpression="#{TableBean2.delete}"
    text="Delete"/>
<webuijsf:button id="actionAdd"
    actionExpression="#{TableBean2.add}"
    text="Add"/>
<webuijsf:button id="actionEdit"
    actionExpression="#{TableBean2.edit}"
    text="Edit"/>
    

</jsp:root>
