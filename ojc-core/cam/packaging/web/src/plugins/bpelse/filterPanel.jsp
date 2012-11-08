<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
  <jsp:directive.page contentType="text/html" />

<!-- Filter Panel -->
<webuijsf:staticText id="statictext1" text="Show only rows with: " /> 
<br />
<webuijsf:textField id="customFilter"
    columns="50"
    label="#{bptablemsgs.table_customFilterLabel_STATUS}"
    labelLevel="2"
    onKeyPress="if (event.keyCode==13) {var e=document.getElementById('form1:table1:filterPanel:submit'); if (e != null) e.click(); return false}"
    text="#{state.groupAFilter.statusCustomFilter}"/>
<webuijsf:textField id="customFilterBpelID"
    columns="50"
    label="#{bptablemsgs.table_customFilterLabel_BPELID}"
    labelLevel="2"
    onKeyPress="if (event.keyCode==13) {var e=document.getElementById('form1:table1:filterPanel:submit'); if (e != null) e.click(); return false}"
    text="#{state.groupAFilter.bpelIdCustomFilter}"/>
<div class="TblPnlBtnDiv">
  <webuijsf:button id="submit"
      actionExpression="#{state.groupAFilter.applyCustomFilter}"
      mini="true"
      primary="true"
      text="#{bptablemsgs.table_ok}"/>
  <webuijsf:button id="cancel"
      mini="true"
      onClick="toggleFilterPanel(); return false"
      text="#{bptablemsgs.table_cancel}"/>
</div>

<!-- Note: If the user presses the enter key while the text field has focus,
     the page will be submitted incorrectly, unless we capture the onKeyPress
     event and invoke the click method of the submit button. -->

</jsp:root>
