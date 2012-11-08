<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@page import="java.util.*"%>
<%@taglib uri="http://java.sun.com/portlet" prefix="portlet"%>
<portlet:defineObjects/>
<br>
</br>
<FONT color="#000000" face="Arial"><B><U>Submit Leave</U></B></FONT>
<br>
</br>
<%
String attribute = (String)renderRequest.getParameter("submitted");
javax.portlet.PortletURL harryUrl = null;
com.sun.wlm.portal.bpelclient.VocationReplyType portTypeRep = null;
java.text.SimpleDateFormat simpleFormat = new java.text.SimpleDateFormat ("MM/dd/yyyy");
java.util.Calendar calendar = java.util.Calendar.getInstance();

String date = simpleFormat.format(calendar.getTime());
if(attribute == null || !attribute.equals("test")) {
%>
<form name="approvalForm" method="POST" action="<%=renderResponse.createActionURL().toString()%>">
    <br/>
    <FONT color="#000000" face="Arial">Employee ID</FONT>
    <INPUT type="text" size="18" name="employeeID" value=""/>
    <br/>
    <br/>
    <FONT color="#000000" face="Arial">Employee Name</FONT>
    <INPUT type="text" size="18" name="employeeName" value=""/>
     <br/>
     <br/>
    <FONT color="#000000" face="Arial">Leave start from (mm/dd/yyyy)</FONT>
    <INPUT type="TEXT" size="18" name="startDate" value="<%=date %>" />
     <br/>     
     <br/>
    <FONT color="#000000" face="Arial">Number of Days</FONT>
    <INPUT type="text" size="18" name="days" value="" /><br>
    </br>
    <br/>
    <INPUT align="right" type="submit" name="Approve" value="Submit" />
    <INPUT align="right" type="submit" name="Approve" value="Cancel" />    
    
</form>
<%
} else if(attribute.equals("test")) {
    harryUrl = renderResponse.createRenderURL();
    harryUrl.setParameter("submitted", "none");
    portTypeRep = (com.sun.wlm.portal.bpelclient.VocationReplyType) renderRequest.getPortletSession().getAttribute ("reply");
 
%>
<b>
    <FONT color="#000000" face="Arial">Approve Result</FONT>    
</b>
<br/>
<b><%=portTypeRep.getResult()%> </b>
<br/>
<b>Approved Days:</b> <%=portTypeRep.getApprovedDays()%>
<br/>
<br/>
<a href="<%= harryUrl.toString()%>">Click to submit another leave form</a>
<
<%
}
%>