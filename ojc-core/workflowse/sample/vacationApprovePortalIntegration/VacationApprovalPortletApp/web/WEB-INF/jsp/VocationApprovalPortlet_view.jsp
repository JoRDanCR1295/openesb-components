<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>

<%-- Uncomment below lines to add portlet taglibs to jsp
<%@ page import="javax.portlet.*"%>
<%@ taglib uri="http://java.sun.com/portlet" prefix="portlet"%>

<portlet:defineObjects />
<%PortletPreferences prefs = renderRequest.getPreferences();%> 
--%>
<%@page import="com.sun.wlm.portal.bpelclient.*"%>
<b>VocationApprovalPortlet - VIEW MODE</b>
<%
       //invoke the bpel porttype
       com.sun.wlm.portal.bpelclient.VacationApprovalService service = new com.sun.wlm.portal.bpelclient.VacationApprovalService ();
       com.sun.wlm.portal.bpelclient.SubmitVacationRequest port = service.getVacationApprovalPort();
        
       com.sun.wlm.portal.bpelclient.VocationRequestType portTypeReq = new com.sun.wlm.portal.bpelclient.VocationRequestType ();
       portTypeReq.setEmployeeId("12345");
       portTypeReq.setEmployeeName("John Smith");
       
       javax.xml.datatype.XMLGregorianCalendar calendar = javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar(); 
       calendar.setDay(3);
       calendar.setMonth(4);
       calendar.setYear(2007);
       
       portTypeReq.setStartDate(calendar);     
       
       out.println("Invoking ...");
       
       com.sun.wlm.portal.bpelclient.VocationReplyType portTypeRep = port.vacationApprovalOperation(portTypeReq);
       
       out.println("Got Reply ..." + portTypeRep);
       
       out.println("Approved :" + portTypeRep.getResult());
       
       out.println("Approved days:" + portTypeRep.getApprovedDays());

%>