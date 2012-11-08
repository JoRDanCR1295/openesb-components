package com.sun.wlm.portal.portlet;
import javax.portlet.GenericPortlet;
import javax.portlet.ActionRequest;
import javax.portlet.RenderRequest;
import javax.portlet.ActionResponse;
import javax.portlet.RenderResponse;
import javax.portlet.PortletException;
import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import javax.portlet.PortletRequestDispatcher;
import javax.portlet.PortletSession;

public class LeaveApplicationPortlet extends GenericPortlet {

    public void processAction(ActionRequest request, ActionResponse response) throws PortletException,IOException {
        response.setRenderParameter("submitted","test");        
        try {
            String id = request.getParameter("employeeID");
            String name = request.getParameter ("employeeName");
            String startDay = request.getParameter ("startDate");
            String days = request.getParameter("days");
            java.text.SimpleDateFormat simpleFormat = new java.text.SimpleDateFormat ("MM/dd/yyyy");
            Date date = simpleFormat.parse (startDay);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            
            javax.xml.datatype.XMLGregorianCalendar calendar = javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar(); 
            calendar.setDay(cal.get(Calendar.DATE));
            calendar.setMonth(cal.get(Calendar.MONTH));
            calendar.setYear(cal.get(Calendar.YEAR));

            com.sun.wlm.portal.bpelclient.VacationApprovalService service = new com.sun.wlm.portal.bpelclient.VacationApprovalService ();
            com.sun.wlm.portal.bpelclient.SubmitVacationRequest port = service.getVacationApprovalPort();
        
            com.sun.wlm.portal.bpelclient.VocationRequestType portTypeReq = new com.sun.wlm.portal.bpelclient.VocationRequestType ();
            portTypeReq.setEmployeeId(id);
            portTypeReq.setEmployeeName(name);
            portTypeReq.setStartDate(calendar);
            portTypeReq.setTotalDays(Integer.parseInt(days));            
      
            com.sun.wlm.portal.bpelclient.VocationReplyType portTypeRep = port.vacationApprovalOperation(portTypeReq);
            request.getPortletSession().setAttribute("reply", portTypeRep);
        }catch(Exception e) {
            e.printStackTrace();
        }
    }
    

    public void doView(RenderRequest request,RenderResponse response) throws PortletException,IOException {
        response.setContentType("text/html");        
        PortletRequestDispatcher dispatcher =
        getPortletContext().getRequestDispatcher("/WEB-INF/jsp/LeaveApplicationPortlet_view.jsp");
        dispatcher.include(request, response);
    }

    public void doEdit(RenderRequest request,RenderResponse response) throws PortletException,IOException {
            response.setContentType("text/html");        
        PortletRequestDispatcher dispatcher =
        getPortletContext().getRequestDispatcher("/WEB-INF/jsp/LeaveApplicationPortlet_edit.jsp");
        dispatcher.include(request, response);
    }

    public void doHelp(RenderRequest request, RenderResponse response) throws PortletException,IOException {

        response.setContentType("text/html");        
        PortletRequestDispatcher dispatcher =
        getPortletContext().getRequestDispatcher("/WEB-INF/jsp/LeaveApplicationPortlet_help.jsp");
        dispatcher.include(request, response);
    }

}