/*
 * ClientServlet.java
 *
 * Created on May 26, 2007, 9:57 PM
 */

package com.sun.jbi.wsittest.client;

import com.sun.jbi.wsittest.client.messageoptimization_basic.MessageOptimizationBasicService;
import com.sun.jbi.wsittest.client.reliablemessaging_basic.ReliableMessagingBasicService;
import java.io.*;
import java.net.*;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author Frank
 * @version
 */
public class ClientServlet extends HttpServlet {
    @WebServiceRef(wsdlLocation = "http://localhost:12100/WSITTestServices/ReliableMessaging_BasicService?wsdl")
    public ReliableMessagingBasicService service_1;
    
    @WebServiceRef(wsdlLocation = "http://localhost:12100/WSITTestServices/MessageOptimization_BasicService?wsdl")
    public MessageOptimizationBasicService service_2;
    
    /**
     * Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();

        out.println("<html>");
        out.println("<head>");
        out.println("<title>Servlet ClientServlet</title>");
        out.println("</head>");
        out.println("<body>");
        out.println("<h1>Servlet ClientServlet at " + request.getContextPath () + "</h1>");

        try { // Call Web Service Operation
            com.sun.jbi.wsittest.client.reliablemessaging_basic.ReliableMessagingBasic port = service_1.getReliableMessagingBasicPort();
            // TODO initialize WS operation arguments here
            java.lang.String p1 = "Hello Reliable Messaging Basic";
            // TODO process result here
            java.lang.String result = port.echo(p1);
            out.println("<p>Result = "+result);
        } catch (Exception ex) {
            out.println("<p>Exception in Reliable Messaging Basic: " + ex);
        }

        try { // Call Web Service Operation
            com.sun.jbi.wsittest.client.messageoptimization_basic.MessageOptimizationBasic port = service_2.getMessageOptimizationBasicPort();
            // TODO initialize WS operation arguments here
            java.lang.String p1 = "Hello Message Optimization Basic";
            // TODO process result here
            java.lang.String result = port.echo(p1);
            out.println("<p>Result = "+result);
        } catch (Exception ex) {
            out.println("<p>Exception in Message Optimization Basic: " + ex);
        }

        out.println("</body>");
        out.println("</html>");

        out.close();
    }
    
    // <editor-fold defaultstate="collapsed" desc="HttpServlet methods. Click on the + sign on the left to edit the code.">
    /**
     * Handles the HTTP <code>GET</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processRequest(request, response);
    }
    
    /**
     * Handles the HTTP <code>POST</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processRequest(request, response);
    }
    
    /**
     * Returns a short description of the servlet.
     */
    public String getServletInfo() {
        return "Short description";
    }
    // </editor-fold>
}
