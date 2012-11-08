/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)CallBpel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.tst.srvlt;

import com.sun.tst.wsc.FaultMsg;
import java.io.*;
import java.net.*;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author gpatil
 * @version
 */
public class CallBpel extends HttpServlet {

    @WebServiceRef(wsdlLocation = "WEB-INF/wsdl/client/greetService/greetService.wsdl")
    private com.sun.tst.wsc.SvcGreet service;
    
    /** Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();
        out.println("<html>");
        out.println("<head>");
        out.println("<title>Servlet CallBpel</title>");
        out.println("</head>");
        out.println("<body>");
        out.println("<h1>Servlet CallBpel at " + request.getContextPath () + "</h1>");
        out.println("<BR>");
        
        try { 
            String name = request.getParameter("name");
            
            if ((name == null) || ("".equals(name))){
                name = "Johnny " ;
            }
            
            out.println("<BR>Calling BPEL to get greeting for:" + name);
            com.sun.tst.wsc.GreetServicePortType port = service.getSvcPort();
            java.lang.String part1 = name;
            java.lang.String result = port.greetServiceOperation(part1);
            out.println("<BR>BPEL returned :" + result);            
        } catch (FaultMsg fm) {
            out.println("<BR> Got fault message:" + fm.getFaultInfo());
            fm.printStackTrace(out);
        } catch (Exception ex) {
            out.println("<BR> There was an exception:" + ex.getMessage());
            ex.printStackTrace(out);
        }
        
        out.println("<BR>");
        out.println("</body>");
        out.println("</html>");
        out.close();
    }
    
    // <editor-fold defaultstate="collapsed" desc="HttpServlet methods. Click on the + sign on the left to edit the code.">
    /** Handles the HTTP <code>GET</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        processRequest(request, response);
    }
    
    /** Handles the HTTP <code>POST</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        processRequest(request, response);
    }
    
    /** Returns a short description of the servlet.
     */
    public String getServletInfo() {
        return "Short description";
    }
    // </editor-fold>
}
