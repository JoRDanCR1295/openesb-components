/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package test;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.ws.WebServiceRef;
import localhost.synchronous.synchronous.Service1;

/**
 *
 * @author sweng
 */
public class InvokeSyncSampleWithSAMLServlet extends HttpServlet {
    @WebServiceRef(wsdlLocation = "http://localhost:18181/Synchronous?wsdl")
    private Service1 service;
   
    /** 
    * Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
    * @param request servlet request
    * @param response servlet response
    */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();
        try {
            out.println("<html>");
            out.println("<head>");
            out.println("<title>Servlet InvokeSyncSampleWithSAMLServlet</title>");  
            out.println("</head>");
            out.println("<body>");
            out.println("<h1>Servlet InvokeSyncSampleWithSAMLServlet at " + request.getContextPath () + "</h1>");
            
try { // Call Web Service Operation
   localhost.synchronous.synchronous.PortType1 port = service.getPort1();
   	 // TODO initialize WS operation arguments here
	org.netbeans.xml.schema.synchronous.SimpleProcess inputType = new org.netbeans.xml.schema.synchronous.SimpleProcess();
        inputType.setParamA("Hello World");
   org.netbeans.xml.schema.synchronous.SimpleProcess result = port.operation1(inputType);
   out.println("=====================================================");
   out.println("Result = "+ result.getParamA());
} catch (Exception ex) {
   out.println("Exception happened: " + ex.getMessage());
}

            out.println("</body>");
            out.println("</html>");
        } finally { 
            out.close();
        }
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
    }// </editor-fold>

}
