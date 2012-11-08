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
 * @(#)RealTimeSUStateServlet.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.common.GenericConstants;
import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 *
 * @author rdamir
 * @version
 */
public class RealTimeSUStateServlet extends HttpServlet {
    
    
    // <editor-fold defaultstate="collapsed" desc="HttpServlet methods. Click on the + sign on the left to edit the code.">
    /** Handles the HTTP <code>GET</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        doPost(request, response);
    }
    
    /** Handles the HTTP <code>POST</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        processRequest(request, response);
    }
     /** Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        String name = request.getParameter(GenericConstants.COMPONENT_PNAME);
    
        RealTimeSUStateHelper suRTHelper =  new RealTimeSUStateHelper(name);
        String responseData = suRTHelper.getServiceUnitsState();
        response.setStatus(HttpServletResponse.SC_OK);
        response.setHeader(RealTimeStatisticServlet.CONTENT_TYPE_HEADER, 
                           RealTimeStatisticServlet.CONTENT_TYPE_VALUE);
        response.setHeader(RealTimeStatisticServlet.CACHE_CONTROL_HEADER, 
                           RealTimeStatisticServlet.CACHE_CONTROL_VALUE);
        
        // Write out the message on the response stream
        OutputStream outputStream = response.getOutputStream();
        try {
                outputStream.write(responseData.getBytes());
        } catch (IOException e2) {
        }
        outputStream.flush();
  
    }
   
 }
