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
 * @(#)WebService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.web.ajax.servlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.StringTokenizer;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.sun.jbi.cam.plugins.aspects.web.ajax.ServiceSupport;

/**
 *
 * @author graj
 * @version
 */
public class WebService extends HttpServlet {
	private static final long serialVersionUID = 1L;

    private final static String REQUEST_SERVICE_OPERATIONSLIST_KEY = "operationsList";
    private final static String REQUEST_SERVICE_MESSAGETYPES_KEY = "messageTypes";
    private final static String PARAMETER_SERVICE_KEY = "service";
    private final static String PARAMETER_WSDL_KEY = "wsdl";
    private final static String PARAMETER_PORTTYPE_KEY = "portType";
    private final static String PARAMETER_OPERATION_KEY = "operation";
    
    /**
     * Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        String concatenatedString = "";
        ServiceSupport support = new ServiceSupport();

        String serviceRequestParameter = (String)request.getParameter(PARAMETER_SERVICE_KEY);
        String wsdlRequestParameter = (String)request.getParameter(PARAMETER_WSDL_KEY);
        String portTypeRequestParameter = (String)request.getParameter(PARAMETER_PORTTYPE_KEY);
        String operationRequestParameter = (String)request.getParameter(PARAMETER_OPERATION_KEY);
        
        if((serviceRequestParameter != null) && (serviceRequestParameter.trim().length() > 0)) {
            if(REQUEST_SERVICE_OPERATIONSLIST_KEY.equalsIgnoreCase(serviceRequestParameter) == true) {
                // User wants a list of operations
                // The parameters required are
                //  - wsdl location
                if((wsdlRequestParameter != null) && (wsdlRequestParameter.trim().length() > 0)) {
                    concatenatedString = support.retrieveOperationsList(wsdlRequestParameter);
                } // end if((wsdlRequestParameter != null) 
            } // end if(REQUEST_SERVICE_OPERATIONSLIST_KEY)
            if(REQUEST_SERVICE_MESSAGETYPES_KEY.equalsIgnoreCase(serviceRequestParameter) == true) {
                // User wants the message types associated with the message
                // The parameters required are
                //  - wsdl location
                //  - portType Name (tns;serviceName;portName;portType)
                //  - operation Name
                if((wsdlRequestParameter != null) && (wsdlRequestParameter.trim().length() > 0)
                && (portTypeRequestParameter != null) && (portTypeRequestParameter.trim().length() > 0)
                && (operationRequestParameter != null) && (operationRequestParameter.trim().length() > 0)
                ) {
                    concatenatedString = support.retrieveMessageTypes(wsdlRequestParameter, portTypeRequestParameter, operationRequestParameter);
                } // end if((wsdlRequestParameter != null) 
            } // end if(REQUEST_SERVICE_MESSAGETYPES_KEY)
        } // end if((serviceRequestParameter != null)
        
        System.out.println("Concatenated String is:"+concatenatedString);
        response.setContentType("text/plain;charset=UTF-8");
        PrintWriter out = response.getWriter();
        out.println(concatenatedString);
        out.close();
    }
    
    /**
     * Parse the String tokens.
     * @param the String to parse
     * @param delimiters
     * @return array of Strings, one for each word in subtoken.
     */
    String[] parseTokens(String inputString, String delimiters) {
        List<String> tokensList = null;
        String token = null;
        boolean returnDelimiters = false;
        StringTokenizer parser = new StringTokenizer(inputString,
                delimiters,
                returnDelimiters);
        while ( true == parser.hasMoreTokens() ) {
            token = parser.nextToken(delimiters);
            if(token != null) {
                tokensList.add(token);
            } else {
                tokensList.add("");
            }
        }
        return toArray(tokensList, String.class);
    }

    /**
     * We can now use this to produce lists of ints or Strings:
     * List<Integer> ints = Lists.toList(1, 2, 3);
     * List<String> names = Lists.toList("Gopalan", "Suresh", "Raj");
     * 
     * @param <T>
     * @param arr
     * @return
     */
    public static <T> List<T> toList(T... array) {
        List<T> list = new ArrayList<T>();
        for (T arrayElement : array) {
        	list.add(arrayElement);
        }
        return list;
    }
    
    /**
     *
     * @param <T>
     * @param collection
     * @param componentType
     * @return
     */
    @SuppressWarnings("unchecked")
    static public <T> T[] toArray(Collection<T> collection,
            Class<T> componentType) {
        // unchecked cast
        T[] array = (T[]) java.lang.reflect.Array.newInstance(componentType,
                collection.size());
        int index = 0;
        for (T value : collection) {
            array[index++] = value;
        }
        return array;
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
        return "Web Service (WSDL/XSD) AJAX Servlet";
    }
    // </editor-fold>
}
