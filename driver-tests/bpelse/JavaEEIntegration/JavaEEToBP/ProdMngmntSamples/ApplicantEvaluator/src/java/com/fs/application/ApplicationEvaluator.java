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
 * @(#)ApplicationEvaluator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.fs.application;

//import com.fs.customer.PerformanceEvaluatorRemote;
import com.fico.credit.FICOSimulatorService;
import com.fs.customer.PerformanceEvaluatorRemote;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author gpatil
 */
@Stateless
@WebService(serviceName = "ApplicantEvaluatorService", portName = "ApplicantEvaluatorPort", endpointInterface = "com.fs.application.ApplicantEvaluator", targetNamespace = "http://application.fs.com/", wsdlLocation = "META-INF/wsdl/ApplicationEvaluator/ApplicantEvaluator.wsdl")
public class ApplicationEvaluator implements ApplicantEvaluator {

    @EJB
    private PerformanceEvaluatorRemote performanceEvaluatorBean;

    @WebServiceRef(wsdlLocation = "http://localhost:8080/FICOSimulatorServiceService/FICOSimulatorService?wsdl")
    private com.fico.credit.FICOSimulatorServiceService service;

    /** Creates a new instance of ApplicationEvaluator */
    public ApplicationEvaluator() {
    }

    public String getApplicantReport(String socialSecurityNumber, String firstName, String lastName) {
        System.out.println("ApplicantEvaluator::getApplicantReport start");
        String result = null;
        final String SEPARATOR = "-";
        String internalResult = "", externalResult = "";
        
        try { 
            // Call Web Service Operation
            FICOSimulatorService port = service.getFICOSimulatorServicePort();
            externalResult = port.getCreditReport(socialSecurityNumber, firstName, lastName);
            System.out.println("ApplicantEvaluator::getApplicantReport::externalResult: "+externalResult);
            //externalResult = "720" ;
            //System.out.println("Due to bug, commented call to WS FICOSimulator:ApplicantEvaluator::getApplicantReport::externalResult: "+externalResult);
            
            
            // Call EJB 
            internalResult = performanceEvaluatorBean.getCustomerReport(socialSecurityNumber, null, null, firstName, lastName );
            System.out.println("ApplicantEvaluator::getApplicantReport::internalResult: "+internalResult);
//            internalResult = "low|1705" ;
//            System.out.println("Due to bug, commented call to ApplicantEvaluator::getApplicantReport::internalResult: "+internalResult);
            result = internalResult+SEPARATOR+externalResult;
            System.out.println("ApplicantEvaluator::getApplicantReport end");
            return result;
        } catch (Exception ex) {
            Logger.getLogger(this.getClass().getName()).log(Level.SEVERE, "Exception while getting credit report.", ex);
        }
        
        return result;
    }
    
}
