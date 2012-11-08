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
 * @(#)EndpointValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.validator;

import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.Endpoint.EndpointType;
import com.sun.jbi.hl7bc.extensions.HL7Input;
import com.sun.jbi.hl7bc.extensions.HL7Output;
import com.sun.jbi.hl7bc.extensions.HL7Operation;
import com.sun.jbi.hl7bc.I18n;

import java.io.File;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.xml.namespace.QName;

/**
 * This class performs validation for endpoints.
 * 
 * @author S. Nageswara Rao
 */
public class EndpointValidator {
    private static Logger mLogger =  Logger.getLogger(EndpointValidator.class.getName());
     
    public static void validateEndpointForUniqueness(Collection<Endpoint> endpoints, Endpoint aEndpoint, boolean endpointValidated) throws Exception {
        if (!endpointValidated) {
            validateEndpoint(aEndpoint);
        }
    	
    	int endpointType = aEndpoint.getEndpointType();
        String serviceEndpointRef = aEndpoint.getServiceName().toString() + aEndpoint.getEndpointName();
    	
        for (Endpoint aDeployedEndpoint : endpoints) {
            if (aDeployedEndpoint.getEndpointType() != endpointType) {
                continue;
            } else {
				String aDeployedServiceEPRef = aDeployedEndpoint.getServiceName().toString() + aDeployedEndpoint.getEndpointName();
            	if (serviceEndpointRef.equals(aDeployedServiceEPRef)) {
            	    throw new Exception(I18n.msg("E0145: An endpoint with the same service name {0} and endpoint name {1} already exists in the JBI container.", 
            	                       aEndpoint.getServiceName().toString(), aEndpoint.getEndpointName()));
            	}
				
				/*
				 * Set<Map.Entry<QName, HL7Operation>> opEntries =
				 * aEndpoint.getHL7Operations().entrySet(); for (Map.Entry<QName,
				 * HL7Operation> aEntry : opEntries) { String opname =
				 * aEntry.getKey().toString(); HL7Operation aOperation =
				 * aEntry.getValue();
				 * 
				 * 
				 * Set<Map.Entry<QName, HL7Operation>> deployedOpEntries =
				 * aDeployedEndpoint.getHL7Operations().entrySet();
				 * 
				 * for (Map.Entry<QName, HL7Operation> aDeployedEntry :
				 * deployedOpEntries) { String aDeployedOpname =
				 * aDeployedEntry.getKey().toString(); HL7Operation
				 * aDeployeOperation = aDeployedEntry.getValue(); } }
				 */
            }
        }
    }


    public static void validateEndpoint(Endpoint aEndpoint) throws Exception {
    	
    	String serviceEndpointRef = aEndpoint.getServiceName().toString() + aEndpoint.getEndpointName();
            
    	Set<Map.Entry<QName, HL7Operation>> opEntries = aEndpoint.getHL7Operations().entrySet();
    	Map<QName, String> operationMeps = aEndpoint.getOperationMsgExchangePattern();
    	
        if (aEndpoint.getEndpointType() == EndpointType.INBOUND) {
            for (Map.Entry<QName, HL7Operation> aEntry : opEntries) {
                QName opname = aEntry.getKey();
                HL7Operation aOperation = aEntry.getValue();
                
                String mep = (String) operationMeps.get(opname);
                if (mep == null) {
                    throw new Exception(I18n.msg("E0146: Found invalid message exchange pattern for operation {0} in service endpoint {1}", opname.toString(), serviceEndpointRef ));
                }
                
               if (Endpoint.EndpointMessageType.IN_ONLY.equals(mep)) {
                    HL7Input input = aOperation.getHL7OperationInput();
                    if (input == null) {
                        throw new Exception(I18n.msg("E0147: Missing hl7:message mapping for the Input element for operation {0} in service endpoint {1}.", opname.toString(), serviceEndpointRef));
                    }
                } else if(Endpoint.EndpointMessageType.IN_OUT.equals(mep)){
                    HL7Input input = aOperation.getHL7OperationInput();
                    if (input == null) {
                        throw new Exception(I18n.msg("E0147: Missing hl7:message mapping for the Input element for operation {0} in service endpoint {1}.", opname.toString(), serviceEndpointRef));
                    }
                    HL7Output output = aOperation.getHL7OperationOutput();
                    if (output == null) {
                        throw new Exception(I18n.msg("E0148: Missing hl7:message mapping for the Output element for operation {0} in service endpoint {1}.",opname.toString(), serviceEndpointRef));
                    }
                } else {
                    throw new Exception(I18n.msg("E0149: Found an invalid or unsupported message exchange pattern for operation {0} in service endpoint {1}.", opname.toString(), serviceEndpointRef));
                }
            }
        }    
    }
       
}
