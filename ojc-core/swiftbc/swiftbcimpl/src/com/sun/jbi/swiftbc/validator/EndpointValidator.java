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

package com.sun.jbi.swiftbc.validator;

import com.sun.jbi.swiftbc.Endpoint;
import com.sun.jbi.swiftbc.Endpoint.EndpointType;
import com.sun.jbi.swiftbc.extensions.SwiftOperation;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.extensions.SwiftInput;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

/**
 * This class performs validation for endpoints.
 *
 * @author S. Nageswara Rao
 */
public class EndpointValidator {
    private static final Messages mMessages = 
        Messages.getMessages(EndpointValidator.class);
    private static Logger mLogger = Messages.getLogger(EndpointValidator.class);
     
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
            	if (serviceEndpointRef.equals(aDeployedEndpoint.getServiceName().toString() + aDeployedEndpoint.getEndpointName())) {
            	    throw new Exception(mMessages.getString("EPV_endpoint_already_exists", 
            	                       new String[] {aEndpoint.getServiceName().toString(), aEndpoint.getEndpointName()}));
            	}
            }
        }
    }


    public static void validateEndpoint(Endpoint aEndpoint) throws Exception {
    	
    	String serviceEndpointRef = aEndpoint.getServiceName().toString() + aEndpoint.getEndpointName();
            
    	Set<Map.Entry<QName, SwiftOperation>> opEntries = aEndpoint.getSwiftOperations().entrySet();
    	Map<QName, String> operationMeps = aEndpoint.getOperationMsgExchangePattern();
    	
        if (aEndpoint.getEndpointType() == EndpointType.INBOUND || aEndpoint.getEndpointType() == EndpointType.OUTBOUND) {
            for (Map.Entry<QName, SwiftOperation> aEntry : opEntries) {
                QName opname = aEntry.getKey();
                SwiftOperation aOperation = aEntry.getValue();
                
                String mep = (String) operationMeps.get(opname);
                if (mep == null) {
                    throw new Exception(mMessages.getString("EPV_Invalid_operation_mep", new String [] {opname.toString(), serviceEndpointRef} ));
                }
                
               if (Endpoint.EndpointMessageType.IN_ONLY.equals(mep) || Endpoint.EndpointMessageType.OUT_ONLY.equals(mep)
            		   || Endpoint.EndpointMessageType.IN_OUT.equals(mep)) {
                    SwiftInput input = aOperation.getSwiftOperationInput();
                    
                    if (input == null) {
                        throw new Exception(mMessages.getString("EPV_Invalid_missing_input", new String [] {opname.toString(), serviceEndpointRef}));
                    }
                } else {
                    throw new Exception(mMessages.getString("EPV_Invalid_MEP", new String [] {opname.toString(), serviceEndpointRef}));
                }
            }
        }    
    }
       
}
