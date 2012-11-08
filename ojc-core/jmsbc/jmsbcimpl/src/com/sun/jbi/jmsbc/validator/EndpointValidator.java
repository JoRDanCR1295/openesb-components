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

package com.sun.jbi.jmsbc.validator;

import com.sun.jbi.jmsbc.Endpoint;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.internationalization.Messages;

import com.codestreet.selector.Selector;
import com.codestreet.selector.parser.InvalidSelectorException;

import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

/**
 * This class performs validation for endpoints.
 *
 * @author Sun
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
            	if (endpointType==Endpoint.EndpointType.INBOUND && serviceEndpointRef.equals(aDeployedEndpoint.getServiceName().toString() + aDeployedEndpoint.getEndpointName())) {
            	    throw new EndpointValidationException(
                            mMessages.getString("JMSBC-E0601.EndpointAlreadyExists",
            	               new String[] {aEndpoint.getEndpointName(), 
                                  aEndpoint.getServiceName().toString()}));
            	}                                
            }
        }
    }


    public static void validateEndpoint(Endpoint aEndpoint) throws Exception {
        //
        // Do not allow provisioning endpoint which has an in/out operation type marked as xa
        //
        if (aEndpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND) {
            StringBuffer provInOutXAErrorMsgs = new StringBuffer();
            boolean hasProvInOutXAOp = false;
            Iterator jmsOpsIter = aEndpoint.getJMSOperations().keySet().iterator();
            while (jmsOpsIter.hasNext()) {
                QName bindingOpQName = (QName)jmsOpsIter.next();
                JMSOperation jmsOp = (JMSOperation) aEndpoint.getJMSOperations().get(bindingOpQName);
                if (jmsOp.getMEP().equals(Endpoint.EndpointMessageType.IN_OUT) && 
                    jmsOp.getTransaction().equals(JMSConstants.TRANSACTION_XA) &&
                    !jmsOp.getVerb().equals(JMSOperation.VERB_READ)) {
                    hasProvInOutXAOp = true;
                    provInOutXAErrorMsgs.append(mMessages.getString("JMSBC-E0602.ProvisioningInOutXA", 
                                                    new String[] {aEndpoint.getEndpointName(),
                                                                  aEndpoint.getServiceName().toString(),
                                                                  bindingOpQName.toString()})).append("\n");
                }
            }
            if (hasProvInOutXAOp) {
                throw new EndpointValidationException (provInOutXAErrorMsgs.toString());
            }
        } else {
            StringBuffer msgSelectorErrorMsgs = new StringBuffer();
            boolean hasMsgSelectorErr = false;
            
            Iterator jmsOpsIter = aEndpoint.getJMSOperations().keySet().iterator();
            while (jmsOpsIter.hasNext()) {
                QName bindingOpQName = (QName)jmsOpsIter.next();
                JMSOperation jmsOp = (JMSOperation) aEndpoint.getJMSOperations().get(bindingOpQName);
                String messageSelector = jmsOp.getMessageSelector();
                if (messageSelector != null && messageSelector.length() > 0 && jmsOp.getValidateMessageSelector()) {
                    try {
                        Selector sel = Selector.getInstance(messageSelector);
                    } catch (InvalidSelectorException ex) {
                        hasMsgSelectorErr = true;
                        msgSelectorErrorMsgs.append(mMessages.getString("JMSBC-E0603.InvalidMessageSelector", 
                                                        new String[] {aEndpoint.getEndpointName(),
                                                                      aEndpoint.getServiceName().toString(),                                                                                      
                                                                      bindingOpQName.toString(),
                                                                      messageSelector,
                                                                      ex.getLocalizedMessage()})).append("\n");
                    }
                }
            }
            if (hasMsgSelectorErr) {
                throw new EndpointValidationException (msgSelectorErrorMsgs.toString());
            }
        }        
    }              
}
