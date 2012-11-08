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
 * @(#)EndpointImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.smtpbc.extensions.SMTPAddress;
import com.sun.jbi.smtpbc.extensions.SMTPBinding;
import com.sun.jbi.smtpbc.extensions.SMTPOperation;
import com.sun.jbi.smtpbc.extensions.SMTPOperationInput;
import com.sun.jbi.smtpbc.extensions.SMTPOperationOutput;


public class EndpointImpl
    implements Endpoint, Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private QName mServiceName;
    private String mEndpointName;
    private Definition mDefinition;
    
    private EndpointState mEndpointState;
    private EndpointStatus mEndpointStatus;
        
    private EndpointType mEndpointType;
    
    private ServiceEndpoint mServiceEndpoint;
    private Document mServiceDescription;
    
    private SMTPAddress mSMTPAddress;
    private SMTPBinding mSMTPBinding;
    private Map mOperations;
    private Map mOperationInput;
    private Map mOperationOutput;
    
	private Map mPartMapping;

    public EndpointImpl() {    
        mOperationInput = new HashMap();
        mOperationOutput = new HashMap();
    }

    ////////
    //
    //  Binding and Port Information Methods
    //
    ////////

    public QName getServiceName() {
        return mServiceName;
    }

    public void setServiceName(final QName serviceName) {
        mServiceName = serviceName;
    }

    public String getEndpointName() {
        return mEndpointName;
    }

    public void setEndpointName(final String endpointName) {
        mEndpointName = endpointName;
    }

    public Definition getDefinition() {
        return mDefinition;
    }

    public void setDefinition(final Definition definition) {
        mDefinition = definition;
    }

    ////////
    //
    //  State Information Methods
    //
    ////////

    public EndpointState getState() {
        return mEndpointState;
    }

    public void setState(final EndpointState state) {
        mEndpointState = state;
    }

    public void setEndpointStatus(final EndpointStatus val) {
        mEndpointStatus = val;
    }

    public EndpointStatus getEndpointStatus() {
        return mEndpointStatus;
    }

    ////////
    //
    //  Message Exchange Pattern Methods
    //
    ////////

    public EndpointType getEndpointType() {
        return mEndpointType;
    }

    public void setEndpointType(final EndpointType type) {
        mEndpointType = type;
    }

    ////////
    //
    //  JBI-representation of Endpoint
    //
    ////////

    public ServiceEndpoint getServiceEndpoint() {
        return mServiceEndpoint;
    }

    public void setServiceEndpoint(final ServiceEndpoint serviceEndpoint) {
        mServiceEndpoint = serviceEndpoint;
    }

    public Document getServiceDescription() {
        return mServiceDescription;
    }

    public void setServiceDescription(final Document serviceDescription) {
        mServiceDescription = serviceDescription;
    }
    
    ////////
    //
    //  SMTP-specific Endpoint.  These should probably be refactored
    //  out in a better way so as to maintain their original
    //  hierarchical structure
    //
    ////////

    public SMTPAddress getSMTPAddress() {
        return mSMTPAddress;
    }
    
    public void setSMTPAddress(final SMTPAddress address) {
        mSMTPAddress = address;
    }

    public SMTPBinding getSMTPBinding() {
        return mSMTPBinding;
    }

    public void setSMTPBinding(final SMTPBinding binding) {
        mSMTPBinding = binding;
    }

    public Map getSMTPOperations() {
        return mOperations;
    }

    public void setSMTPOperations(final Map operations) {
        mOperations = operations;
    }

    public SMTPOperationInput getSMTPOperationInput(final SMTPOperation operation) {
        return (SMTPOperationInput)mOperationInput.get(operation);
    }

    public void setSMTPOperationInput(final SMTPOperation operation,
                                      final SMTPOperationInput operationInput) {
        mOperationInput.put(operation, operationInput);
    }

    public SMTPOperationOutput getSMTPOperationOutput(final SMTPOperation operation) {
        return (SMTPOperationOutput)mOperationOutput.get(operation);
    }

    public void setSMTPOperationOutput(final SMTPOperation operation,
                                       final SMTPOperationOutput operationOutput) {
        mOperationOutput.put(operation, operationOutput);
    }
    
	public void setMessagePartEncoderMapping(final Map partMapping) {
        mPartMapping = partMapping;
    }
    
    public Map getMessagePartEncoderMapping() {
        return mPartMapping;
    }
}
