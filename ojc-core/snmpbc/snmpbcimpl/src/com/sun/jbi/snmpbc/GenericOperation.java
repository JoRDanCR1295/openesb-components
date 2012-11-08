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
 * @(#)GenericOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.snmpbc.extensions.SNMPAddress;
import com.sun.jbi.snmpbc.extensions.SNMPInput;
import com.sun.jbi.snmpbc.extensions.SNMPOperation;
import com.sun.jbi.snmpbc.extensions.SNMPOutput;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

/**
 * this represents a generic entity that is used to send/receive to/from NMR
 *
 * @author echou
 */
public class GenericOperation {
    
    private QName operationName;
    private Endpoint endpoint;
    private SNMPOperation snmpOp;
    private SNMPInput snmpInput;
    private SNMPOutput snmpOutput;
    
    /** Creates a new instance of GenericOperation 
     * @param operationName 
     * @param endpoint 
     */
    public GenericOperation(QName operationName, Endpoint endpoint) {
        this.operationName = operationName;
        this.endpoint = endpoint;
        this.snmpOp = (SNMPOperation) endpoint.getSNMPOperations().get(operationName);
        this.snmpInput = endpoint.getSNMPOperationInput(snmpOp);
        this.snmpOutput = endpoint.getSNMPOperationOutput(snmpOp);
    }
    
    public Endpoint getEndpoint() {
        return endpoint;
    }
    
    public QName getOperationName() {
        return operationName;
    }
    
    public SNMPOperation getSNMPOperation() {
        return snmpOp;
    }
    
    public SNMPInput getSNMPInput() {
        return snmpInput;
    }
    
    public SNMPOutput getSNMPOutput() {
        return snmpOutput;
    }
    
    public SNMPAddress getSNMPAddress() {
        return endpoint.getSNMPAddress();
    }
    
    public QName getServiceName() {
        return endpoint.getServiceName();
    }
    
    public String getEndpointName() {
        return endpoint.getEndpointName();
    }
    
    public int getEndpointType() {
        return endpoint.getEndpointType();
    }
    
    public Definition getDefinition() {
        return endpoint.getDefinition();
    }
    
    public EndpointStatus getEndpointStatus() {
        return endpoint.getEndpointStatus();
    }
    
}
