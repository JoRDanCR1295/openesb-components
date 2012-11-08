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
 * @(#)Endpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc;

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

/**
 * Endpoint represents the combination of the binding information and the
 * port information.  The WSDL specification basically uses the <port> element
 * to define the physical address of an endpoint, while the <binding> element
 * details how the abstract message should be converted to the representation
 * the External Service Provider understands.  Together, this forms an Endpoint.
 * <p>
 * This interface attempts to encapsulate all that information plus some
 * additional information needed for the actual efficient interchange with the
 * service provider. This information includes:
 * <ul>
 * <li>Binding and Port information - this comes from WSDL represented by this
 * endpoint. In many ways, this interface is a compact representation of the
 * WSDL information in the <binding> and <port> sections of the WSDL.</li>
 * <li>State - this details the lifecycle state of the Endpoint. </li>
 * <li>Message Exchange Pattern - this details the type of communication
 * pattern that this endpoint uses to interact with the service provider.</li>
 * <li>JBI-representation of Endpoint - this interface further specfies
 * the translation from this representation of Endpoint to the JBI
 * representation of an Endpoint, the ServiceEndpoint class.</li>
 * </ul>
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public interface Endpoint {

    /**
     * Defines the different types of Endpoints we're allowed to have.
     */
    public final class EndpointType {
        public static EndpointType INBOUND = new EndpointType("INBOUND");
        public static EndpointType OUTBOUND = new EndpointType("OUTBOUND");

        private EndpointType(final String id) {
        }
    }
    
    /**
     * Defines the different states an Endpoint may be in
     */
    public final class EndpointState {
        public static EndpointState SHUTDOWN = new EndpointState("SHUTDOWN");
        public static EndpointState STOPPED = new EndpointState("STOPPED");
        public static EndpointState RUNNING = new EndpointState("RUNNING");

        private EndpointState(final String id) {
        }
    }


    ////////
    //
    //  Binding and Port Information Methods
    //
    ////////

    QName getServiceName();

    void setServiceName(QName serviceName);

    String getEndpointName();

    void setEndpointName(String endpointName);

    Definition getDefinition();

    void setDefinition(Definition definition);


    ////////
    //
    //  State Information Methods
    //
    ////////

    EndpointState getState();

    void setState(EndpointState state);

    EndpointStatus getEndpointStatus();

    void setEndpointStatus(EndpointStatus status);

    ////////
    //
    //  Message Exchange Pattern Methods
    //
    ////////

    EndpointType getEndpointType();

    void setEndpointType(EndpointType type);

    ////////
    //
    //  JBI-representation of Endpoint
    //
    ////////

    ServiceEndpoint getServiceEndpoint();

    void setServiceEndpoint(ServiceEndpoint serviceEndpoint);

    Document getServiceDescription();

    void setServiceDescription(Document serviceDescription);


    ////////
    //
    //  SMTP-specific Endpoint.  These should probably be refactored
    //  out in a better way so as to maintain their original
    //  hierarchical structure
    //
    ////////

    SMTPAddress getSMTPAddress();
    
    void setSMTPAddress(SMTPAddress address);

    SMTPBinding getSMTPBinding();

    void setSMTPBinding(SMTPBinding binding);

//     Map<QName, SMTPOperation> getSMTPOperations();

//     void setSMTPOperations(Map<QName, SMTPOperation> operations);

    Map getSMTPOperations();

    void setSMTPOperations(Map operations);

    SMTPOperationInput getSMTPOperationInput(SMTPOperation operation);

    void setSMTPOperationInput(SMTPOperation operation,
                               SMTPOperationInput operationInput);

    SMTPOperationOutput getSMTPOperationOutput(SMTPOperation operation);

    void setSMTPOperationOutput(SMTPOperation operation,
                                SMTPOperationOutput operationOutput);
    
    void setMessagePartEncoderMapping(Map partMapping);
    
    Map getMessagePartEncoderMapping();

}
