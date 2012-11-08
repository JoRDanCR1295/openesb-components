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
package com.sun.jbi.filebc;

import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.extensions.FileBinding;
import java.util.Map;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

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
 */
public interface Endpoint {

    /**
     * Defines the different types of Endpoints we're allowed to have.
     */
    public interface EndpointType {

        public static final int INBOUND = 0;    // BC is consumer (external proxy consumer)
        public static final int OUTBOUND = 1;   // BC is provider (external proxy provider)
    }

    /**
     * Defines the different states an Endpoint may be in
     */
    public interface EndpointState {

        public static final int SHUTDOWN = 0;
        public static final int STOPPED = 1;
        public static final int RUNNING = 2;
        public static final int STARTED = 3;
    }

    /**
     * Defines the message exchange pattern
     */
    public interface EndpointMessageType {

        public static final String IN_OUT = "inout";
        public static final String IN_ONLY = "inonly";
        public static final String OUT_IN = "outin";
        public static final String OUT_ONLY = "outonly";
        public static final String UNSUPPORTED = "unsupported";
    }

    String getServiceUnitID();

    void setServiceUnitID(String id);

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
    int getState();

    void setState(int state);

    EndpointStatus getEndpointStatus();

    void setEndpointStatus(EndpointStatus status);

    ////////
    //
    //  Outbound (provider) or inbound (consumer) - with respect to SE
    //
    ////////
    int getEndpointType();

    void setEndpointType(int type);

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
    //  File Binding specific Endpoint.
    //
    ////////
    FileAddress getFileAddress();

    void setFileAddress(FileAddress address);

    String getWorkAreaDir();

    void setWorkAreaDir(String workAreaDir);

    FileBinding getFileBinding();

    void setFileBinding(FileBinding binding);

    Map getFileOperations();

    void setFileOperations(Map operations);

    void setOperationMsgExchangePattern(Map opMEPs);

    Map getOperationMsgExchangePattern();

    void setMessagePartEncoderMapping(Map partMapping);

    Map getMessagePartEncoderMapping();

    RedeliveryConfig getRedeliveryConfiguration();

    void setRedeliveryConfiguration(RedeliveryConfig redeliveryConfig);

    long getCRMPMessageId();

    int getMaxConcurrencyLimit();

    void setMaxConcurrencyLimit(int maxConcurrencyLimit);

    String getUniqueName();

    String getEPUUID();
}
