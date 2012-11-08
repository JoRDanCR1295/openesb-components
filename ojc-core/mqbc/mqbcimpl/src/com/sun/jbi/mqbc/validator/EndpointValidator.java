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

package com.sun.jbi.mqbc.validator;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import javax.xml.namespace.QName;

import com.sun.jbi.mqbc.Endpoint;
import com.sun.jbi.mqbc.Endpoint.EndpointType;
import com.sun.jbi.mqbc.I18n;
import com.sun.jbi.mqbc.extensions.MQBCBody;
import com.sun.jbi.mqbc.extensions.MQBCOperation;
import com.sun.jbi.mqbc.extensions.MQInput;
import com.sun.jbi.mqbc.extensions.MQOutput;

/**
 * This class performs validation for endpoints.
 *
 * @author Sherry Weng
 */
public class EndpointValidator {
    
    public static void validateEndpointForUniqueness(Collection<Endpoint> endpoints,
                                                     Endpoint aEndpoint,
                                                     boolean endpointValidated)
            throws Exception {
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
                    throw new Exception(I18n.msg(
                            "9000: The service endpoint {0} is already deployed.",
                            aDeployedServiceEPRef));
                }
                
                
                Set<Map.Entry<QName, MQBCOperation>> opEntries = aEndpoint.getMQOperations().entrySet();
                for (Map.Entry<QName, MQBCOperation> aEntry : opEntries) {
                    String opname = aEntry.getKey().toString();
                    MQBCOperation aOperation = aEntry.getValue();
                   
                    
                    Set<Map.Entry<QName, MQBCOperation>> deployedOpEntries = aDeployedEndpoint.getMQOperations().entrySet();
                    
                    for (Map.Entry<QName, MQBCOperation> aDeployedEntry : deployedOpEntries) {
                        String aDeployedOpname = aDeployedEntry.getKey().toString();
                        MQBCOperation aDeployeOperation = aDeployedEntry.getValue();
                        
                        
                        
                        
                    }
                }
            }
        }
    }
    
    
    public static void validateEndpoint(Endpoint aEndpoint) throws Exception {
        if (aEndpoint.getEndpointType() == EndpointType.OUTBOUND) {
            validateOutboundEndpoint(aEndpoint);
        }
        
        if (aEndpoint.getEndpointType() == EndpointType.INBOUND) {
            validateInboundEndpoint(aEndpoint);
        }
    }

    private static void validateOutboundEndpoint(Endpoint endpoint)
            throws InvalidEndpointException {
        String serviceEndpointRef = endpoint.getServiceName().toString() + endpoint
                .getEndpointName();
        Set<Map.Entry<QName, MQBCOperation>> opEntries = endpoint.getMQOperations().entrySet();
        Map<QName, String> operationMeps = endpoint.getOperationMsgExchangePattern();
        
        for (Map.Entry<QName, MQBCOperation> aEntry : opEntries) {
            QName opname = aEntry.getKey();
            MQBCOperation aOperation = aEntry.getValue();
            String mep = operationMeps.get(opname);
            if (mep == null) {
                throw new InvalidEndpointException(I18n.msg(
                        "9001: Unknown operation {0} encountered,"
                                + " undefined for endpoint {1}",
                        opname.toString(),
                        serviceEndpointRef
                )
                );
            }
            if (Endpoint.EndpointMessageType.IN_OUT.equals(mep)) {
                // InOut pattern + outbound mode can only mean
                // solicit-read or MQ non-polling "get".
                // The output's mq:body must have a messageBody attribute.
                // The input's mq:body SHOULD NOT have one, but if present,
                // it is meaningless, so I will just ignore it.
                MQInput input = aOperation.getMQOperationInput();
                MQOutput output = aOperation.getMQOperationOutput();
                if (input == null) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9002: MQ-bound Request-Response operation {0}"
                                    + " requires an MQ input binding.",
                            opname.toString()
                    )
                    );
                }
                if (output == null) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9004: MQ-bound Request-Response operation {0}"
                                    + " requires an MQ output binding.",
                            opname.toString()
                    )
                    );
                }
                MQBCBody inputBody = input.getMQMessage();
                if (inputBody == null) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9006: Input of operation {0} requires"
                                    + " an MQ body binding.",
                            opname.toString()
                    )
                    );
                }
                MQBCBody outputBody = output.getMQMessage();
                if (outputBody == null) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9008: Output of operation {0} requires"
                                    + " an MQ body binding.",
                            opname.toString()
                    )
                    );
                }
                String outputBodyPart = outputBody.getMQMessageBody();
                if (outputBodyPart == null || "".equals(outputBodyPart)) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9010: Output of operation {0}"
                                    + " requires an MQ body binding that"
                                    + " specifies a ''messageBody''.",
                            opname.toString()
                    )
                    );
                }
            } else if (Endpoint.EndpointMessageType.IN_ONLY.equals(mep)) {
                // InOnly pattern for the outbound mode can mean only
                // MQ "put" semantics. This is indicated by
                // an input whose mq:body has a messageBody attribute.
                MQInput input = aOperation.getMQOperationInput();
                if (input == null) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9012: MQ-bound One-Way operation {0}"
                                    + " requires an MQ input binding.",
                            opname.toString()
                    )
                    );
                }
                MQBCBody body = input.getMQMessage();
                if (body == null) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9006: Input of operation {0} requires"
                                    + " an MQ body binding.",
                            opname.toString()
                    )
                    );
                }
                String bodyPartName = body.getMQMessageBody();
                if (bodyPartName == null || "".equals(bodyPartName)) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9014: Input of operation {0}"
                                    + " requires an MQ body binding that"
                                    + " specifies a ''messageBody''.",
                            opname.toString()
                    )
                    );
                }
            }
        }
    }

    private static void validateInboundEndpoint(Endpoint endpoint)
            throws InvalidEndpointException {
        String serviceEndpointRef = endpoint.getServiceName().toString() + endpoint
                .getEndpointName();
        Set<Map.Entry<QName, MQBCOperation>> opEntries = endpoint.getMQOperations().entrySet();
        Map<QName, String> operationMeps = endpoint.getOperationMsgExchangePattern();
        
        for (Map.Entry<QName, MQBCOperation> aEntry : opEntries) {
            QName opname = aEntry.getKey();
            MQBCOperation aOperation = aEntry.getValue();
            String mep = operationMeps.get(opname);
            if (mep == null) {
                throw new InvalidEndpointException(I18n.msg(
                        "9001: Unknown operation {0} encountered,"
                                + " undefined for endpoint {1}",
                        opname.toString(),
                        serviceEndpointRef
                )
                );
            }
                
            if (Endpoint.EndpointMessageType.IN_OUT.equals(mep)) {
                // InOut not supported in the inbound mode
                throw new InvalidEndpointException(I18n.msg(
                        "9005: MQ-bound Request-Response operation {0} not"
                                + " supported when deployed as an"
                                + " inbound endpoint: {1}.",
                        opname.toString(),
                        serviceEndpointRef
                )
                );
            } else if (Endpoint.EndpointMessageType.IN_ONLY.equals(mep)) {
                // InOnly pattern for the inbound mode can mean only
                // MQ "get" semantics, polling-style. This is indicated by
                // an input whose mq:body has a messageBody attribute.
                MQInput input = aOperation.getMQOperationInput();
                if (input == null) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9012: MQ-bound One-Way operation {0}"
                                    + " requires an MQ input binding.",
                            opname.toString()
                    )
                    );
                }
                MQBCBody body = input.getMQMessage();
                if (body == null) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9006: Input of operation {0} requires"
                                    + " an MQ body binding.",
                            opname.toString()
                    )
                    );
                }
                String bodyPartName = body.getMQMessageBody();
                if (bodyPartName == null || "".equals(bodyPartName)) {
                    throw new InvalidEndpointException(I18n.msg(
                            "9014: Input of operation {0}"
                                    + " requires an MQ body binding that"
                                    + " specifies a ''messageBody''.",
                            opname.toString()
                    )
                    );
                }
                   
            } else {
                throw new InvalidEndpointException(I18n.msg(
                        "9003: Unrecognized message exchange pattern,"
                                + " ''{0}'' for operation {1} endpoint {2}",
                        mep,
                        opname.toString(),
                        serviceEndpointRef));
            }
        }
    }

}
