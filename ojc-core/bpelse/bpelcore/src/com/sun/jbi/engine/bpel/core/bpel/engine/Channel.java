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
 * @(#)Channel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;

import org.w3c.dom.DocumentFragment;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import javax.jbi.servicedesc.ServiceEndpoint;


/**
 * Channel interface
 *
 * @author Sun Microsystems
 */
public interface Channel {
    /**
     * reply
     *
     * @param msgContainer message container
     */
    void reply(MessageContainer msgContainer);

    /**
     * The method can be called to make an invocation to a particular partner as 
     * identified by the partnerLink and the operation.
     *
     * @param msgContainer The message container
     * @param partnerLink The partnerLink definition which identifies 
     * the endpoint which will needs to be called
     * @param operation The name of the operation
     * @param oneWay Flag to inicate whether it is a oneway invoke
     * @param process The process definition for the invoke.
     *
     * @return Object The result of the invoke
     */
    Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink,
        QName operation, boolean oneWay, RBPELProcess process);

    /**
     * sends Fault
     *
     * @param faultName fautl defined on WSDL operation
     * @param msgContainer message container
     */
    void sendFault(QName faultName, MessageContainer msgContainer);
    
    /** send message exchange after setting with error
     * 
     * @param msgExchangeId Message ExchangeId
     * @param error an Exception object representing process error
     */
    public void sendRequestError(String msgExchangeId, Exception error);

    /** send message exchange after setting status to done
     * 
     * @param msgExchangeId Message ExchangeId
     */
    public void sendInOnlyRequestDoneStatus(String msgExchangeId);

    /**
     * Send done status for the response from the server for the invoke from
     * this SE as a client.
     * 
     * @param msgExchangeId
     *            Message ExchangeId
     */
    public  void sendResponseDoneStatus(String msgExchangeId);

    /**
     * Send error status for the response from the server for the invoke from
     * this SE as a client.
     * 
     * @param msgExchangeId
     *            Message ExchangeId
     * @param ex
     *            exception
     */
    public  void sendResponseErrorStatus(String msgExchangeId, Exception ex);

    /** For the partnerLink's myRole we would like to get the registered document fragment 
     * of the concrete binding.
     * @param partnerLink
     * @return
     */
    public DocumentFragment getExternalEndPoint(PartnerLink partnerLink);

    public EPReferenceComposer getEPRComposer();

    public DocumentFragment getEndpointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole);

    public ServiceEndpoint resolveEndpointReference(DocumentFragment docFrag);

    public void sendKPIMEx(QName interfaceName, String operation, String doc) throws MessagingException;

    public void installServiceQualities(String suName, String suPath)throws DeploymentException;
    public void uninstallServiceQualities(String suName);

}
