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
 * @(#)EngineChannel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * Portions Copyright 2011 IntegratedApps LLC.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import com.sun.jbi.engine.bpel.core.bpel.engine.EPReferenceComposer;
import java.io.StringReader;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.engine.bpel.core.bpel.engine.Channel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimePartnerLinkImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationConfigManager;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext;
import com.sun.jbi.engine.bpel.util.I18n;
import com.sun.jbi.systemic.quality.propagation.api.CorrelatedMessageExchangeException;
import com.sun.jbi.systemic.quality.propagation.api.ParentChildExchangeCorrelator;
import com.sun.jbi.systemic.quality.propagation.api.ParentChildExchangeCorrelatorFactory;

/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 * @author Kir Sorokin, kir.sorokin@integrated-apps.com
 * @version 
 */
class EngineChannel implements Channel {
    private static final Logger LOGGER = Logger.getLogger(EngineChannel.class.getName());
    private final BPELSEHelper mBPELSEHelper;
    private final EPReferenceComposer mEPRComposer;

    /**
     * DOCUMENT ME!
     *
     * @param thread
     */
    public EngineChannel(BPELSEHelper bpelsehelper) {
        mBPELSEHelper = bpelsehelper;
        mEPRComposer = new EPReferenceComposerImpl(bpelsehelper);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#sendInOnlyRequestDoneStatus(java.lang.String)
     */
    public void sendInOnlyRequestDoneStatus(String msgExchangeId) {

        InOnly inOnly = (InOnly) (mBPELSEHelper.removefromRequestMap(msgExchangeId));

        String exchangeId = inOnly.getExchangeId();
        try {
            inOnly.setStatus(ExchangeStatus.DONE);

            if (LOGGER.isLoggable(Level.FINE)) {
            	LOGGER.log(Level.FINE, I18n.loc("BPJBI-3015: Sending status for {0}. Status: {1}.", 
            			inOnly.getExchangeId(), inOnly.getStatus()));
            }

            mBPELSEHelper.getDeliveryChannel().send(inOnly);

        } catch (MessagingException e) {
            LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6017: Exception occurred while sending an InOnly message " + 
            		"with status {0} to the NMR. MessageExchangeId is {1}", inOnly.getStatus(), exchangeId), e);
            throw new RuntimeException(e);
        }
    }    
    
    /**
     * @see Channel#sendResponseDoneStatus(com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer,
     *      Exception)
     */
    public void sendResponseDoneStatus(String msgExchangeId) {
        MessageExchange me = (MessageExchange) (mBPELSEHelper.removefromResponseMap(msgExchangeId));
        try {
            me.setStatus(ExchangeStatus.DONE);
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPJBI-3016: Sending Invoke Status for {0}. Status: {1}", 
                		me.getExchangeId(), me.getStatus()));
            }
            mBPELSEHelper.getDeliveryChannel().send(me);
            
        } catch (MessagingException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6033: Unable to set status on ME", ex));
            throw new RuntimeException(ex);
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#sendResponseErrorStatus(java.lang.String,
     *      java.lang.Exception)
     */
    public void sendResponseErrorStatus(String msgExchangeId, Exception error) {
        MessageExchange me = (MessageExchange) (mBPELSEHelper.removefromResponseMap(msgExchangeId));
        mBPELSEHelper.sendError(me, error);
    }
    
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#reply(
     *      com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public  void reply(MessageContainer msgContainer) {
        WSMessage wsMessage = (WSMessage) msgContainer.getContent();
        Document jbiMessageDoc = wsMessage.getDocument();
        InOut inOut = (InOut) (mBPELSEHelper.removefromRequestMap(msgContainer.getId()));

        try {
            NormalizedMessage response = inOut.createMessage();
            
            setContentAttachsNMProps(response, jbiMessageDoc, wsMessage);
           
            inOut.setOutMessage(response);

            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPJBI-3017: Sending Reply for MessageEx id {0}", 
                		inOut.getExchangeId()));
            }
            if (LOGGER.isLoggable(Level.FINEST)) {
            	LOGGER.log(Level.FINEST, I18n.loc("BPJBI-3018: The contents of the message are : {0}", wsMessage));
            }

            mBPELSEHelper.getDeliveryChannel().send(inOut);
            //ends measurement counter for InOut request processing
            msgContainer.endMeasurement();

        } catch (MessagingException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6002: Unable to reply to an InOut"), ex);
            throw new RuntimeException(ex);
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#invoke
     * (com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer, QName, 
     * QName, boolean, com.sun.jbi.engine.bpel.core.bpel.model.meta.RBPELProcess)
     */
    public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, 
            QName operation, boolean oneWay, RBPELProcess process) {

        //Find out service endpoint that needs to be called.
        Object epr = partnerLink.getServiceRef();
        RuntimePartnerLink.InternalEPR epr2 = null;
        QName serviceName;
        String endpointName;
        ServiceEndpoint endpointReference = null;
        if (epr instanceof RuntimePartnerLink.InternalEPR) {
            epr2 = (RuntimePartnerLink.InternalEPR) epr;
            //TODOvb in case correctly resolved partner internalEPR the partners sevicename and endpoint name are different then getted from InternalEPR
            // probably it would be nice to use them
            serviceName = epr2.getService();
            endpointName = epr2.getEndPoint();

            endpointReference = mEPRComposer.resolveEPR(partnerLink.getStaticModel(), epr2);
            if (endpointReference == null) {
                String errorMessage = I18n.loc("BPJBI-6018: EndPoint Reference is not available from the JBI " +
                                "corresponding to the service name {0} and endpoint name {1}", serviceName, endpointName);
                LOGGER.log(Level.WARNING, errorMessage);
                throw new RuntimeException (errorMessage);
            }
        } else {
            assert epr instanceof DocumentFragment;
            DocumentFragment frag = (DocumentFragment) epr;
            endpointReference = mEPRComposer.resolveEPR(frag);

            if (endpointReference == null) {
                String str = DOMHelper.createXmlString(frag);
                String errorMessage = I18n.loc("BPJBI-6019: unable to resolve the service endpoint dynamically " + 
                		"for the document fragment \n {0}", str);
                LOGGER.log(Level.SEVERE, errorMessage);
                throw new RuntimeException(errorMessage);
            }
            if (LOGGER.isLoggable(Level.FINE)) {
                String str = DOMHelper.createXmlString(frag);
                LOGGER.log(Level.FINE, I18n.loc(
                        "Dynamic EPR invocation EPR: {0} resolved to Service Endpoint: {1}Service Name: {2} Endpoint Name:",
                        str, endpointReference, endpointReference.getServiceName(), endpointReference.getEndpointName()));
            }

            epr2 = RuntimePartnerLinkImpl.getEPR(partnerLink.getStaticModel(), false);
            serviceName = epr2.getService();
            endpointName = epr2.getEndPoint();
        }
        
        String meId;
        if (oneWay) {
            meId = oneWayInvoke(msgContainer, endpointReference, 
                    serviceName, endpointName, operation, process);
        } else {
            meId = twoWayInvoke(msgContainer, endpointReference, 
                    serviceName, endpointName, operation, process);
        }

        return meId;
    }
    
    private String oneWayInvoke(MessageContainer msgContainer, ServiceEndpoint endpointReference, 
            QName serviceName, String endpointName, QName operation, RBPELProcess process) {
        
        try {
            String msgExchangeId = null;
            
            InOnly inOnly = (mBPELSEHelper.getExchangeFactory()).createInOnlyExchange();
            
            // set the CRMPInvokeId for custom reliable message exchange as a property. for InOnly, it is only used to relate invoker bp to invokee bp
            String prop = ServiceQuality.MESSAGE_ID;
            Object propValue = msgContainer.getCRMPInvokeId(); 
            inOnly.setProperty(prop, propValue);
            
            // set the ServiceQuality.MESSAGE_ID, the unique id for 
            // Quality of service requirements.
            inOnly.setProperty(ServiceQuality.MESSAGE_ID, propValue);
            
            inOnly.setEndpoint(endpointReference);
//TODOvb check the endpoint info
            //TODO: Should we set the service name since we are already setting the
            //endpoint name? What about the interface name? 05/21/07
//            inOnly.setService(serviceName);
            // TODO this is optional, but today our components can't work if we don't specify this!!
            inOnly.setOperation(operation);
            
            WSMessage wsMessage = (WSMessage) msgContainer.getContent();
            Document jbiMessageDoc = wsMessage.getDocument();
            
            NormalizedMessage nmsg = inOnly.createMessage();
            
            setContentAttachsNMProps(nmsg, jbiMessageDoc, wsMessage);
                        
//            Transaction transaction = msgContainer.getTransaction();
//            if (transaction != null) {
//                inOnly.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, transaction);
//            }

            inOnly.setInMessage(nmsg);
            assignChildExchange(msgContainer, inOnly);
            
            if (msgContainer.getTransaction() != null && msgContainer.isTxAtomicStarted()) {
            	inOnly.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, msgContainer.getTransaction());
            }
            
            msgExchangeId = inOnly.getExchangeId();
            mBPELSEHelper.putinResponseMap(msgExchangeId, process);                
            
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, 
                		I18n.loc("BPJBI-3019: Sending a one way outbound message to the NMR. MessageExchangeId is " + 
                				"{0}, service name is {1}, endpoint name is {2}.", 
                				msgExchangeId, serviceName, endpointName));
            }
            if (LOGGER.isLoggable(Level.FINEST)) {
            	LOGGER.log(Level.FINEST, I18n.loc("BPJBI-3018: The contents of the message are : {0}", wsMessage));
                Map<String, Object> nmProps = wsMessage.getNMProperties();
                if (nmProps != null) {
                    String nmProperties = "NM Properties: \n";
                    for (String key : nmProps.keySet()) {
                        Object val = nmProps.get(key);
                        nmProperties = I18n.loc("Property: {0} value: {1} \n", key, val);
                    }
                    LOGGER.log(Level.FINEST, nmProperties);
                }
            }
            
            try {
                mBPELSEHelper.getDeliveryChannel().send(inOnly);
            } catch(Exception e) {
                mBPELSEHelper.removefromResponseMap(msgExchangeId);                    
                throw e;
            }
            
            return msgExchangeId;
            
        } catch (Exception e) {
            String msg = I18n.loc("BPJBI-6020: Caught exception when invoking One way external webservice");
            LOGGER.log(Level.SEVERE, msg, e);
            throw new RuntimeException(msg, e);
        }
    }    
    
    private String twoWayInvoke(MessageContainer msgContainer,
            ServiceEndpoint endpointReference, QName serviceName, String endpointName, 
            QName operation, RBPELProcess process) {
        
        try {
            
            String msgExchangeId = null;
            
            InOut inOut =  mBPELSEHelper.getExchangeFactory().createInOutExchange();
            inOut.setEndpoint(endpointReference);
//TODOvb review the result after changes done in EPR resolution impl
            //TODO: Should we set the service name since we are already setting the
            //endpoint name? What about the interface name? 05/21/07
//            inOut.setService(serviceName);
            // TODO this is optional, but today our components can't work if we don't specify this!!
            inOut.setOperation(operation);
            
            WSMessage wsMessage = (WSMessage) msgContainer.getContent();
            Document jbiMessageDoc = wsMessage.getDocument();
            NormalizedMessage nmsg = inOut.createMessage();
            setContentAttachsNMProps(nmsg, jbiMessageDoc, wsMessage);
            
            inOut.setInMessage(nmsg);

            // set the CRMPInvokeId for custom reliable message exchange as a property
            String prop = ServiceQuality.MESSAGE_ID;
            Object propValue = msgContainer.getCRMPInvokeId(); 
            inOut.setProperty(prop, propValue);
            
            // set the ServiceQuality.MESSAGE_ID, the unique id for 
            // Quality of service requirements.
            inOut.setProperty(ServiceQuality.MESSAGE_ID, propValue);

            // Set the transaction property on the message exchange if present.
//            Transaction transaction = msgContainer.getTransaction();
//            if (transaction != null) {
//            	inOut.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, transaction);
//            }
            
            assignChildExchange(msgContainer, inOut);
            
            if (msgContainer.getTransaction() != null && msgContainer.isTxAtomicStarted()) {
            	inOut.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, msgContainer.getTransaction());
            }
            
            msgExchangeId = inOut.getExchangeId();
            mBPELSEHelper.putinResponseMap(msgExchangeId, process);
            
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPJBI-3023: Sending a two way outbound message to the NMR. " + 
                		"MessageExchangeId is {0}, service name is {1}, endpoint name is {2}.", 
                		msgExchangeId, serviceName, endpointName));
            }
            if (LOGGER.isLoggable(Level.FINEST)) {
            	LOGGER.log(Level.FINEST, I18n.loc("BPJBI-3018: The contents of the message are : {0}", wsMessage));
                Map<String, Object> nmProps = wsMessage.getNMProperties();
                if (nmProps != null) {
                    String nmProperties = "NM Properties: \n";
                    for (String key : nmProps.keySet()) {
                        Object val = nmProps.get(key);
                        nmProperties = I18n.loc("Property: {0} value: {1} \n", key, val);
                    }
                    LOGGER.log(Level.FINEST, nmProperties);
                }

            }
            
            try {
                mBPELSEHelper.getDeliveryChannel().send(inOut);
            } catch(Exception e) {
                mBPELSEHelper.removefromResponseMap(msgExchangeId);
                throw e;
            }

            return msgExchangeId;
            
        } catch (Exception e) {
            String msg = I18n.loc("BPJBI-6021: Caught exception when invoking two way external webservice");
            LOGGER.log(Level.SEVERE, msg, e);
            throw new RuntimeException(msg, e);
        }
    }    
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#sendFault(javax.xml.namespace.QName,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void sendFault(QName faultName, MessageContainer msgContainer) {
        WSMessage wsMessage = (WSMessage) msgContainer.getContent();
        InOut inOut = 
                (InOut) (mBPELSEHelper.removefromRequestMap(msgContainer.getId()));
        try {
            DOMSource faultContent = null;

            if (wsMessage != null) {
                Document jbiMessageDoc = wsMessage.getDocument();
                faultContent = new DOMSource(jbiMessageDoc);
            }

            Fault fault = inOut.createFault();
            fault.setContent(faultContent);

                    // We should also project the NMProperties that were set on the fault's message,
            // setting just the content is not enough.
            for (Map.Entry<String, Object> entry : wsMessage.getNMProperties().entrySet()) {
                fault.setProperty(entry.getKey(), entry.getValue());
            }

            inOut.setFault(fault);

            if (LOGGER.isLoggable(Level.FINE)) {
				LOGGER.log(Level.FINE, I18n.loc("BPJBI-3020: Sending Fault for MessageEx id {0} faultName: {1}", 
						inOut.getExchangeId(), faultName));
			}
            if (LOGGER.isLoggable(Level.FINEST)) {
            	LOGGER.log(Level.FINEST, I18n.loc("BPJBI-3018: The contents of the message are : {0}", wsMessage));
            }

            mBPELSEHelper.getDeliveryChannel().send(inOut);

        } catch (MessagingException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6002: Unable to reply to an InOut"), ex);
            throw new RuntimeException(ex);
        }
    }
    
    public void sendRequestError(String msgExchangeId, Exception error) {
		MessageExchange me = (MessageExchange) (mBPELSEHelper.removefromRequestMap(msgExchangeId));
		mBPELSEHelper.sendError(me, error);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#getExternalEndPoint(PartnerLink)
     */
    public DocumentFragment getExternalEndPoint(PartnerLink partnerLink) {
        return mEPRComposer.getExternalEndpointReference(partnerLink);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#getEndPointReference(PartnerLink)
     */
    public DocumentFragment getEndpointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole) {
        return getEPRComposer().getEndpointReference(plScope, partnerLink, isMyRole);
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment docFrag) {
        return getEPRComposer().resolveEPR(docFrag);
    }

    public EPReferenceComposer getEPRComposer() {
        return mEPRComposer;
    }

    /*
     * 
     */
    private void assignChildExchange(MessageContainer msgContainer, MessageExchange msgExchange) 
    throws CorrelatedMessageExchangeException {
    	PropagationContext propContext = msgContainer.getPropagationContext();
        PropagationConfigManager manager = new PropagationConfigManager(propContext); 
        ParentChildExchangeCorrelator pceCorrelator = 
        	ParentChildExchangeCorrelatorFactory.getDefault().newParentChildExchangeCorrelator(manager);
        pceCorrelator.assignChildExchange(propContext.getParentMessageExchange(), msgExchange);
    }

    public void sendKPIMEx(QName interfaceName, String operation, String doc) throws MessagingException {

        ServiceEndpoint[] seps = (mBPELSEHelper.getComponentContext()).getEndpointsForService(interfaceName);//.getEndpoints(interfaceName);
        if (seps.length <= 0) {
            return;
        }
        ServiceEndpoint endpointReference = seps[0];
        InOnly inOnly = (mBPELSEHelper.getExchangeFactory()).createInOnlyExchange();

        NormalizedMessage nmsg = inOnly.createMessage();
        Source srcDoc = new StreamSource(new StringReader(doc));
        nmsg.setContent(srcDoc);

        inOnly.setInMessage(nmsg);
        inOnly.setEndpoint(endpointReference);
        inOnly.setOperation(new QName(operation));

        if (LOGGER.isLoggable(Level.FINE)) {
            LOGGER.log(Level.FINE, 
                    I18n.loc("BPJBI-3019: Sending a one way outbound message to the NMR. MessageExchangeId is " + 
                            "{0}, service name is {1}, endpoint name is {2}.", 
                            inOnly.getExchangeId(), endpointReference.getServiceName(), endpointReference.getEndpointName()));
        }
        if (LOGGER.isLoggable(Level.FINEST)) {
            LOGGER.log(Level.FINEST, I18n.loc("BPJBI-3018: The contents of the message are : {0}", doc));
        }

        mBPELSEHelper.getDeliveryChannel().send(inOnly);
    }
    public void installServiceQualities(String suName, String suPath)
            throws DeploymentException {
        MessagingChannel mc = (MessagingChannel)mBPELSEHelper.getDeliveryChannel(); 
        mc.installServiceQualities(suName, suPath);
    }
    public void uninstallServiceQualities(String suName){
        MessagingChannel mc = (MessagingChannel)mBPELSEHelper.getDeliveryChannel(); 
        mc.uninstallServiceQualities(suName);
    }

    private static void setContentAttachsNMProps(NormalizedMessage nmsg, 
            Document jbiMessageDoc, WSMessage wsMessage) throws MessagingException {
        
        nmsg.setContent(new DOMSource(jbiMessageDoc));
        Map<String, Object> nmProps = wsMessage.getNMProperties();
        if (nmProps != null) {
            for (String key : nmProps.keySet()) {
                Object val = nmProps.get(key);
                nmsg.setProperty(key, val);
            }
        }
        
        Set<String> attchNames = wsMessage.getAttachments();
        for (String atchName : attchNames) {
            nmsg.addAttachment(atchName, wsMessage.getAttachment(atchName));
        }
    }
}
