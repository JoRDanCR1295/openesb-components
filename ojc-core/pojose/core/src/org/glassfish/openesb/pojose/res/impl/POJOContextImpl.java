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
 * @(#)POJOContextImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.res.impl;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import org.glassfish.openesb.pojose.api.ErrorMessage;
import org.glassfish.openesb.pojose.api.MessageException;
import org.glassfish.openesb.pojose.api.annotation.Endpoint;
import org.glassfish.openesb.pojose.api.res.POJOContext;
import org.glassfish.openesb.pojose.core.util.Constants;
import org.glassfish.openesb.pojose.core.util.I18n;
import org.glassfish.openesb.pojose.core.util.Util;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Node;

/**
 *  POJO Context provides easy methods to POJO Service to invoke another service. 
 * 
 *  @author Girish Patil
 *  @author Sreeni Genipudi
 */
public class POJOContextImpl implements POJOContext {
    private MessageExchange provMsgExchange = null;
    private DeliveryChannel dc = null;
    private ComponentContext cc = null;
    private static final String LoggerName = "org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor" ; //NOI18N
    
    // *** POJOContext Interface methods...
    public ServiceEndpoint getEndpoint(QName serviceName, String endpoint){
        if (this.cc != null){
            return cc.getEndpoint(serviceName, endpoint);
        }
        return null;
    };

    public Document getEndpointDescriptor(ServiceEndpoint endpoint) throws JBIException{
        if (this.cc != null){
            return cc.getEndpointDescriptor(endpoint);
        }
        return null;
    }

    public ServiceEndpoint[] getEndpoints(QName interfaceName) {
        if (this.cc != null){
            return cc.getEndpoints(interfaceName);
        }
        return null;
    }

    public ServiceEndpoint[] getEndpointsForService(QName serviceName) {
        if (this.cc != null){
            return cc.getEndpointsForService(serviceName);
        }
        return null;
    }

    public ServiceEndpoint[] getExternalEndpoints(QName interfaceName) {
        if (this.cc != null){
            return cc.getExternalEndpoints(interfaceName);
        }
        return null;
    }

    public ServiceEndpoint[] getExternalEndpointsForService(QName serviceName) {
        if (this.cc != null){
            return cc.getExternalEndpointsForService(serviceName);
        }
        return null;        
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment epr) {
        if (this.cc != null){
            return cc.resolveEndpointReference(epr);
        }
        return null;
    }

    public MessageExchangeFactory createExchangeFactory(){
        return this.dc.createExchangeFactory();
    }
    
    public MessageExchangeFactory createExchangeFactory(javax.xml.namespace.QName interfaceName){
        return this.dc.createExchangeFactory(interfaceName);
    }
    
    public MessageExchangeFactory createExchangeFactory(ServiceEndpoint endpoint){
        return this.dc.createExchangeFactory(endpoint);
    }
    
    public MessageExchangeFactory createExchangeFactoryForService(javax.xml.namespace.QName serviceName){
        return this.dc.createExchangeFactory(serviceName);
    }
    
    public MessageExchange getMessageExchange() {
        return this.provMsgExchange;
    }

    public boolean sendSynch(MessageExchange me) throws ErrorMessage{ 
        boolean processed = true;
        try {
            //processed = ((BaseMessagingChannel) this.dc).getDeliveryChannel().sendSync(me);  
            processed = this.dc.sendSync(me);
        } catch (MessagingException ex) {
            Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
            throw new ErrorMessage(ex);
        }
        return processed;
    }
    
    public void send(MessageExchange me) throws ErrorMessage{
        try {
            this.dc.send(me);
        } catch (MessagingException mex){
            throw new ErrorMessage(mex);
        }
    }
    
    public void sendSynchInOnly(ServiceEndpoint se, Object msg) throws ErrorMessage{ 
        String m = null;
        boolean processed = true;
        InOnly io = null;
        
        if (se == null) {
            m = I18n.loc("POJOSE-7201: ServiceEndpoint object can not be null.");
            throw new ErrorMessage(m);
        }

        try {
            MessageExchangeFactory fct = this.dc.createExchangeFactory(((ServiceEndpointImpl)se).getServiceEndpoint());
            io = fct.createInOnlyExchange();
            
            if (!((ServiceEndpointImpl)se).isQNameEmpty()){
                io.setOperation(((ServiceEndpointImpl)se).getOperationQName());                
            } else {
                m = I18n.loc("POJOSE-5001: Operation QName was not specified on the Endpoint {0}, not setting it on MessageExchange.", se);
                Logger.getLogger(LoggerName).info(m);
            }

            ServiceEndpointImpl seImpl = (ServiceEndpointImpl) se;
            Endpoint epAnno = seImpl.getEndpointAnnotation();
            QName inMsgQN = null;
            if (!Constants.ANNOTATION_NULL_VAL.equals(epAnno.inMessageTypeQN())){
                inMsgQN = QName.valueOf(epAnno.inMessageTypeQN());
            } else {
                inMsgQN = new QName(epAnno.interfaceNS(), epAnno.inMessageType());
            }
            
            if (msg instanceof NormalizedMessage) {
                io.setInMessage((NormalizedMessage) msg);
                processed = this.dc.sendSync(io);
            } else if (msg instanceof Source) {
                Source src = Util.source2jbiMessage((Source) msg,
                        inMsgQN.getNamespaceURI(), inMsgQN.getLocalPart());
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setInMessage(nm);
                processed = this.dc.sendSync(io);
            } else if (msg instanceof Node) {
                Source src = Util.node2WrappedSource((Node) msg,
                        inMsgQN.getNamespaceURI(), inMsgQN.getLocalPart());
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setInMessage(nm);
                processed = this.dc.sendSync(io);
            } else if (msg instanceof String) {
                Source src = Util.string2WrappedSource((String) msg,
                        inMsgQN.getNamespaceURI(), inMsgQN.getLocalPart());
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setInMessage(nm);
                processed = this.dc.sendSync(io);
            } else {
                //Assume msg is null;
                NormalizedMessage nm = io.createMessage();
                io.setInMessage(nm);
                processed = this.dc.sendSync(io);                
            }           
        } catch (Exception ex) {
            m = I18n.loc("POJOSE-7302: Exception while transforming and sending In message.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
            throw new ErrorMessage(m, ex);
        }

        if (processed){
            if (io != null){
                if (ExchangeStatus.ERROR.equals(io.getStatus())){
                    m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                    Logger.getLogger(LoggerName).log(Level.SEVERE, m, io.getError());
                    throw new ErrorMessage(io.getError());
                }
            }
        }
        if (!processed) {
            m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
            throw new ErrorMessage(m);
        }
    }

    public Object sendSynchInOut(ServiceEndpoint se, Object msg, MessageObjectType ootype) throws MessageException {
        String m = null;
        InOut io = null;        
        boolean processed = true;
        Object ret = null;
        
        if (se == null) {
            m = I18n.loc("POJOSE-7201: ServiceEndpoint object can not be null.");
            throw new ErrorMessage(m);
        }
        
        try {
            MessageExchangeFactory fct = this.dc.createExchangeFactory(((ServiceEndpointImpl)se).getServiceEndpoint());
            io = fct.createInOutExchange();

            if (!((ServiceEndpointImpl)se).isQNameEmpty()){
                io.setOperation(((ServiceEndpointImpl)se).getOperationQName());                
            } else {
                m = I18n.loc("POJOSE-5001: Operation QName was not specified on the Endpoint {0}, not setting it on MessageExchange.", se);
                Logger.getLogger(LoggerName).info(m);
            }

            ServiceEndpointImpl seImpl = (ServiceEndpointImpl) se;
            Endpoint epAnno = seImpl.getEndpointAnnotation();
            QName inMsgQN = null;
            if (!Constants.ANNOTATION_NULL_VAL.equals(epAnno.inMessageTypeQN())){
                inMsgQN = QName.valueOf(epAnno.inMessageTypeQN());
            } else {
                inMsgQN = new QName(epAnno.interfaceNS(), epAnno.inMessageType());
            }

            if (msg instanceof NormalizedMessage) {
                io.setInMessage((NormalizedMessage) msg);
                processed = this.dc.sendSync(io);
            } else if (msg instanceof Source) {
                Source src = Util.source2jbiMessage((Source) msg,
                        inMsgQN.getNamespaceURI(), inMsgQN.getLocalPart());
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setInMessage(nm);
                processed = this.dc.sendSync(io);
            } else if (msg instanceof Node) {
                Source src = Util.node2WrappedSource((Node) msg,
                        inMsgQN.getNamespaceURI(), inMsgQN.getLocalPart());
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setInMessage(nm);
                processed = this.dc.sendSync(io);
            } else if (msg instanceof String) {
                Source src = Util.string2WrappedSource((String) msg,
                        inMsgQN.getNamespaceURI(), inMsgQN.getLocalPart());
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setInMessage(nm);
                processed = this.dc.sendSync(io);
            } else {
                //Assume msg is null;
                NormalizedMessage nm = io.createMessage();
                io.setInMessage(nm);
                processed = this.dc.sendSync(io);                
            }
        } catch (Exception ex) {
            m = I18n.loc("POJOSE-7302: Exception while transforming and sending In message.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
            throw new ErrorMessage(m, ex);
        }

        if (!processed){
            m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
            throw new ErrorMessage(m);            
        }        
        
        if (processed){
            if (io != null){
                if (ExchangeStatus.ERROR.equals(io.getStatus())){
                    m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                    Logger.getLogger(LoggerName).log(Level.SEVERE, m, io.getError());
                    throw new ErrorMessage(io.getError());
                }
            }
        }
        
        // Now transform out message                    
        try {
            if (MessageObjectType.NormalizedMessage.equals(ootype)){
                ret = io.getOutMessage();
            } else if (MessageObjectType.Source.equals(ootype)){
                NormalizedMessage nm  = io.getOutMessage();
                if (nm != null){
                    Source src = nm.getContent();
                    if (src != null){
                        src = Util.jbiMessage2Source(src);
                        ret = src;
                    }
                }
            } else if (MessageObjectType.Node.equals(ootype)){
                NormalizedMessage nm  = io.getOutMessage();
                if (nm != null){
                    Source src = nm.getContent();
                    if (src != null){
                        Node node = Util.jbiMessage2Node(src);
                        ret = node;
                    }
                }
            } else if (MessageObjectType.String.equals(ootype)){
                NormalizedMessage nm  = io.getOutMessage();
                if (nm != null){
                    Source src = nm.getContent();
                    if (src != null){
                        String str = Util.jbiMessage2String(src);
                        ret = str;
                    }
                }                
            } 
            
            // Send done.
            io.setStatus(ExchangeStatus.DONE);
            this.dc.send(io);        
            
        } catch (Exception ex){
            m = I18n.loc("POJOSE-7303: Exception while transforming Out message.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
            throw new ErrorMessage(m, ex);
        }
        
        return ret;
    }
    // *** End of POJOContext Interface methods...
    
    public void setMessageExchange(MessageExchange me) {
        this.provMsgExchange = me;
    }
    
    public POJOContextImpl(ComponentContext ncc, DeliveryChannel ndc) {
        this.cc = ncc;
        this.dc = ndc;
    }  
}
