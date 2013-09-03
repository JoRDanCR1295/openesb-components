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
 * @(#)ContextImpl.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.res.impl;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.naming.InitialContext;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.FaultMessage;
import org.glassfish.openesb.pojose.api.res.Context;
import org.glassfish.openesb.pojose.core.util.I18n;
import org.glassfish.openesb.pojose.core.util.Util;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Node;

/**
 *
 * @author gpatil
 */
public class ContextImpl implements Context{
    private MessageExchange provMsgExchange = null;
    private MessagingChannel dc = null;
    private ComponentContext cc = null;
    private CallTracker ct = null;
    private Map applicationVariables;
            
    private static final String LoggerName = "org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor" ; //NOI18N
    
    public ContextImpl(ComponentContext ncc, MessagingChannel ndc, CallTracker callt, Map applicationVariables) {
        this.cc = ncc;
        this.dc = ndc;
        this.ct = callt;
        this.applicationVariables = applicationVariables;
    }

    public void setMessageExchange(MessageExchange me) {
        this.provMsgExchange = me;
    }

    // *** Start of POJOContext Interface methods...
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


    public InitialContext getNamingContext(){
        if (this.cc != null){
            return this.cc.getNamingContext();
        } else {
            return null;
        }
    }

    public Consumer getConsumer(ServiceEndpoint se, QName oper, QName inpt){
        ConsumerImpl c = new ConsumerImpl(this.dc, this.cc, this.provMsgExchange,
                se, oper, inpt);
        c.setCallTracker(this.ct);
        return c;
    }

    public Consumer getConsumer(){
        ConsumerImpl c = new ConsumerImpl(this.dc, this.cc, this.provMsgExchange);
        c.setCallTracker(this.ct);
        return c;
    }

    public FaultMessage createFaultMessage(String payload, QName faultMsgType) {
        FaultMessage fm = null;
        try {
            fm = new FaultMessage(this.provMsgExchange.createFault());
            if (faultMsgType != null){
                Source s = Util.string2WrappedSource(payload, faultMsgType.getNamespaceURI(),
                        faultMsgType.getLocalPart());
                fm.getFault().setContent(s);
            } else {
                Source s = Util.string2Source(payload);
                fm.getFault().setContent(s);
            }
        } catch (MessagingException ex) {
            String m = I18n.loc("POJOSE-7309: Severe internal error while creating Fault instance.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
        } catch (Exception ex){
            String m = I18n.loc("POJOSE-7310: Error while converting payload(String or Node or Source) to JBI elements wrapped Source.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
        }
        return fm;
    } //

    public FaultMessage createFaultMessage(Node payload, QName faultMsgType) {
        FaultMessage fm = null;
        try {
            fm = new FaultMessage(this.provMsgExchange.createFault());
            if (faultMsgType != null){
                Source s = Util.node2WrappedSource(payload, faultMsgType.getNamespaceURI(),
                        faultMsgType.getLocalPart());
                fm.getFault().setContent(s);
            } else {
                Source s = Util.node2WrappedSource(payload);
                fm.getFault().setContent(s);
            }
        } catch (MessagingException ex) {
            String m = I18n.loc("POJOSE-7309: Severe internal error while creating Fault instance.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
        } catch (Exception ex){
            String m = I18n.loc("POJOSE-7310: Error while converting payload(String or Node or Source) to JBI elements wrapped Source.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
        }
        return fm;
    }

    public FaultMessage createFaultMessage(Source payload, QName faultMsgType) {
        FaultMessage fm = null;
        try {
            fm = new FaultMessage(this.provMsgExchange.createFault());
            if (faultMsgType != null){
                Source s = Util.source2jbiMessage(payload, faultMsgType.getNamespaceURI(),
                        faultMsgType.getLocalPart());
                fm.getFault().setContent(s);
            } else {
                Source s = Util.source2jbiMessage(payload);
                fm.getFault().setContent(s);
            }
        } catch (MessagingException ex) {
            String m = I18n.loc("POJOSE-7309: Severe internal error while creating Fault instance.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
        } catch (Exception ex){
            String m = I18n.loc("POJOSE-7310: Error while converting payload(String or Node or Source) to JBI elements wrapped Source.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
        }
        return fm;
    }

    public String getApplicationVariable(String applicationVariableName) {
        String [] metadatas = (String []) this.applicationVariables.get(applicationVariableName);
        
        if (metadatas != null) {
            // IDX 0 = value & IDX 1 = type
            return metadatas[0];
        }
        
        return null;
    }
    
    // *** End of Context Interface methods...
}
