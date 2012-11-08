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
 * @(#)AbstractCRLMessageExchange.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange.impl;

import java.net.URI;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.Callback;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.exchange.ExchangeUtil;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Abstract base class for message exchange view implementations.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractCRLMessageExchange implements CRLMessageExchange {
	private Logger mLogger; 
    private MessageExchange mMessage;
    private ManagerContext mContext;
    private Callback mCallback;
    private EndpointInfo mInfo;
    
    protected AbstractCRLMessageExchange(ManagerContext ctx, 
            							 MessageExchange msg) {
    	this(ctx, msg, null);
    }
    
    protected AbstractCRLMessageExchange(ManagerContext ctx, 
                                         MessageExchange msg,
                                         EndpointInfo info) {
        setContext(ctx);
        setMessageExchange(msg);
        setInvokeInfo(info);
        mLogger = LogUtil.getLogger(ctx, CRLMessageExchange.class.getName());
    }

    /** @see com.sun.jbi.crl.mep.exchange.CRLMessageExchange#send() */
    public void send() throws JBIException {
        // TODO do we validate status/messages/anything?
        try {
            if (log().isLoggable(Level.FINE)) {
            	ServiceEndpoint endpt = getEndpoint();
            	log().log(Level.FINE, "CRL-3022: Sending message exchange (id={0}) to endpoint({1}-{2})",
            			  new Object[] { getExchangeId(), endpt.getServiceName(), endpt.getEndpointName() });
            	if (log().isLoggable(Level.FINER)) {
            		log().finer("CRL-2012: Destination endpoint definition:\n"+ 
            					String.valueOf(endpt));
            	}
            }

            getContext().getDeliveryChannel().send(getMessageExchange());
        }
        catch (MessagingException mex) {
        	log().log(Level.WARNING,
        			  I18n.loc("CRL-6028: Failed to send message exchange (id={0}): {1}", 
        					   getMessageExchange().getExchangeId(), mex.getMessage()), 
        			  mex);
            throw mex;
        }
    }
    
    /** @see com.sun.jbi.crl.mep.exchange.CRLMessageExchange#sendError(java.lang.Exception) */
    public void sendError(Exception error) throws JBIException {
        setStatus(ExchangeStatus.ERROR);
        
        // systemic: only send java.lang.Exception
        if (error.getClass().equals("java.lang.Exception")) {
        	setError(error);
        }
        else {
        	setError(new Exception(error.getMessage(), error));
        }
        
        // ensure error properties are populated
        if (!this.getContext().getComponentName().equals(
        		String.valueOf(getProperty(CRLMessageExchange.FAULTACTOR_PROPERTY_NAME)))) {
        	// properties not set, let's do so now
        	ExchangeUtil.setErrorData(this.getMessageExchange(), 
        							  error.getMessage(), 
        							  FaultCode.Server, 
        							  null,	// default message is provided by utility 
        							  this.getContext().getComponentName());
        }
        send();
    }

    /** @see com.sun.jbi.crl.mep.exchange.CRLMessageExchange#sendError(java.lang.String, com.sun.jbi.crl.mep.exchange.CRLMessageExchange.FaultCode, java.lang.String, java.lang.Exception) */
	public void sendError(String error, FaultCode code, String detail, Exception cause) throws JBIException {
		ExchangeUtil.setErrorData(this, error, code, detail, getContext().getComponentName());
		log().warning(I18n.loc(
				"CRL-6056: Sending ERROR on ME({0}, service={1}-{2}) - Cause: {3},{4}",
				getExchangeId(), getEndpoint().getServiceName(), 
				getEndpoint().getEndpointName(), error, detail));
		if (cause == null) {
			sendError(new Exception(error));
		}
		else {
			sendError(new Exception(error, cause));
		}
	}

	/** @see com.sun.jbi.crl.mep.exchange.CRLMessageExchange#getCallback() */
    public Callback getCallback() {
        return mCallback;
    }

    /** @see com.sun.jbi.crl.mep.exchange.CRLMessageExchange#setCallback(com.sun.jbi.crl.mep.Callback) */
    public void setCallback(Callback cback) {
        mCallback = cback;
    }

    
    /**
     * Utility method to send a message which consumes a service.
     * @param src The message data.
     * @param operation The operation to invoke.
     * @throws JBIException If an error occurs sending message exchange.
     */
    protected void invoke(Source src, QName operation) throws JBIException {
        // Create new message and populate its content
        NormalizedMessage yIn = createMessage();
        yIn.setContent(src);
        // Set message as "in" reference in exchange 
        setMessage(yIn, "in");  // "in" literal described in JBI spec

        ServiceEndpoint srvcEndpoint = 
                getContext().getEndpoint(mInfo.getServiceName(), 
                                         mInfo.getEndpointName());
        setEndpoint(srvcEndpoint);
        setOperation(operation);
        send();
    }

    protected ManagerContext getContext() {
        return mContext;
    }
    
	protected EndpointInfo getInvokeInfo() {
		return mInfo;
	}
	
    protected MessageExchange getMessageExchange() {
        return mMessage;
    }
    
    protected void setContext(ManagerContext ctx) {
        mContext = ctx;
    }
    
    protected void setInvokeInfo(EndpointInfo info) {
    	mInfo = info;
    }
    
    protected void setMessageExchange(MessageExchange msg) {
    	// unwrap CRL MEs
    	if (msg instanceof AbstractCRLMessageExchange) {
    		setMessageExchange(((AbstractCRLMessageExchange) msg).getMessageExchange());
    	}
    	else {
    		mMessage = msg;
    	}
    }

    /**
     * Fetches a {@link Logger} for this message exchange.
     * @return a <code>Logger</code>.
     */
    protected Logger log() {
    	return mLogger;
    }

    /* ME impl - forwarding to underlying ME */
    
    /** @see javax.jbi.messaging.MessageExchange#createFault() */
    public Fault createFault() throws MessagingException {
        return mMessage.createFault();
    }

    /** @see javax.jbi.messaging.MessageExchange#createMessage() */
    public NormalizedMessage createMessage() throws MessagingException {
        return mMessage.createMessage();
    }

    /** @see javax.jbi.messaging.MessageExchange#getEndpoint() */
    public ServiceEndpoint getEndpoint() {
        return mMessage.getEndpoint();
    }

    /** @see javax.jbi.messaging.MessageExchange#getError() */
    public Exception getError() {
        return mMessage.getError();
    }

    /** @see javax.jbi.messaging.MessageExchange#getExchangeId() */
    public String getExchangeId() {
        return mMessage.getExchangeId();
    }

    /** @see javax.jbi.messaging.MessageExchange#getFault() */
    public Fault getFault() {
        return mMessage.getFault();
    }

    /** @see javax.jbi.messaging.MessageExchange#getInterfaceName() */
    public QName getInterfaceName() {
        return mMessage.getInterfaceName();
    }

    /** @see javax.jbi.messaging.MessageExchange#getMessage(java.lang.String) */
    public NormalizedMessage getMessage(String name) {
        return mMessage.getMessage(name);
    }

    /** @see javax.jbi.messaging.MessageExchange#getOperation() */
    public QName getOperation() {
        return mMessage.getOperation();
    }

    /** @see javax.jbi.messaging.MessageExchange#getPattern() */
    public URI getPattern() {
        return mMessage.getPattern();
    }

    /** @see javax.jbi.messaging.MessageExchange#getProperty(java.lang.String) */
    public Object getProperty(String name) {
        return mMessage.getProperty(name);
    }

    /** @see javax.jbi.messaging.MessageExchange#getPropertyNames() */
    public Set getPropertyNames() {
        return mMessage.getPropertyNames();
    }

    /** @see javax.jbi.messaging.MessageExchange#getRole() */
    public Role getRole() {
        return mMessage.getRole();
    }

    /** @see javax.jbi.messaging.MessageExchange#getService() */
    public QName getService() {
        return mMessage.getService();
    }

    /** @see javax.jbi.messaging.MessageExchange#getStatus() */
    public ExchangeStatus getStatus() {
        return mMessage.getStatus();
    }

    /** @see javax.jbi.messaging.MessageExchange#isTransacted() */
    public boolean isTransacted() {
        return mMessage.isTransacted();
    }

    /** @see javax.jbi.messaging.MessageExchange#setEndpoint(javax.jbi.servicedesc.ServiceEndpoint) */
    public void setEndpoint(ServiceEndpoint endpoint) {
        mMessage.setEndpoint(endpoint);
    }

    /** @see javax.jbi.messaging.MessageExchange#setError(java.lang.Exception) */
    public void setError(Exception error) {
        mMessage.setError(error);
    }

    /** @see javax.jbi.messaging.MessageExchange#setFault(javax.jbi.messaging.Fault) */
    public void setFault(Fault fault) throws MessagingException {
        mMessage.setFault(fault);
    }

    /** @see javax.jbi.messaging.MessageExchange#setInterfaceName(javax.xml.namespace.QName) */
    public void setInterfaceName(QName interfaceName) {
        mMessage.setInterfaceName(interfaceName);
    }

    /** @see javax.jbi.messaging.MessageExchange#setMessage(javax.jbi.messaging.NormalizedMessage, java.lang.String) */
    public void setMessage(NormalizedMessage msg, String name) throws MessagingException {
        mMessage.setMessage(msg, name);
    }

    /** @see javax.jbi.messaging.MessageExchange#setOperation(javax.xml.namespace.QName) */
    public void setOperation(QName name) {
        mMessage.setOperation(name);
    }

    /** @see javax.jbi.messaging.MessageExchange#setProperty(java.lang.String, java.lang.Object) */
    public void setProperty(String name, Object obj) {
        mMessage.setProperty(name, obj);
    }

    /** @see javax.jbi.messaging.MessageExchange#setService(javax.xml.namespace.QName) */
    public void setService(QName service) {
        mMessage.setService(service);
    }

    /** @see javax.jbi.messaging.MessageExchange#setStatus(javax.jbi.messaging.ExchangeStatus) */
    public void setStatus(ExchangeStatus status) throws MessagingException {
        mMessage.setStatus(status);
    }
}
