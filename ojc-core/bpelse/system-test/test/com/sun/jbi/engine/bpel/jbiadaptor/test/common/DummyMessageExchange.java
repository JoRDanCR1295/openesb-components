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
 * @(#)DummyMessageExchange.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.Transaction;
import javax.xml.namespace.QName;

/**
 *
 *
 * @author Sun Microsystems
 */
public class DummyMessageExchange implements MessageExchange {
	
	private String mExchangeId;
	
	private Transaction mTransaction;
	
	private Map<String, NormalizedMessage> mMessageMap;
	
	private boolean mTransacted = false;

	/**
	 * 
	 */
	public DummyMessageExchange(String exchangeId) {
		mExchangeId = exchangeId;
		mMessageMap = new HashMap<String, NormalizedMessage>();
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#createFault()
	 */
	public Fault createFault() throws MessagingException {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#createMessage()
	 */
	public NormalizedMessage createMessage() throws MessagingException {
		return new DummyNormalizedMessage();
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getEndpoint()
	 */
	public ServiceEndpoint getEndpoint() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getError()
	 */
	public Exception getError() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getExchangeId()
	 */
	public String getExchangeId() {
		return mExchangeId;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getFault()
	 */
	public Fault getFault() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getInterfaceName()
	 */
	public QName getInterfaceName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getMessage(java.lang.String)
	 */
	public NormalizedMessage getMessage(String arg0) {
		return mMessageMap.get(arg0);
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getOperation()
	 */
	public QName getOperation() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getPattern()
	 */
	public URI getPattern() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getProperty(java.lang.String)
	 */
	public Object getProperty(String arg0) {
		if (arg0.equals(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME)) {
			return mTransaction;
		} else {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getPropertyNames()
	 */
	public Set getPropertyNames() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getRole()
	 */
	public Role getRole() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getService()
	 */
	public QName getService() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#getStatus()
	 */
	public ExchangeStatus getStatus() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#isTransacted()
	 */
	public boolean isTransacted() {
		return mTransacted;
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setEndpoint(javax.jbi.servicedesc.ServiceEndpoint)
	 */
	public void setEndpoint(ServiceEndpoint arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setError(java.lang.Exception)
	 */
	public void setError(Exception arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setFault(javax.jbi.messaging.Fault)
	 */
	public void setFault(Fault arg0) throws MessagingException {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setInterfaceName(javax.xml.namespace.QName)
	 */
	public void setInterfaceName(QName arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setMessage(javax.jbi.messaging.NormalizedMessage, java.lang.String)
	 */
	public void setMessage(NormalizedMessage arg0, String arg1)
			throws MessagingException {
		mMessageMap.put(arg1, arg0);
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setOperation(javax.xml.namespace.QName)
	 */
	public void setOperation(QName arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setProperty(java.lang.String, java.lang.Object)
	 */
	public void setProperty(String arg0, Object arg1) {
		if (arg0.equals(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME)) {
			mTransaction = (Transaction) arg1;
			mTransacted = true;
		}
	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setService(javax.xml.namespace.QName)
	 */
	public void setService(QName arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see javax.jbi.messaging.MessageExchange#setStatus(javax.jbi.messaging.ExchangeStatus)
	 */
	public void setStatus(ExchangeStatus arg0) throws MessagingException {
		// TODO Auto-generated method stub

	}
}
