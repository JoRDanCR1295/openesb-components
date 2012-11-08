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
 * @(#)PropagationContext.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import javax.jbi.messaging.MessageExchange;
import javax.transaction.Transaction;

import com.sun.jbi.systemic.quality.propagation.api.ConfigManager;

/**
 *
 *
 * @author Sun Microsystems
 */
public class PropagationContext {

	private MessageExchange mParentExchange;
	
	private ConfigManager.TRANSACTIONTYPE mTransType;
	
	private ConfigManager.SECURITYTYPE mSecType;
	
	private Transaction mTransaction;
	
	/**
	 * 
	 * @param frame
	 * @param context
	 */
	public PropagationContext(MessageExchange parentExchange) {
		mParentExchange = parentExchange;
	}
	
	/**
	 * 
	 * @return
	 */
	public MessageExchange getParentMessageExchange() {
		return mParentExchange;
	}
	
	/**
	 * 
	 * @return
	 */
	public ConfigManager.TRANSACTIONTYPE getTransactionType() {
		return mTransType;
	}
	
	/**
	 * 
	 * @return
	 */
	public ConfigManager.SECURITYTYPE getSecurityType() {
		return mSecType;
	}
	
	/**
	 * 
	 * @return
	 */
	public Transaction getTransaction() {
		return mTransaction;
	}
	
	/**
	 * 
	 * @param transType
	 */
	public void setTransactionType(ConfigManager.TRANSACTIONTYPE transType) {
		mTransType = transType;
	}
	
	/**
	 * 
	 * @param secType
	 */
	public void setSecurityType(ConfigManager.SECURITYTYPE secType) {
		mSecType = secType;
	}
	
	/**
	 * 
	 * @param transaction
	 */
	public void setTransaction(Transaction transaction) {
		mTransaction = transaction;
	}
}
