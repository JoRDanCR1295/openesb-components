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
 * @(#)PropagationConfigManager.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.systemic.quality.propagation.api.ConfigManager;

/**
 *
 *
 * @author Sun Microsystems
 */
public class PropagationConfigManager implements ConfigManager {
	
	private PropagationContext mPropContext;

	/**
	 * 
	 */
	public PropagationConfigManager(PropagationContext propContext) {
		mPropContext = propContext;
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.systemic.quality.propagation.api.ConfigManager#createNewTransaction(javax.jbi.messaging.MessageExchange, javax.jbi.messaging.MessageExchange)
	 */
	public Object createNewTransaction(MessageExchange childExchange) {
		return mPropContext.getTransaction();
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.systemic.quality.propagation.api.ConfigManager#getSecurityType(javax.jbi.messaging.MessageExchange, javax.jbi.messaging.MessageExchange)
	 */
	public SECURITYTYPE getSecurityType(MessageExchange childExchange) {
		return mPropContext.getSecurityType();
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.systemic.quality.propagation.api.ConfigManager#getTransaction(javax.jbi.messaging.MessageExchange, javax.jbi.messaging.MessageExchange)
	 */
	public Object getTransaction(MessageExchange childExchange) {
		return mPropContext.getTransaction();
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.systemic.quality.propagation.api.ConfigManager#getTransactionType(javax.jbi.messaging.MessageExchange, javax.jbi.messaging.MessageExchange)
	 */
	public TRANSACTIONTYPE getTransactionType(MessageExchange childExchange) {
		return mPropContext.getTransactionType();
	}
}
