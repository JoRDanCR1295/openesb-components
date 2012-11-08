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
 * @(#)AbstractThreadManager.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.mep.ThreadManager;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractThreadManager implements ThreadManager {
    private Logger mLogger;

    public AbstractThreadManager(ComponentContext ctx) {
    	if (mLogger == null) {
            mLogger = LogUtil.getLogger(ctx, ThreadManager.class.getName());
        }
    }
    
	protected boolean validateMessage(MessageExchange msg, ComponentContext ctx) {
        if (msg == null) {
            log().warning(I18n.loc(
            		"CRL-6044: Received NULL message exchange. Canceling invoke for component \"{0}\"!", 
            		(ctx == null) ? "Unknown Component" : ctx.getComponentName()));
            return false;
        }

        return true;
	}
	
	protected void handleInvokeFailure(Exception e) throws JBIException {
    	String err = I18n.loc("CRL-6045: Invoking {0} failed: {1}", 
	  			  			  this.getClass().getSimpleName(), e.getMessage());
    	log().log(Level.WARNING, err, e);
    	throw ((e instanceof JBIException) ? ((JBIException) e) 
    									   : new JBIException(err, e));

	}
	protected Logger log() {
		return mLogger;
	}
}
