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
 * @(#)DefaultListenerContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.lifecycle.ComponentManager;
import com.sun.jbi.crl.mep.ListenerContext;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.MessageListenerFactory;
import com.sun.jbi.crl.mep.ThreadManager;

/**
 * Default implementation of {@link ListenerContext}.
 * 
 * @author Kevan Simpson
 */
public class DefaultManagerContext extends AbstractCustomContext 
								   implements ManagerContext {
    // we don't need a pool, as our default impl is stateless and thread-safe
    private ComponentManager mComponentMgr;
    private MessagingChannel mChannel;
    
    public DefaultManagerContext(ComponentManager cmgr, MessagingChannel channel) {
    	super(cmgr.getComponentContext());
        setComponentManager(cmgr);
        setMessagingChannel(channel);
    }
    
	/** @see com.sun.jbi.crl.mep.ManagerContext#acquireListenerContext() */
	public ListenerContext acquireListenerContext() {
		return getComponentManager().getAcceptManager().getListenerContext();
	}

	/** @see com.sun.jbi.crl.mep.ManagerContext#getListenerFactory() */
	public MessageListenerFactory getListenerFactory() {
		return getComponentManager().getAcceptManager().getListenerFactory();
	}


	/** @see com.sun.jbi.crl.mep.ManagerContext#getMessagingChannel() */
	public MessagingChannel getMessagingChannel() {
		return mChannel;
	}

	/** @see com.sun.jbi.crl.mep.ManagerContext#getThreadManager() */
	public ThreadManager getThreadManager() {
		return getComponentManager().getAcceptManager().getThreadManager();
	}

	protected ComponentManager getComponentManager() {
        return mComponentMgr;
    }
    protected void setComponentManager(ComponentManager cmgr) {
        mComponentMgr = cmgr;
    }
    protected void setMessagingChannel(MessagingChannel ch) {
    	mChannel = ch;
    }
}
