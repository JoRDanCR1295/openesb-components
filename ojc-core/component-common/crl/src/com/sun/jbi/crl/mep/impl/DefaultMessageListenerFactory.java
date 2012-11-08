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
 * @(#)DefaultMessageListenerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.crl.mep.MessageListener;
import com.sun.jbi.crl.mep.MessageListenerFactory;
import com.sun.jbi.crl.util.I18n;

/**
 * Default implementation of {@link MessageListenerFactory}.
 * 
 * @author Kevan Simpson
 */
public class DefaultMessageListenerFactory implements MessageListenerFactory {
    private boolean mThreadSafe = true;
    private MessageListener mListener = null;
    private NewInstanceFactory mFactory = null;
    
    public DefaultMessageListenerFactory(ComponentContext ctx) {
        this(new DefaultMessageListener(ctx));
    }
    
    public DefaultMessageListenerFactory(MessageListener listener) {
        setListener(listener);
        setThreadSafe(true);
    }
    
    public DefaultMessageListenerFactory(Class type) throws JBIException {
        initFactory(type);
        setThreadSafe(false);
    }

    /** @see com.sun.jbi.crl.mep.MessageListenerFactory#getInstance() */
    public MessageListener getInstance() throws JBIException {
        MessageListener ml = null;
        if (mThreadSafe) {
            ml = getListener();
        } 
        else {
            Class type = getFactory().getType();
            try {
                ml = (MessageListener) getFactory().newInstance(type);
            }
            catch (Exception e) {
                throw new JBIException(
                		I18n.loc("CRL-6040: Failed to create listener of type \"{0}\": {1}", 
                				 String.valueOf(type), e.getMessage()), 
                        e);
            }
        }

        return ml;
    }
    
    public boolean isThreadSafe() {
        return mThreadSafe;
    }
    
    protected void initFactory(Class type) {
        if (type != null) {
            mFactory = new NewInstanceFactory(type);
        }
    }
    protected NewInstanceFactory getFactory() {
        return mFactory;
    }
    protected MessageListener getListener() {
        return mListener;
    }
    protected void setListener(MessageListener ml) {
        mListener = ml;
    }
    protected void setThreadSafe(boolean tsafe) {
        mThreadSafe = tsafe;
    }
}
