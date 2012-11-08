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
 * @(#)AbstractExchangeRouter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import javax.jbi.JBIException;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.mep.Callback;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.proc.Processor;
import com.sun.jbi.crl.mep.proc.ProcessorFactory;
import com.sun.jbi.crl.util.EntryRegistry;
import com.sun.jbi.crl.util.I18n;

/**
 * Abstract base class for {@link ExchangeRouter} implementations.
 * <p>
 * <b>NOTE:</b> Extensions should only need to implement {@link #toKey(MessageExchange)}.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractExchangeRouter implements ExchangeRouter {
    private EntryRegistry<RouterKey, ProcessorFactory> mRegistry = 
            new EntryRegistry<RouterKey, ProcessorFactory>();

    /** @see com.sun.jbi.crl.mep.ExchangeRouter#getProcessor(com.sun.jbi.crl.mep.ExchangeRouter.RouterKey) */
    public Processor getProcessor(RouterKey key) throws JBIException {
        if (key == null) return null;
        return lookupFactory(key).getProcessor();
    }

    /** @see com.sun.jbi.crl.mep.ExchangeRouter#getCallback(com.sun.jbi.crl.mep.ExchangeRouter.RouterKey) */
    public Callback getCallback(RouterKey key) throws JBIException {
        if (key == null) return null;
        return lookupFactory(key).getCallback();
    }

    /** @see com.sun.jbi.crl.mep.ExchangeRouter#register(com.sun.jbi.crl.mep.ExchangeRouter.RouterKey, com.sun.jbi.crl.mep.proc.ProcessorFactory) */
    public void register(RouterKey key, ProcessorFactory entry) {
        mRegistry.register(key, entry);
    }

    /** @see com.sun.jbi.crl.mep.ExchangeRouter#remove(com.sun.jbi.crl.mep.ExchangeRouter.RouterKey) */
    public ProcessorFactory remove(RouterKey key) {
        return mRegistry.remove(key);
    }

    public abstract RouterKey toKey(MessageExchange me) throws JBIException;
    
    protected ProcessorFactory lookupFactory(RouterKey key) throws JBIException{
        ProcessorFactory factory = getRegistry().lookup(key);
        if (factory == null) {
            String msg = I18n.loc("CRL-6031: No factory registered for: {0}", 
            					String.valueOf(key));
//            LogUtil.log(this.getClass(), Level.WARNING, msg, null);
            throw new JBIException(msg);
        }

        return factory;
    }
    
    protected EntryRegistry<RouterKey, ProcessorFactory> getRegistry() {
        return mRegistry;
    }
    protected void setRegistry(EntryRegistry<RouterKey, ProcessorFactory> registry) {
        mRegistry = registry;
    }
}
