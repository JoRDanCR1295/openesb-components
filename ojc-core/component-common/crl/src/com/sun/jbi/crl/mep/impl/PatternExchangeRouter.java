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
 * @(#)PatternExchangeRouter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.proc.Processor;
import com.sun.jbi.crl.mep.proc.impl.DefaultInOnlyConsumer;
import com.sun.jbi.crl.mep.proc.impl.DefaultInOutConsumer;
import com.sun.jbi.crl.mep.proc.impl.DefaultInOutProvider;
import com.sun.jbi.crl.util.I18n;

/**
 * Standard implemenation of the {@link ExchangeRouter} interface.
 * 
 * @author Kevan Simpson
 */
public class PatternExchangeRouter extends AbstractExchangeRouter {
    private static Logger mLogger = Logger.getLogger(PatternExchangeRouter.class.getName());
    
    /**
     * Creates an {@link ExchangeRouter} using pattern-role {@link RouterKey}s.
     */
    public PatternExchangeRouter() {
        this(true);
    }
    
    /**
     * Creates an {@link ExchangeRouter} using pattern-role {@link RouterKey}s.
     * 
     * @param providePatternRoleDefaults A flag indicating whether or not to
     *                                   populate router with logging-only
     *                                   default {@link Processor} implementations 
     *                                   of all standard JBI message exchange
     *                                   patterns.
     */
    public PatternExchangeRouter(boolean providePatternRoleDefaults) {
        if (providePatternRoleDefaults) {
            if (mLogger.isLoggable(Level.FINER)) {
            	mLogger.finer("CRL-2011: Inserting default processor implementations...");
            }
        
            register(PatternRoleKey.IN_ONLY_CONSUMER, 
                     new SimpleProcessorFactory(new DefaultInOnlyConsumer()));
            register(PatternRoleKey.IN_ONLY_PROVIDER, 
                     new SimpleProcessorFactory(new DefaultInOnlyConsumer()));
            register(PatternRoleKey.IN_OUT_CONSUMER, 
                     new SimpleProcessorFactory(new DefaultInOutConsumer()));
            register(PatternRoleKey.IN_OUT_PROVIDER, 
                     new SimpleProcessorFactory(new DefaultInOutProvider()));
//            register(PatternRoleKey.IN_OPT_OUT_CONSUMER, 
//                     new SimpleProcessorFactory(new DefaultInOptOutConsumer()));
//            register(PatternRoleKey.IN_OPT_OUT_PROVIDER, 
//                     new SimpleProcessorFactory(new DefaultInOptOutProvider()));
//            register(PatternRoleKey.ROBUST_IN_ONLY_CONSUMER, 
//                     new SimpleProcessorFactory(new DefaultRobustInOnlyConsumer()));
//            register(PatternRoleKey.ROBUST_IN_ONLY_PROVIDER, 
//                     new SimpleProcessorFactory(new DefaultRobustInOnlyProvider()));
        }
    }

    /** @see com.sun.jbi.crl.mep.impl.AbstractExchangeRouter#toKey(javax.jbi.messaging.MessageExchange) */
    @Override
    public RouterKey toKey(MessageExchange me) throws JBIException {
    	RouterKey key = PatternRoleKey.toKey(me);
    	if (key == null) {
            mLogger.warning(I18n.loc("CRL-6043: Failed to determine key for message: {0} - {1}", 
            				String.valueOf(me.getPattern()),
            				String.valueOf(me.getRole())));
    	}
    	return key;
    }
}
