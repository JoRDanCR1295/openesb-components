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
 * @(#)ExchangeRouter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep;

import javax.jbi.JBIException;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.mep.impl.PatternRoleKey;
import com.sun.jbi.crl.mep.proc.Processor;
import com.sun.jbi.crl.mep.proc.ProcessorFactory;

/**
 * Component responsible for routing accepted message exchanges
 * to the appropriate {@link Processor}.
 * 
 * @author Kevan Simpson
 */
public interface ExchangeRouter {
    /** 
     * Tagging interface for routing keys, the implementations 
     * of which are likely to be enums declared in custom 
     * <code>ExchangeRouter</code> implementations.
     * @see PatternRoleKey  
     */
    interface RouterKey {}
    
    /**
     * Fetches {@link Processor} registered to the specified exchange.
     * @param key The router key, typically acquired by invoking {@link #toKey(MessageExchange)}.
     * @return a <code>Processor</code>.
     * @throws JBIException if an error occurs acquiring factory or creating processor.
     */
    public Processor getProcessor(RouterKey key) throws JBIException;
    
    /**
     * Fetches {@link Callback} registered to the specified exchange.
     * @param key The router key, typically acquired by invoking {@link #toKey(MessageExchange)}.
     * @return a <code>Callback</code>.
     * @throws JBIException if an error occurs acquiring factory or creating callback.
     */
    public Callback getCallback(RouterKey key) throws JBIException;
    
    /**
     * Registers a {@link ProcessorFactory} using the specified exchange as a key.
     * @param key The exchange to use as a key.
     * @param entry The template to register.
     */
    public void register(RouterKey key, ProcessorFactory entry);
    
    /**
     * Removes a registered {@link ProcessorFactory}.
     * @param msg The exchange to use as a key.
     * @return The registered template or <code>null</code>.
     */
    public ProcessorFactory remove(RouterKey key);
    
    /**
     * Maps a {@link MessageExchange} to the correct routing key.
     * @param me A message exchange.
     * @return The corresponding routing key.
     * @throws JBIException if there is no key corresponding to the specified message.
     */
    public RouterKey toKey(MessageExchange me) throws JBIException;
}
