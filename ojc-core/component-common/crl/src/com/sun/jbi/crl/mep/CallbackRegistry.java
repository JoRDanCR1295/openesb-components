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
 * @(#)CallbackRegistry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep;

import javax.jbi.JBIException;

import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;

/**
 * Defines registry for {@link Callback} instances.
 * <p>
 * Registration of a <code>Callback</code> involves two critical steps,
 * both of which must be provided by implementations of <code>CallbackRegistry</code>:
 * <ol>
 *      <li>The <code>Callback</code> instance must be stored with a timestamp
 *          in order to calculate the time at which the <code>Callback</code>
 *          expires.</li>
 *      <li>The <code>Callback</code> instance must be provided with the
 *          component's {@link CRLMessageExchangeFactory} so the <code>Callback</code>
 *          is able to create {@link CRLMessageExchange} responses when invoked.</li>
 * </ol>
 *          
 * @author Kevan Simpson
 */
public interface CallbackRegistry {
    /**
     * Invokes a {@link Callback} registered with the specified key,
     * if one exists. 
     * 
     * @param key The key used to register a <code>Callback</code>.
     * @param data Component-specific data required to invoke <code>Callback</code>.
     * @return the asynchronous response or <code>null</code>.
     * @throws JBIException if an error occurs invoking <code>Callback</code>.
     */
    public CRLMessageExchange invoke(Object key, Object data) throws JBIException;
    // TODO do we need to overload this to make onTimeout conditional?

    /**
     * Registers the {@link Callback} to invoke when an asynchronous
     * <code>Processor</code> is ready to complete the message exchange.
     * 
     * @param key The object key used to register <code>Callback</code>.
     * @param cback The <code>Callback</code> to invoke.
     */
    public void register(Object key, Callback cback);

    /**
     * Registers with a timeout the {@link Callback} to invoke when an asynchronous
     * <code>Processor</code> is ready to complete the message exchange.
     * <p>
     * <b>NOTE:</b> If a <code>Callback</code> times out, then 
     * {@link Callback} will be invoked.
     * 
     * @param key The object key used to register <code>Callback</code>.
     * @param cback The <code>Callback</code> to invoke.
     * @param timeout Duration in milliseconds until <code>Callback</code> times out.
     */
    public void register(Object key, Callback cback, long timeout);
    
    /**
     * Fetches a registered {@link Callback}.
     * 
     * @param key The object key used to register <code>Callback</code>.
     * @return a <code>Callback</code> or 
     *           <code>null</code> if 
     *                  1) the registered <code>Callback</code> timed out OR
     *                  2) no <code>Callback</code> was registered using key.
     *            
     */
//    public Callback lookupCallback(Object key);// TODO do we need this?
}
