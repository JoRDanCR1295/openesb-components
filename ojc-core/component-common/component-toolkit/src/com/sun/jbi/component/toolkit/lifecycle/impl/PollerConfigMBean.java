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
 * @(#)PollerConfigMBean.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.lifecycle.impl;

import javax.jbi.messaging.MessageExchange;
import javax.management.MBeanException;

/**
 * Parent interface for component configuration MBeans needing to manage the
 * number of {@link AcceptPoller}s and exchange handling threads.
 * 
 * @author Kevan Simpson
 */
public interface PollerConfigMBean {
    /**
     * Fetches the threading configuration for handling exchanges.
     * <p>
     * The format of this property is expected to be a comma-delimited list of 
     * up to three positive integers, each of which will indicate the maximum
     * size of a thread pool for handling exchanges. Message exchanges will be 
     * routed based on {@link javax.jbi.messaging.MessageExchange.Role} and
     * {@link javax.jbi.messaging.ExchangeStatus}. To which pool is determined
     * by the number of pools configured:
     * <dl>
     *      <dt>Non-numeric value | Integer less than 1</dt>
     *      <dd>No pools, exchange handling is synchronous (uses {@link AcceptPoller} threads).</dd>
     *      <dt>"x,y,z"</dt>
     *      <dd>x is the # of threads handling requests (
     *          {@link MessageExchange.Role#PROVIDER}/{@link ExchangeStatus#ACTIVE),<br>
     *          y is the # of threads handling status ACKs, and<br>
     *          {@link MessageExchange.Role#PROVIDER}/not-{@link ExchangeStatus#ACTIVE),<br>
     *          z is the # of threads handling replies ({@link MessageExchange.Role#CONSUMER}). 
     *      <dt>"x,y"</dt>
     *      <dd>x is the # of threads handling requests (
     *          {@link MessageExchange.Role#PROVIDER}/{@link ExchangeStatus#ACTIVE),<br>
     *          y is the # of threads handling all other message exchanges.</dd>
     *      <dt>"x"</dt>
     *      <dd>where x is the # of threads that handle all exchanges.</dd>
     * </dl>
     *  
     * @return the threading configuration for handling exchanges.
     */
    public String getExchangeThreading();
    
    /**
     * Fetches the number of NMR-polling threads the component manages.
     * @return the number of NMR-polling threads the component manages.
     */
    public Integer getPollerCount();
    
    /**
     * Sets the threading configuration for handling exchanges.
     * <p>
     * The format of this property is expected to be a comma-delimited list of 
     * up to three positive integers, each of which will indicate the maximum
     * size of a thread pool for handling exchanges. Message exchanges will be 
     * routed based on {@link javax.jbi.messaging.MessageExchange.Role} and
     * {@link javax.jbi.messaging.ExchangeStatus}. To which pool is determined
     * by the number of pools configured:
     * <dl>
     *      <dt>Non-numeric value | Integer less than 1</dt>
     *      <dd>No pools, exchange handling is synchronous (uses {@link AcceptPoller} threads).</dd>
     *      <dt>"x,y,z"</dt>
     *      <dd>x is the # of threads handling requests (
     *          {@link MessageExchange.Role#PROVIDER}/{@link ExchangeStatus#ACTIVE),<br>
     *          y is the # of threads handling status ACKs, and<br>
     *          {@link MessageExchange.Role#PROVIDER}/not-{@link ExchangeStatus#ACTIVE),<br>
     *          z is the # of threads handling replies ({@link MessageExchange.Role#CONSUMER}). 
     *      <dt>"x,y"</dt>
     *      <dd>x is the # of threads handling requests (
     *          {@link MessageExchange.Role#PROVIDER}/{@link ExchangeStatus#ACTIVE),<br>
     *          y is the # of threads handling all other message exchanges.</dd>
     *      <dt>"x"</dt>
     *      <dd>where x is the # of threads that handle all exchanges.</dd>
     * </dl>
     *  
     * @param cfg the threading configuration for handling exchanges.
     * @throws MBeanException if an error occurs persisting configuration changes.
     */
    public void setExchangeThreading(String cfg) throws MBeanException;

    /**
     * Sets the number of NMR-polling threads the component manages.
     * @param count The number of NMR-polling threads the component manages.
     * @throws MBeanException if an error occurs persisting configuration changes.
     */
    public void setPollerCount(Integer count) throws MBeanException;
}
