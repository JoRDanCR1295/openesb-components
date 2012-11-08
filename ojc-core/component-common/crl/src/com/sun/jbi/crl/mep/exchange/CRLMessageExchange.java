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
 * @(#)CRLMessageExchange.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange;


import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.mep.Callback;

/**
 * Defines a view of JBI message exchanges.
 * <p>
 * It is expected that any implementation of this interface will account
 * for and prevent the sending of itself to the NMR; instead, only the
 * underlying <code>MessageExchange</code> will be exposed external to
 * the JBI component using instances of this class.
 * 
 * @author Kevan Simpson
 */
public interface CRLMessageExchange extends MessageExchange {
    public static final String FAULTCODE_PROPERTY_NAME = "com.sun.jbi.crl.faultcode";
    public static final String FAULTSTRING_PROPERTY_NAME = "com.sun.jbi.crl.faultstring";
    public static final String FAULTACTOR_PROPERTY_NAME = "com.sun.jbi.crl.faultactor";
    public static final String FAULTDETAIL_PROPERTY_NAME = "com.sun.jbi.crl.faultdetail";
    
    public enum FaultCode { Client, Server, VersionMismatch };
    
    /**
     * Sends the JBI {@link MessageExchange} to the NMR channel.
     * <p>
     * <b>NOTE:</b> The implementation is responsible for providing access to channel.
     * The implementation is also required to not send itself to the NMR, 
     * thereby absolving other JBI components from the responsibility of
     * having CRL as a dependency.
     * 
     * @throws JBIException if an error occurs sending exchange.
     */
    public void send() throws JBIException;
    
    /**
     * Terminates this exchange by setting the status to 
     * {@link ExchangeStatus#ERROR} and sends to the NMR channel.
     * <p>
     * <b>NOTE:</b> The implementation is responsible for providing access to channel.
     * 
     * @param error The error that occurred.
     * @throws JBIException if an error occurs sending exchange.
     */
    public void sendError(Exception error) throws JBIException;

    /**
     * Terminates this exchange by setting the status to 
     * {@link ExchangeStatus#ERROR}, sets the specified error properties,
     * and sends to the NMR channel.
     * <p>
     * <b>NOTE 1:</b> The implementation is responsible for providing access to channel.
     * <p>
     * <b>NOTE 2:</b> The error properties must be set with the specified parameters as follows:
     * <ul>
     * 		<li>{@link #FAULTSTRING_PROPERTY_NAME} set to "error"</li>
     * 		<li>{@link #FAULTCODE_PROPERTY_NAME} set to "code"</li>
     * 		<li>{@link #FAULTDETAIL_PROPERTY_NAME} set to "detail"</li>
     * 		<li>{@link #FAULTACTOR_PROPERTY_NAME} set to the component name</li>
     * </ul>
     * <p>
     * <b>NOTE: 3</b> The exception set on this exchange (via {@link MessageExchange#setError(Exception)})
     * MUST be an instance of {@link java.lang.Exception}.  If the exception 
     * parameter "e" is <code>null</code>, a new {@link Exception#Exception(String)}
     * is instantiated passing "error" to the constructor.  Otherwise, instantiate
     * a new {@link Exception#Exception(String, Throwable)} passing "error" and "e".
     * 
     * @param error A high-level description of the error.
     * @param code A {@link FaultCode} describing responsible role.
     * @param detail Detailed description of the error, including how to resolve it.
     * @param e The underlying cause of the error, may be <code>null</code>.
     * @throws JBIException If an error occurs sending the exchange.
     */
    public void sendError(String error, FaultCode code, String detail, Exception e) throws JBIException;
    
    /**
     * Fetches the {@link Callback} associated with this exchange's <code>Processor</code>.
     * @return the associated <code>Callback</code>.
     */
    public Callback getCallback();
    
    /**
     * Specifies a {@link Callback} with this exchange.
     * @param cback A component-specific <code>Callback</code> instance.
     */
    public void setCallback(Callback cback);//TODO should this be public? used by CRL
}
