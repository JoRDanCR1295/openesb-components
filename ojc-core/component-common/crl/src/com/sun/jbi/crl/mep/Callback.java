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
 * @(#)Callback.java 
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
 * Defines callback for asynchronous processing.
 * 
 * @author Kevan Simpson
 */
public interface Callback {
    /**
     * Invoked when the <code>Callback</code> is triggered.
     * 
     * @param data The data which triggered the <code>Callback</code>. 
     * @return The asynchronous response or <code>null</code>.
     * @throws JBIException if an error occurs processing <code>Callback</code>.
     */
    public CRLMessageExchange onCallback(Object data) throws JBIException;
    
    /**
     * Invoked when the <code>Callback</code> has timed out.
     * 
     * @return The asynchronous response or <code>null</code>.
     * @throws JBIException if an error occurs processing <code>Callback</code>.
     */
    public CRLMessageExchange onTimeout() throws JBIException;
    
    public Object getData();
    public void setData(Object data);
    
    public CRLMessageExchange getMessageExchange();
    public void setMessageExchange(CRLMessageExchange msg);
   
    public CRLMessageExchangeFactory getExchangeFactory();
    public void setExchangeFactory(CRLMessageExchangeFactory fac);
}
