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
 * @(#)AbstractCallback.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import javax.jbi.JBIException;

import com.sun.jbi.crl.mep.Callback;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;

/**
 * Abstract base class for {@link Callback} implementations.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractCallback implements Callback {
    private Object mData = null;
    private CRLMessageExchange mMessage = null;
    private CRLMessageExchangeFactory mFactory = null;
    
    /** @see com.sun.jbi.crl.mep.Callback#getData() */
    public Object getData() {
        return mData;
    }

    /** @see com.sun.jbi.crl.mep.Callback#getExchangeFactory() */
    public CRLMessageExchangeFactory getExchangeFactory() {
        return mFactory;
    }

    /** @see com.sun.jbi.crl.mep.Callback#getMessageExchange() */
    public CRLMessageExchange getMessageExchange() {
        return mMessage;
    }

    /** @see com.sun.jbi.crl.mep.Callback#onCallback(java.lang.Object) */
    public abstract CRLMessageExchange onCallback(Object data) throws JBIException;

    /** @see com.sun.jbi.crl.mep.Callback#onTimeout() */
    public abstract CRLMessageExchange onTimeout() throws JBIException;

    /** @see com.sun.jbi.crl.mep.Callback#setData(java.lang.Object) */
    public void setData(Object data) {
        mData = data;
    }

    /** @see com.sun.jbi.crl.mep.Callback#setExchangeFactory(com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory) */
    public void setExchangeFactory(CRLMessageExchangeFactory fac) {
        mFactory = fac;
    }

    /** @see com.sun.jbi.crl.mep.Callback#setMessageExchange(com.sun.jbi.crl.mep.exchange.CRLMessageExchange) */
    public void setMessageExchange(CRLMessageExchange msg) {
        mMessage = msg;
    }
}
