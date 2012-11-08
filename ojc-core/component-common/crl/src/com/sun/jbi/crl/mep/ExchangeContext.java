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
 * @(#)ExchangeContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;
import com.sun.jbi.crl.mep.proc.Processor;

/**
 * Represents the runtime context of a message exchange {@link Processor}.
 * 
 * @author Kevan Simpson
 */
public interface ExchangeContext {
    public CRLMessageExchangeFactory getExchangeFactory();

    /** Returns newly-created DOM document. */
    public Document newDocument();

    public CorrelationMap getCorrelationMap();
    public CallbackRegistry getCallbackRegistry();
    public EndpointManager getEndpointManager();
    
    public ServiceEndpoint lookupServiceEndpoint(QName serviceName, String endpointName);
    public void release();
}
