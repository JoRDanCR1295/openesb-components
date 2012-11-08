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
 * @(#)DefaultInOnlyConsumer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc.impl;

import javax.jbi.JBIException;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.proc.InOnlyConsumer;
import com.sun.jbi.crl.util.I18n;

/**
 * Default implementation of {@link InOnlyConsumer}.
 * 
 * @author Kevan Simpson
 */
public class DefaultInOnlyConsumer extends AbstractInOnlyConsumer {
    /** @see com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyConsumer#processStatus(com.sun.jbi.crl.mep.exchange.CRLInOnly, com.sun.jbi.crl.mep.ExchangeContext) */
    public void processStatus(CRLInOnly msg, 
                              ExchangeContext ctx) throws JBIException {
    	log().info(I18n.loc("CRL-5010: {0} received {1} status for exchange: {2}", 
    						this.getClass().getSimpleName(),
    						String.valueOf(msg.getStatus()),
    						msg.getExchangeId()));
    }
}
