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
 * @(#)DefaultInOnlyProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc.impl;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.proc.InOnlyProvider;
import com.sun.jbi.crl.util.I18n;

/**
 * Default implementation of {@link InOnlyProvider}.
 * 
 * @author Kevan Simpson
 */
public class DefaultInOnlyProvider extends AbstractInOnlyProvider {
    /** @see com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyProvider#processIn(com.sun.jbi.crl.mep.exchange.CRLInOnly, com.sun.jbi.crl.mep.ExchangeContext) */
    public void processIn(CRLInOnly msg, ExchangeContext ctx) throws JBIException {
    	log().info(I18n.loc("CRL-5011: {0} received IN message for exchange: {2}", 
    						this.getClass().getSimpleName(), msg.getExchangeId()));

        if (msg.getError() != null) {
        	log().log(Level.WARNING,
        			  I18n.loc("CRL-6050: {0} received exchange with error: {1}", 
        					   this.getClass().getSimpleName(), 
        					   msg.getError().getMessage()), 
                      msg.getError());
        }
        else {
            msg.setStatus(ExchangeStatus.DONE);
        }
        // reply to consumer
        msg.send();
    }
}
