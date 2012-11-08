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
 * @(#)DefaultInOutProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc.impl;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.proc.InOutProvider;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.xml.XmlUtil;

/**
 * Default implementation of {@link InOutProvider}.
 * 
 * @author Kevan Simpson
 */
public class DefaultInOutProvider extends AbstractInOutProvider {
    /** @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processStatus(com.sun.jbi.crl.mep.exchange.CRLInOut, com.sun.jbi.crl.mep.ExchangeContext) */
    public void processStatus(CRLInOut msg, 
                              ExchangeContext ctx) throws JBIException {
    	log().info(I18n.loc("CRL-5010: {0} received {1} status for exchange: {2}", 
							this.getClass().getSimpleName(),
							String.valueOf(msg.getStatus()),
							msg.getExchangeId()));
    }

    /** @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processIn(com.sun.jbi.crl.mep.exchange.CRLInOut, com.sun.jbi.crl.mep.ExchangeContext) */
    public void processIn(CRLInOut msg, 
                          ExchangeContext ctx) throws JBIException {
    	log().info(I18n.loc("CRL-5011: {0} received IN message for exchange: {2}", 
							this.getClass().getSimpleName(), msg.getExchangeId()));

    	if (msg.getError() != null) {
        	log().log(Level.WARNING,
      			  	  I18n.loc("CRL-6050: {0} received exchange with error: {1}", 
      			  			   this.getClass().getSimpleName(), 
      			  			   msg.getError().getMessage()), 
      			  	  msg.getError());
        }

        // copy incoming message if DOMSource
        if (msg.getInMessage().getContent() instanceof DOMSource) {
        	try {
	            DOMSource in = XmlUtil.toDOMSource(msg.getInMessage().getContent());
	            DOMSource out = new DOMSource(in.getNode());
	            msg.reply(out);
        	}
        	catch (Exception e) {
        		msg.sendError(e);
        	}
        }
        else {  // otherwise, send back without OUT message
            msg.send();
        }
    }
}
