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
 * @(#)ExchangeUtil.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange;

import java.util.logging.Logger;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.crl.mep.exchange.CRLMessageExchange.FaultCode;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.Util;

/**
 * Utility for message exchanges.
 * @author Kevan Simpson
 */
public class ExchangeUtil {
	/**
	 * Propagates all systemic properties from one message exchange to another.
	 * 
	 * @param src The source message exchange.
	 * @param dest The destination message exchange.
	 * @return <code>true</code> if all systemic properties were propagated.
	 * @see #propagateSecurity(MessageExchange, MessageExchange)
	 * @see #propagateTransaction(MessageExchange, MessageExchange)
	 */
	public static boolean propagateSystemics(MessageExchange src, MessageExchange dest) {
		// propogate both, even if security fails
		return propagateSecurity(src, dest) & propagateTransaction(src, dest);
	}

	/**
	 * Propagates the {@link MessageExchange#JTA_TRANSACTION_PROPERTY_NAME} 
	 * property from one message exchange to another if and only if the source
	 * exchange is transacted.
	 * 
	 * @param src The source message exchange.
	 * @param dest The destination message exchange.
	 * @return <code>true</code> if transaction property is propagated.
	 * @see MessageExchange#isTransacted()
	 */
	public static boolean propagateTransaction(MessageExchange src, MessageExchange dest) {
        if (src != null && dest != null) {
        	if (src.isTransacted()) {
	            dest.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, 
	            				 src.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME));
        	}
            return true;
        }

        return false;
	}

	/**
	 * Propagates the <code>Subject</code> from one message exchange's IN message
	 * to another exchange's IN message.
	 * 
	 * @param src The source message exchange.
	 * @param dest The destination message exchange.
	 * @return <code>true</code> if the <code>Subject</code> is propagated.
	 * @see #getInMessage(MessageExchange)
	 * @see NormalizedMessage#getSecuritySubject()
	 */
	public static boolean propagateSecurity(MessageExchange src, MessageExchange dest) {
		if (src != null && dest != null) {
			NormalizedMessage from = getInMessage(src), to = getInMessage(dest);
			if (from != null && to != null) {
				to.setSecuritySubject(from.getSecuritySubject());
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Propagates the systemic error properties from one message exchange to 
	 * another.
	 * 
	 * @param src The source message exchange.
	 * @param dest The destination message exchange.
	 * @return <code>true</code> if error properties were propagated.
	 * @see #setErrorData(MessageExchange, String, FaultCode, String, String)
	 */
	public static boolean propagateErrorData(MessageExchange src, MessageExchange dest) {
		if (src == null || dest == null) return false;
		
		return setErrorData(dest, 
					 (String) src.getProperty(CRLMessageExchange.FAULTSTRING_PROPERTY_NAME), 
					 (FaultCode) src.getProperty(CRLMessageExchange.FAULTCODE_PROPERTY_NAME),
					 (String) src.getProperty(CRLMessageExchange.FAULTDETAIL_PROPERTY_NAME),
					 (String) src.getProperty(CRLMessageExchange.FAULTACTOR_PROPERTY_NAME));
	}
	
	/**
	 * Sets on the specified message exchange the following error properties:
	 * <ul>
	 * 		<li>{@link CRLMessageExchange#FAULTSTRING_PROPERTY_NAME}</li>
	 *		<li>{@link CRLMessageExchange#FAULTCODE_PROPERTY_NAME}</li>
	 *		<li>{@link CRLMessageExchange#FAULTDETAIL_PROPERTY_NAME}</li>
	 *		<li>{@link CRLMessageExchange#FAULTACTOR_PROPERTY_NAME}</li>
	 * </ul> 
	 * <p>
	 * <b>NOTE:</b> The <code>error</code> and the <code>actor</code> arguments
	 * cannot be empty (i.e. zero-length) or <code>null</code>. The <code>detail</code>
	 * argument is optional; a default message directing user to the log will be supplied. 
	 * 
	 * @param msg The exchange on which to set the properties.
	 * @param error The high-level description of the error.
	 * @param code The {@link FaultCode} describing the role of the error.
	 * @param detail A detailed description of the error or <code>null</code>.
	 * @param actor The name of the component which owns the specified exchange.
	 * @return <code>true</code> if all properties are valid and set on the exchange.
	 */
	public static boolean setErrorData(MessageExchange msg, 
									   String error, FaultCode code, 
									   String detail, String actor) {
		if (Util.isEmpty(error) || Util.isEmpty(actor)) {
			Logger.getLogger(ExchangeUtil.class.getName())
					.warning(I18n.loc(
							"CRL-6054: Failed to set error data - 'error' and 'actor' cannot be empty!"));
			return false;
		}
		
		msg.setProperty(CRLMessageExchange.FAULTSTRING_PROPERTY_NAME, error);
		msg.setProperty(CRLMessageExchange.FAULTCODE_PROPERTY_NAME, String.valueOf(code));
		if (Util.isEmpty(detail)) {
			msg.setProperty(CRLMessageExchange.FAULTDETAIL_PROPERTY_NAME,	// default detail message 
							I18n.loc("CRL-6055: Exchange({0}) terminated.  Please review logs for details.", 
									 msg.getExchangeId()));
		}
		else {
			msg.setProperty(CRLMessageExchange.FAULTDETAIL_PROPERTY_NAME, detail);
		}
		msg.setProperty(CRLMessageExchange.FAULTACTOR_PROPERTY_NAME, actor);
		return true;
	}

	/**
	 * Extracts the &quot;IN&quot; {@link NormalizedMessage} from the 
	 * specified message exchange, which must be {@link InOut} or {@link InOnly}.
	 * 
	 * @param msg The specified exchange.
	 * @return The IN message of the specified exchange or <code>null</code>.
	 */
	public static NormalizedMessage getInMessage(MessageExchange msg) {
		if (msg instanceof InOnly) {
			return ((InOnly) msg).getInMessage();
		}
		else if (msg instanceof InOut) {
			return ((InOut) msg).getInMessage();
		}
		else {
			return null;
		}
	}
	
	/**
	 * Sets the &quot;IN&quot; {@link NormalizedMessage} on the specified
	 * message exchange, which must be {@link InOut} or {@link InOnly}.
	 * 
	 * @param msg The specified exchange.
	 * @param content The IN message to set.
	 * @throws MessagingException if setting the content fails.
	 */
	public static void setInMessage(MessageExchange msg, NormalizedMessage content) 
	        throws MessagingException {
	    if (msg == null || content == null) return;
	    
        if (msg instanceof InOnly) {
            ((InOnly) msg).setInMessage(content);
        }
        else if (msg instanceof InOut) {
            ((InOut) msg).setInMessage(content);
        }
	}
}
