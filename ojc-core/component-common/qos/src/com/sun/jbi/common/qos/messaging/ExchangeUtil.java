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

package com.sun.jbi.common.qos.messaging;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.messaging.tracking.MessageTracking;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;

/**
 * Utility for message exchanges.
 * @author Kevan Simpson
 */
public class ExchangeUtil {
    public static final String FAULTCODE_PROPERTY_NAME = "com.sun.jbi.crl.faultcode";
    public static final String FAULTSTRING_PROPERTY_NAME = "com.sun.jbi.crl.faultstring";
    public static final String FAULTACTOR_PROPERTY_NAME = "com.sun.jbi.crl.faultactor";
    public static final String FAULTDETAIL_PROPERTY_NAME = "com.sun.jbi.crl.faultdetail";
    
    public enum FaultCode { Client, Server, VersionMismatch }
    
    public enum Systemic { transaction, security, message_tracking }

    /**
     * Extracts the &quot;IN&quot; {@link NormalizedMessage} from the 
     * specified message exchange and returns the jbi part elements.
     * 
     * @param mex The specified message exchange.
     * @return The jbi part elements of an <code>IN</code> message.
     * @throws Exception If an error occurs extracting content.
     */
    public static Element[] extractParts(MessageExchange mex) throws Exception {
        return extractParts(mex, true);
    }
    
    /**
     * Extracts the &quot;IN&quot; or &quot;OUT&quot; {@link NormalizedMessage} 
     * from the specified message exchange and returns the jbi part elements.
     * 
     * @param mex The specified message exchange.
     * @param inMessage <code>true</code> if <code>IN</code> message should be 
     *                  extracted, otherwise <code>false</code>.
     * @return The <code>IN</code> message's jbi parts.
     * @throws Exception If an error occurs extracting content.
     */
    public static Element[] extractParts(MessageExchange mex, boolean inMessage) throws Exception {
        return extractParts(inMessage ? getInMessage(mex) : getOutMessage(mex));
    }
    
    /**
     * Extracts the jbi:part elements from the specified normalized message.
     * @param nmsg A normalized <code>IN</code>, <code>OUT</code>, or 
     *              <code>Fault</code> message.
     * @return The message's jbi parts.
     * @throws Exception If an error occurs extracting content.
     */
    public static Element[] extractParts(NormalizedMessage nmsg) throws Exception {
        List<Element> list = new ArrayList<Element>();
        if (nmsg != null) {
            DOMSource src = XmlUtil.toDOMSource(nmsg.getContent());
            Node node = src.getNode();
            Element elem = (node instanceof Element) ? (Element) node
                    : ((node instanceof Document) 
                            ? ((Document) node).getDocumentElement() : null);
            if (elem != null) {
                // elem is expected to be jbi:message
                NodeList parts = elem.getElementsByTagNameNS(
                        JbiDescriptor.JBI_WSDL11_NS, "part");
                for (int i = 0, n = parts.getLength(); i < n; i++) {
                    list.add((Element) parts.item(i));
                }
            }
        }
        
        Element[] result = new Element[list.size()];
        list.toArray(result);
        return result;
    }
    
	/**
	 * Propagates all systemic properties related to security and transaction
	 * from one message exchange to another.
	 * 
	 * @param src The source message exchange.
	 * @param dest The destination message exchange.
	 * @return <code>true</code> if all systemic properties were propagated.
	 * @see #propagateSecurity(MessageExchange, MessageExchange)
	 * @see #propagateTransaction(MessageExchange, MessageExchange)
	 */
	public static boolean propagateSystemics(MessageExchange src, MessageExchange dest) {
		// propogate both, even if security fails
		return propagateSystemics(src, dest, 
		        Systemic.security ,Systemic.transaction);
	}

    /**
     * Propagates all systemic properties from one message exchange to another
     * related to the specified {@link Systemic} qualities.
     * 
     * @param src The source message exchange.
     * @param dest The destination message exchange.
     * @param qos One or more {@link Systemic} qualities.
     * @return <code>true</code> if all systemic properties were propagated.
     * @see #propagateSecurity(MessageExchange, MessageExchange)
     * @see #propagateTransaction(MessageExchange, MessageExchange)
     * @see MessageTracking#propagateTracking(MessageExchange, MessageExchange)
     */
    public static boolean propagateSystemics(MessageExchange src, 
                                             MessageExchange dest, 
                                             Systemic... qos) {
        if (qos != null) {
            boolean copied = true;
            for (Systemic sys : qos) {
                switch (sys) {
                    case transaction: {
                        copied &= propagateTransaction(src, dest);
                        break;
                    }
                    case security: {
                        copied &= propagateSecurity(src, dest);
                        break;
                    }
                    case message_tracking: {
                        copied &= MessageTracking.propagateTracking(src, dest);
                        break;
                    }
                }
            }
            return copied;
        }
        
        return false;
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
		
		FaultCode code = FaultCode.Server;
		Object fault = src.getProperty(FAULTCODE_PROPERTY_NAME);
		// some components may have put the enum on the exchange
		if (fault instanceof FaultCode) {
		    code = (FaultCode) fault;
		}
		else {    // force proper value since we don't put enum on exchange
		    try {
		        code = FaultCode.valueOf(String.valueOf(fault));
		    }
		    catch (Exception e) { /* ignore, default to FaultCode.Server */ }
		}
		
		return setErrorData(dest, 
					 (String) src.getProperty(FAULTSTRING_PROPERTY_NAME), 
					 code,
					 (String) src.getProperty(FAULTDETAIL_PROPERTY_NAME),
					 (String) src.getProperty(FAULTACTOR_PROPERTY_NAME));
	}
	
	/**
	 * Sets on the specified message exchange the following error properties:
	 * <ul>
	 * 		<li>{@link #FAULTSTRING_PROPERTY_NAME}</li>
	 *		<li>{@link #FAULTCODE_PROPERTY_NAME}</li>
	 *		<li>{@link #FAULTDETAIL_PROPERTY_NAME}</li>
	 *		<li>{@link #FAULTACTOR_PROPERTY_NAME}</li>
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
							"QOS-6013: Failed to set error data - 'error' and 'actor' cannot be empty!"));
			return false;
		}
		// description
		msg.setProperty(FAULTSTRING_PROPERTY_NAME, error);
		// fault code
		if (code == null) {
		    msg.setProperty(FAULTCODE_PROPERTY_NAME, FaultCode.Server.name());
		}
		else {
		    msg.setProperty(FAULTCODE_PROPERTY_NAME, code.name());
		}
		// detail
		if (Util.isEmpty(detail)) {
			msg.setProperty(FAULTDETAIL_PROPERTY_NAME,	// default detail message 
							I18n.loc("QOS-6015: Exchange({0}) terminated.  Please review logs for details.", 
									 msg.getExchangeId()));
		}
		else {
			msg.setProperty(FAULTDETAIL_PROPERTY_NAME, detail);
		}
		// actor
		msg.setProperty(FAULTACTOR_PROPERTY_NAME, actor);
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
     * Extracts the &quot;OUT&quot; {@link NormalizedMessage} from the 
     * specified message exchange, which must be {@link InOut}.
     * 
     * @param msg The specified exchange.
     * @return The <code>OUT</code> message of the specified exchange or <code>null</code>.
     */
    public static NormalizedMessage getOutMessage(MessageExchange msg) {
        if (msg instanceof InOut) {
            return ((InOut) msg).getOutMessage();
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
	 * @param content The <code>IN</code> message to set.
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
