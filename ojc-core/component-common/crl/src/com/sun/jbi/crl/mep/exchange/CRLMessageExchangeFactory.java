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
 * @(#)CRLMessageExchangeFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.RobustInOnly;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.exchange.impl.AbstractCRLMessageExchange;
import com.sun.jbi.crl.mep.exchange.impl.DefaultCRLInOnly;
import com.sun.jbi.crl.mep.exchange.impl.DefaultCRLInOptionalOut;
import com.sun.jbi.crl.mep.exchange.impl.DefaultCRLInOut;
import com.sun.jbi.crl.mep.exchange.impl.DefaultCRLRobustInOnly;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Factory to create instances of {@link CRLMessageExchange}.
 * 
 * @author Kevan Simpson
 */
public class CRLMessageExchangeFactory {
    private ManagerContext mContext = null;
    private MessageExchangeFactory mJbiFactory = null;
    
    protected CRLMessageExchangeFactory(ManagerContext ctx) {
        mContext = ctx;
        try {
            mJbiFactory = ctx.getDeliveryChannel().createExchangeFactory();
        }
        catch (MessagingException me) {
        	LogUtil.getLogger(ctx, CRLMessageExchangeFactory.class.getPackage().getName())
        			.log(Level.WARNING,
	                     I18n.loc("CRL-6029: Failed to create \"{0}\" exchange factory: {1}", 
	                       	      ctx.getComponentName(), me.getMessage()), 
	                     me);
        }
    }
    
    public static CRLMessageExchangeFactory getInstance(ManagerContext ctx) {
        // TODO cache?
        return new CRLMessageExchangeFactory(ctx);
    }
    
    public CRLMessageExchange createExchange(MessageExchange me) 
            throws MessagingException {
        return createExchange(me, null);
    }
    
    public CRLMessageExchange createExchange(ExchangePattern p) 
            throws MessagingException {
        return createExchange(null, p);
    }
    
    protected CRLMessageExchange createExchange(MessageExchange me, ExchangePattern p) 
        throws MessagingException {
        if (p == null && me != null) {
            p = ExchangePattern.valueOf(me);
        }
        
        if (p != null) {
            if (me == null) {
                me = createJbiExchange(p);
            }
            switch (p) {
                case IN_OUT: {
                    return new DefaultCRLInOut(getContext(), (InOut) me);
                }
                case IN_ONLY: {
                    return new DefaultCRLInOnly(getContext(), (InOnly) me);
                }
                case IN_OPTIONAL_OUT: {
                    return new DefaultCRLInOptionalOut(getContext(), 
                                                       (InOptionalOut) me);
                }
                case ROBUST_IN_ONLY: {
                    return new DefaultCRLRobustInOnly(getContext(), 
                                                      (RobustInOnly) me);
                }
            }
        }
        else {
            throw new MessagingException(
            		I18n.loc("CRL-6030: Cannot create exchange from NULL pattern!"));
        }
        return null;
    }

    protected MessageExchange createJbiExchange(ExchangePattern ep) throws MessagingException {
        switch (ep) {
            case IN_ONLY: {
                return mJbiFactory.createInOnlyExchange();
            }
            case IN_OPTIONAL_OUT: {
                return mJbiFactory.createInOptionalOutExchange();
            }
            case IN_OUT: {
                return mJbiFactory.createInOutExchange();
            }
            case ROBUST_IN_ONLY: {
                return mJbiFactory.createRobustInOnlyExchange();
            }
            default: return null;
        }
    }
    
    public InvokableExchange createInvokableExchange(ExchangePattern p, EndpointInfo info) 
            throws MessagingException {
        if (p == null || info == null) {
        	return null;
        }
        else {
        	switch (p) {
        		case IN_ONLY: {
        			return new InvokableInOnly(getContext(), 
        									   (InOnly) createJbiExchange(p), 
        									   info);
        		}
        		case IN_OUT: {
        			return new InvokableInOut(getContext(),
        									  (InOut) createJbiExchange(p),
        									  info);
        		}
        		default: {
        			return new DefaultInvokableExchange(getContext(), 
                                                      	createJbiExchange(p), 
                                                      	info);
        		}
        	}
        }
    }

    protected ManagerContext getContext() {
        return mContext;
    }
    protected void setContext(ManagerContext ctx) {
        mContext = ctx;
    }

    private static class DefaultInvokableExchange extends AbstractCRLMessageExchange 
                                                  implements InvokableExchange {
        public DefaultInvokableExchange(ManagerContext ctx,
                                        MessageExchange msg,
                                        EndpointInfo info) {
            super(ctx, msg, info);
        }
        
        /** @see com.sun.jbi.crl.mep.exchange.InvokableExchange#invoke(javax.xml.transform.Source, javax.xml.namespace.QName) */
        public void invoke(Source src, QName operation) throws JBIException {
        	super.invoke(src, operation);	// makes this method public
        }
    }
    
    private static class InvokableInOut extends DefaultCRLInOut 
    									implements InvokableExchange {
    	public InvokableInOut(ManagerContext ctx,
    						  InOut msg,
    						  EndpointInfo info) {
    		super(ctx, msg, info);
    	}

        /** @see com.sun.jbi.crl.mep.exchange.InvokableExchange#invoke(javax.xml.transform.Source, javax.xml.namespace.QName) */
        public void invoke(Source src, QName operation) throws JBIException {
        	super.invoke(src, operation);	// makes this method public
        }
    }

    private static class InvokableInOnly extends DefaultCRLInOnly 
    									 implements InvokableExchange {

    	public InvokableInOnly(ManagerContext ctx,
    						   InOnly msg,
    						   EndpointInfo info) {
    		super(ctx, msg, info);
    	}

        /** @see com.sun.jbi.crl.mep.exchange.InvokableExchange#invoke(javax.xml.transform.Source, javax.xml.namespace.QName) */
        public void invoke(Source src, QName operation) throws JBIException {
        	super.invoke(src, operation);	// makes this method public
        }
    }
    
    // TODO add InvokableRobustInOnly and InvokeableInOptionalOut
}
