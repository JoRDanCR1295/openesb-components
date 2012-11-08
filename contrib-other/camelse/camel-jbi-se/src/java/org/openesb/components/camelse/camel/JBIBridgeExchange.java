/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
/*
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.openesb.components.camelse.camel;

import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import org.apache.camel.CamelContext;
import org.apache.camel.Exchange;
import org.apache.camel.ExchangePattern;
import org.apache.camel.Message;
import org.apache.camel.impl.DefaultExchange;

/**
 *
 * @author chikkala
 */
public class JBIBridgeExchange extends DefaultExchange {

    public static final String JBI_OPERATION_PROP = "org.openesb.camelse.jbi.operation";
    
    private static final Logger LOG = Logger.getLogger(JBIBridgeExchange.class.getName());
    private MessageExchange jbiExchange;

    public JBIBridgeExchange(CamelContext camelContext, ExchangePattern pattern) {
        super(camelContext, pattern);
        this.jbiExchange = null;
        populateJBIExchangeProperties();
    }

    public JBIBridgeExchange(CamelContext camelContext, InOnly jbiEx) {
        super(camelContext, ExchangePattern.InOnly);
        setIn(new JBIBridgeMessage(jbiEx.getInMessage()));
        this.jbiExchange = jbiEx;
        populateJBIExchangeProperties();
    }

    public JBIBridgeExchange(CamelContext camelContext, InOut jbiEx) {
        super(camelContext, ExchangePattern.InOut);
        setIn(new JBIBridgeMessage(jbiEx.getInMessage()));
        this.jbiExchange = jbiEx;
        populateJBIExchangeProperties();
    }

    public JBIBridgeExchange(DefaultExchange parent, MessageExchange jbiEx) {
        super(parent);
        this.jbiExchange = jbiEx;
        populateJBIExchangeProperties();
    }

    public MessageExchange getJBIExchange() {
        return jbiExchange;
    }

    public void setJBIExchange(MessageExchange jbiEx) {
        this.jbiExchange = jbiEx;
        populateJBIExchangeProperties();
    }
    private void populateJBIExchangeProperties() {
        if (this.jbiExchange != null ) {
            // populate jbi exchange properties
            Set propNames = this.jbiExchange.getPropertyNames();
            for ( Object propName : propNames) {
                setProperty((String)propName, this.jbiExchange.getProperty((String)propName));
            }
            // always set the operation local name.
            String opName = null;
            QName opQName = this.jbiExchange.getOperation();
            if ( opQName != null ) {
                opName = opQName.toString();
            }
            setProperty(JBI_OPERATION_PROP, opName);
        }
    }
    
    @Override
    public Exchange newInstance() {
        return new JBIBridgeExchange(this, getJBIExchange());
    }

    private NormalizedMessage createJBIMessage(boolean fault) {
        NormalizedMessage nm = null;
        try {
            if (this.jbiExchange != null) {
                if (fault) {
                    nm = this.jbiExchange.createFault();
                } else {
                    nm = this.jbiExchange.createMessage();
                }
            }
        } catch (MessagingException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
        return nm;
    }

    @Override
    protected Message createFaultMessage() {
        NormalizedMessage nm = createJBIMessage(true);
        return new JBIBridgeMessage(nm);
    }

    @Override
    protected Message createInMessage() {
        NormalizedMessage nm = createJBIMessage(false);
        return new JBIBridgeMessage(nm);
    }

    @Override
    protected Message createOutMessage() {
        NormalizedMessage nm = createJBIMessage(false);
        return new JBIBridgeMessage(nm);
    }
}
