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
 * @(#)InboundMessageProcessorFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;

import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;

import javax.resource.spi.UnavailableException;
import javax.resource.spi.endpoint.MessageEndpoint;

import javax.jbi.component.ComponentContext;

import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSOperation;

import com.sun.jbi.jmsbc.jca.MessagListenerEndpointFactory;

import com.sun.jbi.jmsbc.mbeans.RuntimeConfiguration;

import com.sun.jbi.jmsbc.recovery.LoggableXAResource;
import com.sun.jbi.jmsbc.util.AlertsUtil;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;

/**
 *
 * Factory of InboundMessageProcessorListenerEndpoint(s)
 */
public class InboundMessageProcessorFactory extends MessagListenerEndpointFactory {

    private static final Messages mMessages =
        Messages.getMessages(InboundMessageProcessorFactory.class);
    private static final Logger mLog =
        Messages.getLogger(InboundMessageProcessorFactory.class);
    
    private ComponentContext context = null;
    private Endpoint endpoint = null;
    private JMSOperation jmsOp = null;
    private Map inboundMessageExchanges = null;  // used by receive channels for saving message exchanges for inbound request/reply exchanges    
    
    /****
     * NOTE: for xa recovery test only
     */
    private boolean allowCrashOnCommit = false; // for xa recovery test only
    private String notifyToCrashFile = null;
    private boolean redeliveryApplied;
    
    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif =
                    (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                /*
                if (attrName.equals("AllowCrashOnCommit")) {  // todo: change this to use environment map
                    Boolean newVal = (Boolean) (attrNotif.getNewValue());
                    setAllowCrashOnCommit(newVal.booleanValue());
                } else if (attrName.equals("NotifyToCrashFile")) { // todo: change this to use environment map
                    String newVal = (String) (attrNotif.getNewValue());
                    setCrashNotificationFile(newVal);                    
                }
                 */
            }
        }
    };
    
    
    /** Creates a new instance of InboundMessageProcessorFactory */
    public InboundMessageProcessorFactory(ComponentContext context,
                                          Endpoint endpoint,
                                          JMSOperation jmsOp,
                                          Map inboundMessageExchanges,
                                          boolean redeliveryApplied) {
        super(isDeliveryTransacted(jmsOp.getTransaction()));
        
        this.context = context;
        this.endpoint = endpoint;
        this.jmsOp = jmsOp;
        this.inboundMessageExchanges = inboundMessageExchanges;
        this.redeliveryApplied = redeliveryApplied;
    }

    
    public MessageEndpoint createEndpoint (XAResource xar) 
    throws UnavailableException {
        XAResource xaResource = xar;
        if (JMSBindingComponent.ALLOW_CRASH_ON_COMMIT)
            xaResource = new LoggableXAResource(xar); 
        
        MessageEndpoint me = null;
        try {
            me = new InboundMessageProcessorListenerEndpoint (context,
                                                              xaResource,
                                                              endpoint,                
                                                              jmsOp,
                                                              inboundMessageExchanges,
                                                              redeliveryApplied);
            
        } catch (Throwable t) {
            mLog.log(Level.SEVERE,
                    "JMSBC-E0701.CreateMessageEndpointFailed");    
            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0701.CreateMessageEndpointFailed"), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    endpoint.getServiceUnitID(), 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0701");
            throw new UnavailableException (t);
        }
        
        return me;
    }
        
    private static boolean isDeliveryTransacted(String transaction) {
        return transaction.equals(JMSConstants.TRANSACTION_XA);
    }
    
    private void setAllowCrashOnCommit (boolean val) {
        this.allowCrashOnCommit = val;
    }
    
    private void setCrashNotificationFile (String notifyToCrashFile) {
        this.notifyToCrashFile = notifyToCrashFile;
    }
}
