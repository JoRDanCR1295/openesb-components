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
 * @(#)ASManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jca;

import java.util.Iterator;
import java.util.Map;
import javax.resource.spi.ActivationSpec;

import com.stc.jmsjca.core.RAJMSActivationSpec;
import com.stc.jmsjca.unifiedjms.RAUnifiedActivationSpec;

import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSOptions;
import com.sun.jbi.jmsbc.extensions.JMSOption;

import com.sun.jbi.internationalization.Messages;

/**
 *
 * Abstracts much of the RA ActivationSpec creation from the BC.
 */
public class ASManager {
        
    private static final String enableJMSJCAHoldUntilAck = "1";
    
    private static ASManager singleton = null;
    
    private static final Messages mMessages =
        Messages.getMessages(ASManager.class);
    
    /** Creates a new instance of ASManager */
    private ASManager() {
    }
    
    public static ASManager getInstance() {
        if (singleton == null) {
            singleton = new ASManager();
        }
        return singleton;
    }
    
    public ActivationSpec getActivationSpec (String ndcContext,
                                             JMSAddress jmsAddress,
                                             JMSOperation jmsOperation) 
    throws UnsupportedProviderException, ManagerException {
        RAJMSActivationSpec as = new RAUnifiedActivationSpec();
            
        //
        // Set the ActivationSpec bean properties
        //
        as.setDestination(jmsOperation.getDestination());
        as.setDestinationType(
                getQualifiedDestinationType(jmsOperation.getDestinationType()));
        if (!Util.isEmpty(jmsOperation.getClientID())) {
            as.setClientId(jmsOperation.getClientID());
        }

        as.setConcurrencyMode(jmsOperation.getConcurrencyMode());
        
        // If Topic, set the subscription durability bean properties
        if (jmsOperation.getDestinationType().equals(JMSConstants.TOPIC)) {
            if (jmsOperation.getSubscriptionDurability() != null &&
                jmsOperation.getSubscriptionDurability().equals(JMSConstants.DURABLE)) {
                as.setSubscriptionDurability(Constants.SUBSCRIPTION_DURABILITY_DURABLE);
                if (!Util.isEmpty(jmsOperation.getSubscriptionName())) {
                    as.setSubscriptionName(jmsOperation.getSubscriptionName());
                } else {
                    String errMsg = mMessages.getString(
                                "JMSBC-E0100.DurableSubscriberNoSubscriptionName",
                                new Object [] {jmsOperation.getBindingOperation().getName()});
                    throw new ManagerException(errMsg);
                }
            } else {
                as.setSubscriptionDurability(Constants.SUBSCRIPTION_DURABILITY_NONDURABLE);
            }
        }

        // Set message selector if provided
        if (!Util.isEmpty(jmsOperation.getMessageSelector())) {
            as.setMessageSelector(jmsOperation.getMessageSelector());
        }

        // Concurrent consumers
        if (jmsOperation.getMaxConcurrentConsumers() != null && 
            jmsOperation.getMaxConcurrentConsumers().intValue() > 0) {
            as.setEndpointPoolMaxSize(jmsOperation.getMaxConcurrentConsumers());
        }
        
        // Inbound requests are batched?
        int batchSize = 0;
        if (jmsOperation.getBatchSize() != null && jmsOperation.getBatchSize().intValue() > 0) {
            batchSize = jmsOperation.getBatchSize().intValue();
        }
                
        //
        // Set additonal "RA" options on ActivationSpec if any
        //
        JMSOptions jmsOpts = jmsOperation.getOptions();        
        if (jmsOpts != null) {
            Map optsMap = jmsOpts.getOptions();
            StringBuffer optionsStr = new StringBuffer();
            if (optsMap != null) {
                Iterator optsIter = optsMap.keySet().iterator();
                while (optsIter.hasNext()) {
                    JMSOption jmsOption = (JMSOption)optsMap.get(optsIter.next());
                    String optName = jmsOption.getName();
                    String optValue = jmsOption.getValue();
                    if (optName.equals(Constants.JMSJCA_AS_OPTION_ConcurrencyMode)) {
                        // CC is AS bean property
                        as.setConcurrencyMode(optValue);
                    } else { // append the option to our list
                        optionsStr.append(optName);
                        optionsStr.append("=");
                        optionsStr.append(optValue);
                        optionsStr.append('\n');
                    }
                }
                String jmsjcaOptions = null;
        		if (jmsAddress.getJmsjcaOptions() != null
        				&& jmsAddress.getJmsjcaOptions().getOptions() != null) {
        			jmsjcaOptions = jmsAddress.getJmsjcaOptions().getOptions();
        		}
        		String options = Util.mergeOptions(optionsStr.toString(), jmsjcaOptions, jmsOperation
        				.getDestinationType().equals(JMSConstants.TOPIC));
                as.setOptions(options);
            }
        }
                
        // Use HoldUntilAck for asynch message processing with jmsjca ra
        as.setHoldUntilAck(enableJMSJCAHoldUntilAck);
        
        // Make use of batch size
        if (batchSize > 0) {
            as.setBatchSize(batchSize + "");
        }
        
        // RedeliveryHandling by ra
        if (jmsOperation.getRedeliveryHandling() != null) {
            as.setRedeliveryHandling(jmsOperation.getRedeliveryHandling());
        }
        
        // Set the NDC context
        if (ndcContext != null) {
            as.setContextName(ndcContext);
        }
        
        return as;
    }
    
    private String getQualifiedDestinationType (String destinationType) {
        if (destinationType.equalsIgnoreCase(JMSConstants.TOPIC)) {
            return javax.jms.Topic.class.getName();
        } else {
            return javax.jms.Queue.class.getName();
        }
    }
    
}
