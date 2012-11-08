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
 * @(#)MCFManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jca;

import java.util.Iterator;
import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;

import java.util.logging.Logger;
import java.util.logging.Level;

import javax.resource.spi.ManagedConnectionFactory;
import javax.resource.spi.ResourceAdapter;


import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSOptions;
import com.sun.jbi.jmsbc.extensions.JMSOption;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.alerter.NotificationEvent;

import com.stc.jmsjca.core.XManagedConnectionFactory;
import com.stc.jmsjca.core.XMCFQueueXA;
import com.stc.jmsjca.core.XMCFTopicXA;



/**
 * Abstracts much of the RA ManagedConnectionFactory creation from the BC.
 */
public class MCFManager {
    
    private static final String JMSJCA_BYPASS_RA = "JMSJCA.BypassRA";
    private static final Messages mMessages =
        Messages.getMessages(MCFManager.class);
    private static final Logger mLogger =
        Messages.getLogger(MCFManager.class);    
    
    private static MCFManager singleton = null;
    private static final long DEFAULT_POOLTIMEOUT = 0;  // wait indefinitely until connection is available
    
    private Map mcfMap = Collections.synchronizedMap(new HashMap());
    
    private MCFManager() {}
    
    public static MCFManager getInstance() {
        if (singleton == null) {
            singleton = new MCFManager();
        }
        return singleton;
    }    
    
    synchronized public ManagedConnectionFactory acquireMCF (JMSAddress jmsAddress,
                                                             JMSOperation jmsOperation) {

        JMSOptions options = jmsOperation.getOptions();

        if (options == null) {
            options = new JMSOptions();
        }

        if (options.getOptions() == null) {
            Map optionsMap = new HashMap();
            options.setOptions(optionsMap);                
        }

        Map optionsMap = options.getOptions();
        
        handleMappingToOptions (optionsMap, jmsAddress, jmsOperation);
        
        MCFEntry mcfEnt = matchOrCreateMCF(jmsAddress, jmsOperation, options);
        mcfEnt.incrementCount();
        return mcfEnt.getMCF();
    }

    synchronized public void releaseMCF(JMSAddress jmsAddress,
                                        JMSOperation jmsOperation) {
        JMSOptions options = jmsOperation.getOptions();
        if (options == null) {
            options = new JMSOptions();
        }

        if (options.getOptions() == null) {
            Map optionsMap = new HashMap();
            options.setOptions(optionsMap);                
        }

        Map optionsMap = options.getOptions();
        
        handleMappingToOptions (optionsMap, jmsAddress, jmsOperation);
                
        String optsStr = getOptionsAsString(options, jmsAddress, jmsOperation);
        String mcfKey = getKey(jmsAddress, jmsOperation, optsStr);

        MCFEntry mcfEnt =  matchMCF(mcfKey);
        if (mcfEnt == null) {
            mLogger.log(Level.WARNING, 
                        "JMSBC-W0102.NoMatchingManagedConnectionFactory",
                        new Object[]{jmsAddress.getConnectionURL(),
                                     optsStr.replaceAll("\n", ";")});
            AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0102.NoMatchingManagedConnectionFactory",
                        new Object[]{jmsAddress.getConnectionURL(),
                                     optsStr.replaceAll("\n", ";")}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0102"); 
            
        } else {
            mcfEnt.decrementCount();
            if (mcfEnt.getCount() == 0) {
                // release RA
                RAManager.getInstance().releaseRA(jmsAddress, options, jmsOperation);
                
                // remove matching mcf from map
                mcfMap.remove(mcfKey);
            }
        }
    }
    
    private MCFEntry matchOrCreateMCF(JMSAddress jmsAddress,
                                      JMSOperation jmsOperation,
                                      JMSOptions options) {
                
        String optsStr = getOptionsAsString(options, jmsAddress, jmsOperation);
        String mcfKey = getKey(jmsAddress, jmsOperation, optsStr);
        MCFEntry mcfEnt = matchMCF(mcfKey);
        if (mcfEnt == null) {
            
            //
            // All JMCJCA.xxx options will be matched at the RA level
            //
            ResourceAdapter ra = RAManager.getInstance()
                                          .acquireRA(jmsAddress, options, jmsOperation);

            //
            // Create the JMSJCA MCF instance
            // Support both JMS 1.0.1 and 1.1
            //
            XManagedConnectionFactory mcf = null;
            if (jmsOperation.getDestinationType().equals(JMSConstants.TOPIC)) {
                mcf = new XMCFTopicXA();
            } else {
                mcf = new XMCFQueueXA();
            }

            //
            // Associate RA to MCF
            //
            mcf.setResourceAdapter(ra);

            //
            // Default to using producer pooling for better performance
            //
            mcf.setProducerPooling("true");
            
            // 
            // Set additional MCF properties
            //
            if (options != null) {
                Map optsMap = (Map)options.getOptions();
                if (optsMap != null) {
                    Iterator optsIter = optsMap.keySet().iterator();
                    while(optsIter.hasNext()) {
                        JMSOption jmsOption = (JMSOption)optsMap.get(optsIter.next());
                        String optName = jmsOption.getName();
                        String optValue = jmsOption.getValue();
                        // These properties are actually MCF bean properties but
                        // it is carried as part of the jms:options in the jms extensibility element
                        if (optName.equals(Constants.JMSJCA_MCF_OPTION_ProducerPooling)) {
                            mcf.setProducerPooling(optValue);
                        } else if (optName.equals(Constants.JMSJCA_MCF_OPTION_IdleTimeout)) {
                            mcf.setIdleTimeout(optValue);
                        }
                    }
                }
            }
            
            mcfEnt = new MCFEntry(mcf);
            mcfMap.put(mcfKey, mcfEnt);            
        }
        return mcfEnt;
    }
    
    private MCFEntry matchMCF (String mcfKey) {
        MCFEntry mcfEnt = null;
        if (mcfMap.size() > 0) {
            if (mcfMap.keySet().contains(mcfKey)) {
                mcfEnt = (MCFEntry)mcfMap.get(mcfKey);
            }
        }
        return mcfEnt;    
    }
    
    private String getKey (JMSAddress jmsAddress, 
                           JMSOperation jmsOperation,
                           String optionsStr) {
        return jmsAddress.getConnectionURL() + 
               jmsAddress.getUsername() + 
               jmsAddress.getPassword() +
               jmsOperation.getDestinationType() +
               optionsStr;                       
    }
    
    
    private class MCFEntry {
        private int count = 0;
        private XManagedConnectionFactory mcf = null;
        
        public MCFEntry (XManagedConnectionFactory mcf) {
            this.mcf = mcf;
        }
        
        public XManagedConnectionFactory getMCF() { return mcf; }
        public int getCount() { return count; }
        public void incrementCount() {count++; };
        public void decrementCount() {count--; };
    }
    
    public static boolean isMCFOption (String optName) {
//        boolean retVal = false;
//        if (optName.equals(Constants.JMSJCA_MCF_OPTION_ProducerPooling) ||
//            optName.equals(Constants.JMSJCA_MCF_OPTION_IdleTimeout)) {
//            retVal = true;
//        }
//        return retVal;
    	return true;
    }
    
    private String getOptionsAsString(JMSOptions jmsOpts, JMSAddress jmsAddress, JMSOperation jmsOper) {
        StringBuffer optionsStr = new StringBuffer();
        // Set any other user defined options
        if (jmsOpts != null) {
            Map optsMap = (Map)jmsOpts.getOptions();
            if (optsMap != null) {
                TreeMap sortedOptsMap = new TreeMap(optsMap);
                Iterator optsIter = sortedOptsMap.keySet().iterator();
                while(optsIter.hasNext()) {
                    JMSOption jmsOption = (JMSOption)sortedOptsMap.get(optsIter.next());
                    String optName = jmsOption.getName();
                    String optValue = jmsOption.getValue();
                    // Get union of mcf and ra options
                    if (optValue!= null && isMCFOption(optName)) {
                        // append the option to the ra options list
                        optionsStr.append(optName);
                        optionsStr.append("=");
                        optionsStr.append(optValue);
                        optionsStr.append('\n');
                    }
                }
            }
        }
        String jmsjcaOptions = null;
		if (jmsAddress.getJmsjcaOptions() != null
				&& jmsAddress.getJmsjcaOptions().getOptions() != null) {
			jmsjcaOptions = jmsAddress.getJmsjcaOptions().getOptions();
		}
		return Util.mergeOptions(optionsStr.toString(), jmsjcaOptions, jmsOper
				.getDestinationType().equals(JMSConstants.TOPIC));
    }        
    
    private void handleMappingToOptions (Map optionsMap,
                                         JMSAddress jmsAddr,
                                         JMSOperation jmsOper) {
        // Defaults for connection pool behavior: 
        // wait if no more connections are available in the connection pool
        if (!optionsMap.containsKey(Constants.JMSJCA_RA_OPTION_pooltimeout)) {
            JMSOption jmsOpt = new JMSOption();
            jmsOpt.setName(Constants.JMSJCA_RA_OPTION_pooltimeout);
            jmsOpt.setValue(Long.toString(DEFAULT_POOLTIMEOUT));
            optionsMap.put(Constants.JMSJCA_RA_OPTION_pooltimeout, jmsOpt);
        }
        
        // if nonXA, set ACC to true in the options, otherwise will fail
        // for some provider (e.g., wepshere mq).
        if (!jmsOper.getTransaction().equals(JMSConstants.TRANSACTION_XA)) {
            if (!optionsMap.containsKey(Constants.JMSJCA_RA_OPTION_ACC)) {
                JMSOption jmsOpt = new JMSOption();
                jmsOpt.setName(Constants.JMSJCA_RA_OPTION_ACC);
                jmsOpt.setValue("true");
                optionsMap.put(Constants.JMSJCA_RA_OPTION_ACC, jmsOpt);
            }
            if (!optionsMap.containsKey(Constants.JMSJCA_RA_OPTION_NoXA)) {
                JMSOption jmsOpt = new JMSOption();
                jmsOpt.setName(Constants.JMSJCA_RA_OPTION_NoXA);
                jmsOpt.setValue("true");
                optionsMap.put(Constants.JMSJCA_RA_OPTION_NoXA, jmsOpt);
            }
        }

        // handle jndi related info
        if (jmsAddr.getConnectionURL().startsWith(
				Constants.SCHEME_JMS_GENERIC_JNDI)
				&& (jmsAddr.getConnectionFactoryName() != null && jmsAddr
						.getConnectionFactoryName().length() > 0)) {
            JMSOption jndiCFNameOpt = new JMSOption();
            String optName = Constants.JMSJCA_RA_OPTION_TopicCF;
            if (jmsOper.getDestinationType().equals(JMSConstants.QUEUE)) {
                optName = Constants.JMSJCA_RA_OPTION_QueueCF;
            } 
        
            jndiCFNameOpt.setName(optName);
            jndiCFNameOpt.setValue(jmsAddr.getConnectionFactoryName());
            optionsMap.put(optName, jndiCFNameOpt);
        }        
        
        //If clientId is set then disable connection pooling
        if (jmsOper.getClientID()!= null && !jmsOper.getClientID().equals("")) {
            JMSOption jndiCFNameOpt = new JMSOption();
            jndiCFNameOpt.setName(JMSJCA_BYPASS_RA);
            jndiCFNameOpt.setValue("true");
            optionsMap.put(JMSJCA_BYPASS_RA, jndiCFNameOpt);
            mLogger.log(Level.WARNING, 
                    "JMSBC-W0103.ConnectionPoolingDisabled",
                    new Object[]{jmsOper.getClientID()});
        }  
    }
}
