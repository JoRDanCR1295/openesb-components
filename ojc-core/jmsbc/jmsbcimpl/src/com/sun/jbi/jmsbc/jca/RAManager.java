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
 * @(#)RAManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jca;

import java.util.Collections;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.resource.spi.ResourceAdapter;

import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSOptions;
import com.sun.jbi.jmsbc.extensions.JMSOption;
import com.sun.jbi.jmsbc.extensions.JMSJNDIEnvEntry;
import com.sun.jbi.jmsbc.util.AlertsUtil;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;

import com.stc.jmsjca.core.RAJMSResourceAdapter;
import com.stc.jmsjca.unifiedjms.RAUnifiedResourceAdapter;

/**
 *
 * Abstracts much of the JCA RA creation from the BC.
 */
public class RAManager {
    private Map raMap = Collections.synchronizedMap(new HashMap());
    private static RAManager singleton = null;

    private static final Messages mMessages =
        Messages.getMessages(RAManager.class);
    private static final Logger mLogger =
        Messages.getLogger(RAManager.class);    
    
    private RAManager() {}
    
    public static RAManager getInstance() {
        if (singleton == null) {
            singleton = new RAManager();
        }
        return singleton;
    }    
    
    synchronized public ResourceAdapter acquireRA (JMSAddress jmsAddress,
                                                   JMSOptions raOptions,
                                                   JMSOperation jmsOperation) {
        if (raOptions.getOptions() == null) {
            Map optionsMap = new HashMap();
            raOptions.setOptions(optionsMap);                
        }

        Map optionsMap = raOptions.getOptions();
                
        handleMappingJNDIToOptions (optionsMap, jmsAddress);
        
        RAEntry raEnt = matchOrCreateRA(jmsAddress, raOptions, jmsOperation);
        RAJMSResourceAdapter ra = raEnt.getRA();
        raEnt.incrementCount();
        return ra;
    }
    
    synchronized public void releaseRA(JMSAddress jmsAddress,
                                       JMSOptions raOptions, JMSOperation jmsOperation) {
        if (raOptions.getOptions() == null) {
            Map optionsMap = new HashMap();
            raOptions.setOptions(optionsMap);                
        }

        Map optionsMap = raOptions.getOptions();
                
        handleMappingJNDIToOptions (optionsMap, jmsAddress);
        
        String raOptsStr = getRAOptionsAsString(jmsAddress, raOptions, jmsOperation);
        String raKey = getKey(jmsAddress, raOptsStr);
        RAEntry raEnt =  matchRA(raKey);
        if (raEnt == null) {
            mLogger.log(Level.WARNING, 
                        "JMSBC-W0101.NoMatchingResourceAdapter",
                        new Object[]{jmsAddress.getConnectionURL(),
                                     raOptsStr.replaceAll("\n", ";")});
            AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0101.NoMatchingResourceAdapter",
                        new Object[]{jmsAddress.getConnectionURL(),
                                     raOptsStr.replaceAll("\n", ";")}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0101"); 
            
        } else {
            raEnt.decrementCount();
            if (raEnt.getCount() == 0) {
                RAJMSResourceAdapter ra = raEnt.getRA();
                ra.stop();
                raMap.remove(raKey);
            }
        }
    }
    
    public static boolean isRAOption (String optionName) {
//        // Treat everything else as RA option
//        return !optionName.equals(Constants.JMSJCA_MCF_OPTION_IdleTimeout) &&
//               !optionName.equals(Constants.JMSJCA_MCF_OPTION_ProducerPooling);
    	return true;
    } 
    
    private class RAEntry {
        private int count = 0;
        private RAJMSResourceAdapter ra = null;
        
        public RAEntry (RAJMSResourceAdapter ra) {
            this.ra = ra;
        }
        
        public RAJMSResourceAdapter getRA() { return ra; }
        public int getCount() { return count; }
        public void incrementCount() {count++; };
        public void decrementCount() {count--; };
    }
    
    private RAEntry matchOrCreateRA(JMSAddress jmsAddress, JMSOptions raOptions, JMSOperation jmsOperation) {        
        String raOptsStr = getRAOptionsAsString(jmsAddress, raOptions, jmsOperation);
        String raKey = getKey(jmsAddress, raOptsStr);
        RAEntry raEnt = matchRA(raKey);
        if (raEnt == null) {
            RAJMSResourceAdapter ra = new RAUnifiedResourceAdapter();
            ra.setConnectionURL(jmsAddress.getConnectionURL());
            String userName = jmsAddress.getUsername();
            String passWord = jmsAddress.getPassword();
            if (!Util.isEmpty(userName)) {
                ra.setUserName(userName);
                if (!Util.isEmpty(passWord)) {
                    ra.setPassword(passWord);
                }
            }
            if (raOptsStr != null) {
                ra.setOptions(raOptsStr);
            }
            raEnt = new RAEntry(ra);
            raMap.put(raKey, raEnt);
        }
        return raEnt;
    }
    
    private RAEntry matchRA (String raKey) {
        RAEntry raEnt = null;
        if (raMap.size() > 0) {
            if (raMap.keySet().contains(raKey)) {
                raEnt = (RAEntry)raMap.get(raKey);
            }
        }
        return raEnt;    
    }
    
    private String getKey (JMSAddress jmsAddress, String optionsStr) {
        return jmsAddress.getConnectionURL() + 
               jmsAddress.getUsername() + 
               jmsAddress.getPassword() +
               optionsStr;                       
    }
    
    private String getRAOptionsAsString(JMSAddress jmsAddress, JMSOptions jmsOpts, JMSOperation jmsOperation) {
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
                    // Get only the RA level options
                    if (optValue!= null && isRAOption(optName)) {
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
		return Util.mergeOptions(optionsStr.toString(), jmsjcaOptions, jmsOperation
				.getDestinationType().equals(JMSConstants.TOPIC));
    }    
    
    private void handleMappingJNDIToOptions (Map optionsMap,
                                             JMSAddress jmsAddr) {        
        // convert jndienventry(ies) to options
        if (jmsAddr.getConnectionURL().startsWith(Constants.SCHEME_JMS_GENERIC_JNDI)) {            
            if (jmsAddr.getInitialContextFactory() != null) {
                JMSOption opt = new JMSOption();
                opt.setName(Constants.JAVA_NAMING_FACTORY_INITIAL);
                opt.setValue(jmsAddr.getInitialContextFactory());
                optionsMap.put(opt.getName(), opt);
            }
            
            if (jmsAddr.getProviderURL() != null) {
                JMSOption opt = new JMSOption();
                opt.setName(Constants.JAVA_NAMING_PROVIDER_URL);
                opt.setValue(jmsAddr.getProviderURL());
                optionsMap.put(opt.getName(), opt);                
            }
            
            if (jmsAddr.getSecurityPrincial() != null) {
                JMSOption opt = new JMSOption();
                opt.setName(Constants.JAVA_NAMING_SECURITY_PRINCIPAL);
                opt.setValue(jmsAddr.getSecurityPrincial());
                optionsMap.put(opt.getName(), opt);
            }
            
            if (jmsAddr.getSecurityCredentials() != null) {
                JMSOption opt = new JMSOption();
                opt.setName(Constants.JAVA_NAMING_SECURITY_CREDENTIALS);
                opt.setValue(jmsAddr.getSecurityCredentials());
                optionsMap.put(opt.getName(), opt);
            }            
            
            if (jmsAddr.getJndiEnv() != null) {
                Map jndiEnvEntries = jmsAddr.getJndiEnv().getJNDIEnvEntries();
                if (jndiEnvEntries != null) {
                    Iterator iter = jndiEnvEntries.keySet().iterator();
                    while (iter.hasNext()) {
                        JMSJNDIEnvEntry jndienventry = (JMSJNDIEnvEntry) jndiEnvEntries.get(iter.next());
                        JMSOption jmsOpt = new JMSOption();
                        jmsOpt.setName(jndienventry.getName());
                        jmsOpt.setValue(jndienventry.getValue());
                        optionsMap.put(jndienventry.getName(), jmsOpt);
                    }
                }
            }
        }        
    }
}
