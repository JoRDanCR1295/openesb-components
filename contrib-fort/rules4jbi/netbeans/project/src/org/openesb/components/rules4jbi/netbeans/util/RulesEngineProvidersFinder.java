/*
 * @(#)RulesEngineProvidersFinder.java        $Revision: 1.1 $ $Date: 2008/12/17 23:21:34 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import org.openide.util.Lookup;

import org.openesb.components.rules4jbi.netbeans.spi.RulesEngineProvider;

/**
 * This singleton class is used to obtain configuration values of currently registered
 * rule engine providers. Those providers can be registered into the global lookup by external
 * NetBeans modules either through <code>META-INF/services</code> or through <code>layer.xml</code>.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:21:34 $
 * 
 * @see org.openesb.components.rules4jbi.netbeans.spi.RulesEngineProvider
 * @since 0.1
 */
public final class RulesEngineProvidersFinder {
    
    private static final RulesEngineProvidersFinder INSTANCE = new RulesEngineProvidersFinder();
    
    private static final Logger logger = Logger.getLogger(RulesEngineProvidersFinder.class.getName());

    private final Lookup.Result<RulesEngineProvider> lookupResult;
    
    private RulesEngineProvidersFinder() {
        lookupResult = Lookup.getDefault().lookupResult(RulesEngineProvider.class);
    }
    
    public static RulesEngineProvidersFinder getInstance() {
        return INSTANCE;
    }

    public String[] getRuleServiceProviderURIs() {
        logger.fine("Retrieving all registered rule service provider URIs");

        Collection<? extends RulesEngineProvider> currentRuleEngineProviders = findCurrentRuleEngineProviders();
        
        List<String> providerURIs = new ArrayList<String>();

        for (RulesEngineProvider ruleEngineProvider : currentRuleEngineProviders) {
            String providerURI = ruleEngineProvider.getRuleServiceProviderURI();

            if (providerURI != null) {
                logger.finer("Found provider URI: " + providerURI);
                
                providerURIs.add(providerURI);
            }
        }

        String[] result = providerURIs.toArray(new String[providerURIs.size()]);
        
        logger.fine("Found following provider URIs: " + Arrays.toString(result));
        
        return result;
    }
    
    public String[] getRuleServiceProviderClassNames() {
        logger.fine("Retrieving all registered rule service provider class names");

        Collection<? extends RulesEngineProvider> currentRuleEngineProviders = findCurrentRuleEngineProviders();
        
        List<String> providerClassNames = new ArrayList<String>();

        for (RulesEngineProvider ruleEngineProvider : currentRuleEngineProviders) {
            String providerClassName = ruleEngineProvider.getRuleServiceProviderClassName();

            if (providerClassName != null) {
                logger.finer("Found provider class name: " + providerClassName);
                
                providerClassNames.add(providerClassName);
            }
        }

        String[] result = providerClassNames.toArray(new String[providerClassNames.size()]);
        
        logger.fine("Found following provider class names: " + Arrays.toString(result));
        
        return result;
    }
    
    private Collection<? extends RulesEngineProvider> findCurrentRuleEngineProviders() {
        logger.finest("Searching for implementations of RuleEngineProvider in global lookup");
        
        Collection<? extends RulesEngineProvider> currentRuleEngineProviders = lookupResult.allInstances();
        
        logger.finest("Found " + currentRuleEngineProviders.size() + " rule engine provider(s)");
        
        return currentRuleEngineProviders;
    }
}
