/*
 * @(#)DroolsRulesEngineProvider.java        $Revision: 1.1 $ $Date: 2008/12/17 23:17:41 $
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

package org.openesb.components.rules4jbi.netbeans.drools;

import org.openesb.components.rules4jbi.netbeans.spi.RulesEngineProvider;

/**
 * Provides JSR 94 configuration values for Drools.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:17:41 $
 * 
 * @since 0.1
 */
public class DroolsRulesEngineProvider implements RulesEngineProvider {

    private static final String RULE_SERVICE_PROVIDER_URI = "http://drools.org/";
    
    private static final String RULE_SERVICE_PROVIDER_CLASS_NAME = "org.drools.jsr94.rules.RuleServiceProviderImpl";
    
    public String getRuleServiceProviderURI() {
        return RULE_SERVICE_PROVIDER_URI;
    }
    
    public String getRuleServiceProviderClassName() {
        return RULE_SERVICE_PROVIDER_CLASS_NAME;
    }
}
