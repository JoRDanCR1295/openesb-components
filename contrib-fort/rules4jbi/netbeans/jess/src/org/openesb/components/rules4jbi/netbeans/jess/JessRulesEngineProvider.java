/*
 * @(#)JessRulesEngineProvider.java        $Revision: 1.1 $ $Date: 2008/12/17 23:20:04 $
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

package org.openesb.components.rules4jbi.netbeans.jess;

import org.openesb.components.rules4jbi.netbeans.spi.RulesEngineProvider;

/**
 * Provides JSR 94 configuration values for Jess.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:20:04 $
 * 
 * @since 0.1
 */
public class JessRulesEngineProvider implements RulesEngineProvider {

    private static final String RULE_SERVICE_PROVIDER_URI = "org.jcp.jsr94.jess";
    
    private static final String RULE_SERVICE_PROVIDER_CLASS_NAME = "org.jcp.jsr94.jess.RuleServiceProviderImpl";
    
    public String getRuleServiceProviderURI() {
        return RULE_SERVICE_PROVIDER_URI;
    }
    
    public String getRuleServiceProviderClassName() {
        return RULE_SERVICE_PROVIDER_CLASS_NAME;
    }
}
