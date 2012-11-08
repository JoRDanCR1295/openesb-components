/*
 * @(#)JamochaRulesEngineProvider.java        $Revision: 1.2 $ $Date: 2009/01/27 21:42:43 $
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

package org.openesb.components.rules4jbi.netbeans.jamocha;

import org.openesb.components.rules4jbi.netbeans.spi.RulesEngineProvider;

/**
 * Provides JSR 94 configuration values for Jamocha.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2009/01/27 21:42:43 $
 * 
 * @since 0.1
 */
public final class JamochaRulesEngineProvider implements RulesEngineProvider {

    private static final String RULE_SERVICE_PROVIDER_URI = "http://www.jamocha.org";
    
    private static final String RULE_SERVICE_PROVIDER_CLASS_NAME = "org.jamocha.communication.jsr94.JamochaRuleServiceProvider";
    
    private static final JamochaRulesEngineProvider INSTANCE = new JamochaRulesEngineProvider();
    
    private JamochaRulesEngineProvider() {}
    
    public static RulesEngineProvider getInstance() {
        return INSTANCE;
    }
    
    public String getRuleServiceProviderURI() {
        return RULE_SERVICE_PROVIDER_URI;
    }
    
    public String getRuleServiceProviderClassName() {
        return RULE_SERVICE_PROVIDER_CLASS_NAME;
    }
}
