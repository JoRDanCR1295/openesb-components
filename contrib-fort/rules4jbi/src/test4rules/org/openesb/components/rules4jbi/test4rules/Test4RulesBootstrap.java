/*
 * @(#)Test4RulesBootstrap.java        $Revision: 1.2 $ $Date: 2008/07/03 05:46:05 $
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

package org.openesb.components.rules4jbi.test4rules;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.management.ObjectName;

import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.logging.LoggerFactory;

/**
 * Bootstrap implementation of the test engine used for functional
 * testing of the rules service engine.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/03 05:46:05 $
 * 
 * @since 0.1
 */
public class Test4RulesBootstrap implements Bootstrap {

    private Logger logger = null;
    
    private InstallationContext installationContext = null;
    
    public void cleanUp() throws JBIException {
        logger.entering(this.getClass(), "cleanUp");
    }

    public ObjectName getExtensionMBeanName() {
        logger.entering(this.getClass(), "getExtensionMBeanName");
        
        return null;
    }

    public void init(InstallationContext installationContext) throws JBIException {
        if (installationContext == null) {
            throw new JBIException("Null installation context received during bootstrap");
        }
        
        this.installationContext = installationContext;
        
        LoggerFactory.getInstance().init(Logger.TEST_PREFIX, installationContext.getContext());
        
        logger = LoggerFactory.getInstance().getLogger(Test4RulesBootstrap.class);
        
        logger.exiting(this.getClass(), "init");
    }
    
    public void onInstall() throws JBIException {
        logger.entering(this.getClass(), "onInstall");
    }

    public void onUninstall() throws JBIException {
        logger.entering(this.getClass(), "onUninstall");
    }
}
