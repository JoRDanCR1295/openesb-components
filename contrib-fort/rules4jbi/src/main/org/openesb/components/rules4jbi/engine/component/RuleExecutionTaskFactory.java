/*
 * @(#)RuleExecutionTaskFactory.java        $Revision: 1.3 $ $Date: 2008/07/25 04:54:42 $
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

package org.openesb.components.rules4jbi.engine.component;

import java.io.File;

import java.util.List;
import javax.jbi.messaging.InOut;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.name.Named;

import javax.xml.namespace.QName;
import org.openesb.components.rules4jbi.shared.logging.Logger;

/**
 * Factory that produces <code>RuleExecutionTask</code> instances properly injected with dependencies.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/07/25 04:54:42 $
 * 
 * @since 0.1
 */
public class RuleExecutionTaskFactory {
    
    @Inject @Named("RuleExecutionTaskFactory")
    private Logger logger;
    
    @Inject
    private Provider<RuleExecutionTask> ruleExecutionTaskProvider;
    
    RuleExecutionTask createNewRuleExecutionTask(String ruleServiceProvider, String ruleServiceProviderClass,
            File rulesetFile, Class<?>[] classes, ClassLoader classLoader,
            String targetNamespace, List<QName> outputElements, InOut messageExchange)
    {
        logger.fine("Creating new rule execution task for target namespace '%s'", targetNamespace);
        
        RuleExecutionTask task = ruleExecutionTaskProvider.get();
        
        task.setRuleServiceProvider(ruleServiceProvider);
        task.setRuleServiceProviderClass(ruleServiceProviderClass);
        task.setRulesetFile(rulesetFile);
        task.setClasses(classes);
        task.setClassLoader(classLoader);
        task.setTargetNamespace(targetNamespace);
        task.setOutputElements(outputElements);
        task.setMessageExchange(messageExchange);
        
        return task;
    }
}
