/*
 * @(#)LoggerModule.java        $Revision: 1.3 $ $Date: 2008/07/14 16:30:28 $
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

package org.openesb.components.rules4jbi.engine.guice.modules;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.google.inject.AbstractModule;
import com.google.inject.name.Names;

import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.logging.LoggerImpl;

import org.openesb.components.rules4jbi.engine.guice.annotations.ExecutionTask;
import org.openesb.components.rules4jbi.engine.guice.annotations.Main;
import org.openesb.components.rules4jbi.engine.guice.annotations.Manager;
import org.openesb.components.rules4jbi.engine.guice.annotations.Unit;

/**
 * Guice module for configuring loggers.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/07/14 16:30:28 $
 * 
 * @since 0.1
 */
public class LoggerModule extends AbstractModule {
    
    private static final String COMPONENT_LOGGER_NAME = "component";
    
    private static final String SERVICE_UNIT_MANAGER_LOGGER_NAME = "manager";
    
    private static final String SERVICE_UNIT_LOGGER_NAME = "service-unit";

    private static final String DELIVERY_CHANNEL_SERVICE_LOGGER_NAME = "delivery-channel";
    
    private static final String SERVICE_UNIT_FACTORY_LOGGER_NAME = "ServiceUnitFactory";

    private static final String RULE_EXECUTION_TASK_LOGGER_NAME = "execution-task";
    
    private static final String RULE_EXECUTION_TASK_FACTORY_LOGGER_NAME = "RuleExecutionTaskFactory";
    
    private static final String BOOTSTRAP_LOGGER_NAME = "Bootstrap";
    
    private static final String INSTALLATION_CONFIGURATION_LOGGER_NAME = "InstallationConfiguration";
    
    private static final String NO_NAME_LOGGER_NAME = "undefined-name";
    
    private final ComponentContext componentContext;

    public LoggerModule(ComponentContext componentContext) {
        this.componentContext = componentContext;
    }    

    @Override
    protected void configure() {
        try {
//            bind(Logger.class)
//                .toInstance(new LoggerImpl(
//                        Logger.NORMAL_PREFIX, componentContext.getLogger(NO_NAME_LOGGER_NAME, null)));
            
            //TODO: replace string names with binding annotations
            
            bind(Logger.class)
                .annotatedWith(Main.class)
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX, componentContext.getLogger(COMPONENT_LOGGER_NAME, null)));
            
            bind(Logger.class)
                .annotatedWith(Manager.class)
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX, componentContext.getLogger(SERVICE_UNIT_MANAGER_LOGGER_NAME, null)));
            
            bind(Logger.class)
                .annotatedWith(Unit.class)
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX, componentContext.getLogger(SERVICE_UNIT_LOGGER_NAME, null)));
            
            bind(Logger.class)
                .annotatedWith(ExecutionTask.class)
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX, componentContext.getLogger(RULE_EXECUTION_TASK_LOGGER_NAME, null)));
            
            bind(Logger.class)
                .annotatedWith(Names.named("DeliveryChannelService"))
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX,
                        componentContext.getLogger(DELIVERY_CHANNEL_SERVICE_LOGGER_NAME, null)));
            
            bind(Logger.class)
                .annotatedWith(Names.named("ServiceUnitFactory"))
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX,
                        componentContext.getLogger(SERVICE_UNIT_FACTORY_LOGGER_NAME, null)));

            bind(Logger.class)
                .annotatedWith(Names.named("RuleExecutionTaskFactory"))
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX,
                        componentContext.getLogger(RULE_EXECUTION_TASK_FACTORY_LOGGER_NAME, null)));
            
            bind(Logger.class)
                .annotatedWith(Names.named("Bootstrap"))
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX,
                        componentContext.getLogger(BOOTSTRAP_LOGGER_NAME, null)));
            
            bind(Logger.class)
                .annotatedWith(Names.named("InstallationConfiguration"))
                .toInstance(new LoggerImpl(
                        Logger.NORMAL_PREFIX,
                        componentContext.getLogger(INSTALLATION_CONFIGURATION_LOGGER_NAME, null)));
            
        } catch (JBIException e) {

            // we don't use resource bundles, so it shouldn't happen.
            throw new AssertionError(e);
        }
    }
}
