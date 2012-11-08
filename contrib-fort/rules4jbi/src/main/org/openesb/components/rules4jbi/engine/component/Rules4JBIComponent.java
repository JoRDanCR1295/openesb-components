/*
 * @(#)Rules4JBIComponent.java        $Revision: 1.2 $ $Date: 2008/07/14 16:30:26 $
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
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Stage;

import net.jcip.annotations.NotThreadSafe;

import org.openesb.components.rules4jbi.shared.logging.Logger;

import org.openesb.components.rules4jbi.engine.guice.annotations.Main;
import org.openesb.components.rules4jbi.engine.guice.annotations.MessageExchangeProcessor;
import org.openesb.components.rules4jbi.engine.guice.modules.ConstantModule;
import org.openesb.components.rules4jbi.engine.guice.modules.ExecutorModule;
import org.openesb.components.rules4jbi.engine.guice.modules.JBIModule;
import org.openesb.components.rules4jbi.engine.guice.modules.LoggerModule;
import org.openesb.components.rules4jbi.engine.util.DOMUtils;

import static org.openesb.components.rules4jbi.engine.util.ConcurrencyUtils.shutdownExecutorService;

/**
 * This class is the entry point of the Rules service engine to the JBI runtime.
 * It implements both the <code>javax.jbi.component.Component</code> and
 * <code>javax.jbi.component.ComponentLifeCycle</code> interfaces.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/14 16:30:26 $
 * 
 * @see javax.jbi.component.Component
 * @see javax.jbi.component.ComponentLifeCycle
 * @since 0.1
 */
@NotThreadSafe
public class Rules4JBIComponent implements Component, ComponentLifeCycle {
    
    /** Time to wait for tasks to complete after component was stopped; in seconds. */
    private static final long TASK_COMPLETION_WAIT_TIME = 2;
    
    @Inject @Main
    private Logger logger;
    
    @Inject
    private Injector injector;

    @Inject
    private Rules4JBIServiceUnitManager manager;
    
    @Inject
    private DeliveryChannelService deliveryChannelService;
    
    @Inject
    private ComponentContext componentContext;
    
    @Inject @Main
    private ExecutorService mainExecutorService;

    /**
     * This completition service uses mainExecutorService as its internal executor.
     * 
     * @see #mainExecutorService
     */
    @Inject @MessageExchangeProcessor
    private CompletionService<InOut> messageExchangeProcessor;
    
    private ExecutorService dispatcher;

    private ExecutorService collector;

    
    /* ComponentLifeCycle Methods */
    
    public ObjectName getExtensionMBeanName() {
        logger.entering(this.getClass(), "getExtensionMBeanName");
        
        return null;
    }
    
    public void init(ComponentContext componentContext) throws JBIException {
        if (componentContext == null) {
            throw new JBIException("Null component context received during bootstrap");
        }

        /*
         * The JBI spec doesn't guarantee that the same instance will be provided
         * during the lifecycle of the component. Therefore, we need to re-inject
         * the dependencie upon every init() method call.
         */
//      if (this.componentContext != null && this.componentContext != componentContext) {
//          throw new JBIException("Different instance of component context during lifecycle is not supported");
//      }

        boolean configurationValuesLoaded = true;
        InstallationConfiguration configuration = null;
        
        try { 
            configuration = InstallationConfiguration.load(
                    new File(componentContext.getWorkspaceRoot(), InstallationConfiguration.CONFIG_FILE_NAME));
            
        } catch (InvalidInstallationConfigurationException e) {
            
            configurationValuesLoaded = false;
            configuration = new InstallationConfiguration();
        }
        
        injector = Guice.createInjector(Stage.PRODUCTION,
                new JBIModule(componentContext),
                new LoggerModule(componentContext),
                new ConstantModule(configuration.getMaxServiceUnits()),
                new ExecutorModule(configuration.getPoolSize()));
        
        injector.injectMembers(this);
        manager.injectDependenciesIntoServiceUnits();
        
        if (logger == null) {
            throw new JBIException("Unable to properly inject depencencies");
        }

        logger.entering(this.getClass(), "init");
        
        logger.info("Initializing component '%s'", componentContext.getComponentName());

        if (!configurationValuesLoaded) {
            logger.warning("Failed to load configuration file; using default values");
        }
        
        logger.config("Thread pool size: %d", configuration.getPoolSize());
        logger.config("Maximum allowed service units: %d", configuration.getMaxServiceUnits());
        
        deliveryChannelService.init();
        
        dispatcher = Executors.newSingleThreadExecutor();
        
        collector = Executors.newSingleThreadExecutor();
        
        logger.exiting(this.getClass(), "init");
    }

    public void start() throws JBIException {
        logger.entering(this.getClass(), "start");
        
        deliveryChannelService.start();
        
        collector.execute(new CollectorTask());
        dispatcher.execute(new DispatcherTask());
        
        /*
         * JBI runtime will start the service units automatically, if appropriate
         */
    }

    public void stop() throws JBIException {
        logger.entering(this.getClass(), "stop");
        
        deliveryChannelService.stop();
        
        /*
         * JBI runtime will stop the service units automatically, if appropriate
         */
    }
    
    public void shutDown() throws JBIException {
        logger.entering(this.getClass(), "shutDown");
        
        shutdownExecutorService(logger, "Dispatcher", dispatcher);
        
        shutdownExecutorService(logger, "Collector", collector);
        
        shutdownExecutorService(logger, "Main Executor", mainExecutorService);
        
        deliveryChannelService.shutDown();
    }

    
    /* Component Methods */
    
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }
    
    public Document getServiceDescription(ServiceEndpoint endpoint) {
        logger.entering(this.getClass(), "getServiceDescription", endpoint);

        ServiceUnit serviceUnit = manager.findByServiceEndpoint(endpoint);
        
        return serviceUnit != null ? serviceUnit.getServiceDescription() : null;
    }

    public ServiceUnitManager getServiceUnitManager() {
        logger.entering(this.getClass(), "getServiceUnitManager");

        return manager;
    }

    public boolean isExchangeWithConsumerOkay(ServiceEndpoint endpoint, MessageExchange exchange) {
        logger.entering(this.getClass(), "isExchangeWithConsumerOkay");
        
        /*
         * TODO: We should check whether we want/are able to handle this exchange
         */  
        
        return true;
    }

    public boolean isExchangeWithProviderOkay(ServiceEndpoint endpoint, MessageExchange exchange) {
        logger.entering(this.getClass(), "isExchangeWithProviderOkay");
        
        /*
         * TODO: We should probably return false here. We do not initiate message exchanges,
         * so this method should never be called on us.
         */  
        
        return true;
    }
    
    public ServiceEndpoint resolveEndpointReference(DocumentFragment epr) {
        logger.entering(this.getClass(), "resolveEndpointReference", DOMUtils.documentFragmentToString(epr));

        /* We do not support dynamic endpoints */
        return null;
    }

    
    /* Tasks */
    
    /**
     * Task responsible for retrieving received <code>MessageExchange</code>s
     * and dispatching them to the appropriate service unit for processing.
     */
    private class DispatcherTask implements Runnable {

        public void run() {
            logger.fine("Starting message dispatcher");
            
            while (true) {
                try {
                    MessageExchange messageExchange = deliveryChannelService.receive();

                    if (messageExchange instanceof EmptyMessageExchange) {
                        logger.fine("Message dispatcher received terminal message");

                        messageExchangeProcessor.submit(new Callable<InOut>() {

                            public InOut call() throws Exception {

                                /* give the other tasks some time to complete */
                                TimeUnit.SECONDS.sleep(TASK_COMPLETION_WAIT_TIME);

                                return new EmptyMessageExchange();
                            }
                        });

                        break;

                    } else if (messageExchange instanceof InOut) {
                        InOut inOut = (InOut) messageExchange;

                        if (ExchangeStatus.ACTIVE.equals(inOut.getStatus())) {

                            logger.fine("Searching for service unit for the endpoint: %s", inOut.getEndpoint());
                            ServiceUnit serviceUnit = manager.findByServiceEndpoint(inOut.getEndpoint());

                            if (serviceUnit != null) {
                                serviceUnit.process(inOut);
                                
                            } else {
                                logger.warning("Could not find providing service unit");
                            }
                            
                        } else if (ExchangeStatus.DONE.equals(inOut.getStatus())) {
                            logger.fine("Received message with DONE status");
                            
                        } else if (ExchangeStatus.ERROR.equals(inOut.getStatus())) {
                            logger.warning("Received message with ERROR status", inOut.getError());
                        }
                        
                    } else {
                        logger.severe("Received incorrect MEP");
                    }

                } catch (InterruptedException e) {
                    logger.fine("Message dispatcher interrupted");
                    
                    break;
                }
            }
            
            logger.fine("Message dispatcher terminated");
        }
    }

    /**
     * Task responsible for retrieving completed <code>MessageExchange</code>s
     * and sending them back to the caller via <code>DeliveryChannelService</code>.
     */
    private class CollectorTask implements Runnable {

        public void run() {
            logger.fine("Starting results collector");

            while (true) {
                try {
                    Future<InOut> processedMessageExchange = messageExchangeProcessor.take();

                    InOut processedMessage = processedMessageExchange.get();

                    deliveryChannelService.send(processedMessage);
                    
                    if (processedMessage instanceof EmptyMessageExchange) {
                        logger.fine("Results collector received terminal message");
                        
                        break;
                    }

                } catch (InterruptedException e) {
                    logger.fine("Results collector interrupted");
                    
                    break;

                } catch (ExecutionException e) {
                    logger.warning("Error while executing task", e);
                }
            }
            
            logger.fine("Results collector terminated");
        }
    }
}
