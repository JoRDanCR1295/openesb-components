/*
 * @(#)ExecutorModule.java        $Revision: 1.2 $ $Date: 2008/07/14 16:30:28 $
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

import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.jbi.messaging.InOut;

import com.google.inject.AbstractModule;
import com.google.inject.TypeLiteral;

import org.openesb.components.rules4jbi.engine.guice.annotations.Main;
import org.openesb.components.rules4jbi.engine.guice.annotations.MessageExchangeProcessor;

/**
 * Guice module for configuring executors.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/14 16:30:28 $
 * 
 * @since 0.1
 */
public class ExecutorModule extends AbstractModule {
    
    private final ExecutorService mainExecutorService;
    
    private final CompletionService<InOut> messageExchangeProcessor;
    
    public ExecutorModule(int poolSize) {
        mainExecutorService = Executors.newFixedThreadPool(poolSize);
        
        messageExchangeProcessor = new ExecutorCompletionService<InOut>(mainExecutorService);
    }

    @Override
    protected void configure() {
        bind(ExecutorService.class)
            .annotatedWith(Main.class)
            .toInstance(mainExecutorService);
        
        bind(new TypeLiteral<CompletionService<InOut>>() {})
            .annotatedWith(MessageExchangeProcessor.class)
            .toInstance(messageExchangeProcessor);
    }
}
