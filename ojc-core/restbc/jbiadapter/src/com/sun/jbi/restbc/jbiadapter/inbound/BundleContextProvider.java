package com.sun.jbi.restbc.jbiadapter.inbound;

import javax.ws.rs.core.Context;
import javax.ws.rs.ext.Provider;

import org.osgi.framework.BundleContext;

import com.sun.jersey.spi.inject.SingletonTypeInjectableProvider;

/**
 * BundleContextProvider.java
 *
 * @author Edward Chou
 */
@Provider
public class BundleContextProvider extends SingletonTypeInjectableProvider<Context, BundleContext> { 
    
    public BundleContextProvider() {
        super(BundleContext.class, ComponentBundleActivator.getBundleContext());
    }
    
    
} 