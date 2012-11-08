package com.sun.jbi.restbc.jbiadapter.inbound;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

/**
 * ComponentBundleActivator.java
 *
 * @author Edward Chou
 */
public class ComponentBundleActivator implements BundleActivator {

    private static BundleContext bundleContext;
    
    /* (non-Javadoc)
     * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
     */
    public void start(BundleContext context) throws Exception {
         bundleContext = context;
    }

    /* (non-Javadoc)
     * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    public void stop(BundleContext context) throws Exception {
        
    }

    public static BundleContext getBundleContext() {
        return bundleContext;
    }
}
