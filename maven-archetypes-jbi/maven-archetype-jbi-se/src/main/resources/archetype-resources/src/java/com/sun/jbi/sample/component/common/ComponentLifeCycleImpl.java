/*
 * ComponentLifeCycleImpl.java
 */
package com.sun.jbi.sample.component.common;

import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.management.ObjectName;

/**
 * This class implements the javax.jbi.component.ComponentLifeCycle. This class
 * implements the minimum required functionality required for the component lifecycle 
 * contract of the jbi component. It also initializes and maintains the component
 * context referece and creates the component runtime logger.
 *
 * It makes sure that the mininum initialization required for the component
 * lifecycle is implemenated by this class or the classes extending this class.
 *
 * Service engine or a binding component lifecycle implementation can directly use
 * this class or extend this class to add more functionality to their lifecycle
 * operations.
 *
 * @see javax.jbi.ComponentLifeCycle
 * @author chikkala
 */
public class ComponentLifeCycleImpl implements ComponentLifeCycle {
    /** default logger*/
    private Logger mDefLogger;
    /** component logger */
    private Logger mCompLogger ;
    /** component context */
    private ComponentContext mComponentContext;
    
    /** default constructor  */
    public ComponentLifeCycleImpl() {}
    /**
     * @return ComponentContext component context.
     */
    public final ComponentContext getComponentContext() {
        return mComponentContext;
    }
    /**
     * Returns logger initialized from the component context or a default logger.
     * @return Logger
     */
    public final Logger getLogger() {
        // try init component logger
        if (this.mCompLogger == null && this.mComponentContext != null ) {
            try {
                this.mCompLogger =
                    this.mComponentContext.getLogger(this.getClass().getName(), null);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
        // init default logger if required
        if ( this.mCompLogger == null && this.mDefLogger == null) {
            this.mDefLogger = Logger.getLogger(this.getClass().getName(), null);
        }
        return (this.mCompLogger != null) ? this.mCompLogger : this.mDefLogger;
    }
    /**
     * Initialize the component. By making this class final makes sure that the
     * required initialization is done in this method and any extended
     * initialization is done by onInit method of this class or the extended
     * class that overrides the onInit method.
     * @param context Component context
     * @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext)
     * @throws javax.jbi.JBIException on error
     */
    public final void init(ComponentContext context) throws JBIException {
        initContext(context); // 1. initialize component context
        doInit();             // 2. do any other component specific initialization.
    }
    /**
     * initializes the implementation using ComponentContext passed by init method.
     * @param context
     */
    private void initContext(ComponentContext context) throws JBIException {
        this.mComponentContext = context;
        getLogger().info(this.mComponentContext.getComponentName() +
            " Component Context initialized");
    }
    /**
     * Chance to extended classes to do the component specific init.
     * Component context will be initialized before calling this method. So,
     * initialize other things if required in this method.
     * @throws javax.jbi.JBIException on error
     * @see com.sun.jbi.sample.component.common.ComponentLifeCycleImpl#init(
     * javax.jbi.component.ComponentContext)
     */
    protected void doInit() throws JBIException {
        this.getLogger().info(this.getComponentContext().getComponentName() + "  Initialized");
    }
    /**
     * Start the component.
     * @see javax.jbi.component.ComponentLifeCycle#start()
     * @throws javax.jbi.JBIException on error.
     */
    public void start() throws JBIException {
        this.getLogger().info(this.getComponentContext().getComponentName() + "  Started.");
    }
    /**
     * Stop the component.
     * @see javax.jbi.component.ComponentLifeCycle#stop()
     * @throws javax.jbi.JBIException on error.
     */
    public void stop() throws JBIException {
        this.getLogger().info(this.getComponentContext().getComponentName() + "  Stoped.");
    }
    /**
     * Shut down the component.
     * @see javax.jbi.component.ComponentLifeCycle#shutDown()
     * @throws javax.jbi.JBIException on error.
     */
    public void shutDown() throws JBIException {
        System.out.println( this.getComponentContext().getComponentName() + " Shutdown.");
    }
    /**
     * this is a default implementation which does not have any extension mbeans. extended
     * classes can return the mbean name for the extension mbeans if the component supports.
     * @return null to indicate no extension mbean.
     * @see javax.jbi.component.ComponentLifeCycle#getExtensionMBeanName()
     */
    public ObjectName getExtensionMBeanName() {
        return null;
    }
}
