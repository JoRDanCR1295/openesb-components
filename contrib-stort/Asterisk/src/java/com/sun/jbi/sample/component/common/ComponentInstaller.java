/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: ComponentInstaller.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common;

import java.util.logging.Level;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

public class ComponentInstaller implements Bootstrap {
    /** default logger*/
    private Logger mDefLogger;
    /** bootstrap logger */
    private Logger mBTLogger ;
    /** Installation Context . */
    private InstallationContext mInstallationContext;
    /** Extension Mbean Name*/
    private ObjectName mExtensionMBeanName;
    /** Extension Mbean Implementation*/
    private StandardMBean mExtensionMBeanImpl;
    
    /** Constructor to create the ComponentInstaller. */
    public ComponentInstaller() {
    }
    /**
     * @return InstallationContext of the component.
     */
    public final InstallationContext getInstallationContext() {
        return this.mInstallationContext;
    }
    protected String getComponentName() {
        return (this.mInstallationContext != null) ?
            this.mInstallationContext.getComponentName() : null;
    }
    /**
     * @return logger initialized from the installation context or a default
     * logger.
     */
    protected Logger  getLogger() {
        // try init bootstrap logger
        if (this.mBTLogger == null && this.mInstallationContext != null ) {
            try {
                this.mBTLogger =
                    this.mInstallationContext.getContext().getLogger(this.getClass().getName(), null);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
        // init default logger if required
        if ( this.mBTLogger == null && this.mDefLogger == null) {
            this.mDefLogger = Logger.getLogger(this.getClass().getName(), null);
        }
        return (this.mBTLogger != null) ? this.mBTLogger : this.mDefLogger;
    }
    /**
     * Initializes the installation environment for a component.
     * @see javax.jbi.component.Bootstrap#init(javax.jbi.component.InstallationContext)
     */
    public final void init(InstallationContext installContext) throws javax.jbi.JBIException {
        
        if ( installContext == null  ) {
            throw new JBIException("Null Installation Context received in " +
                "Component Bootstrap initialization");
        }
        // save the installation context reference passed by the jbi framework.
        this.mInstallationContext = installContext;         
        registerExtensionMBean(); // create and register extension mbean
        doInit(); // do any other extra initialization required specific to the component installation.
        // initialize logger
        this.getLogger().fine(this.mInstallationContext.getComponentName() +
            " : Component Installer initialized");
    }
    /**
     * default implementation just logs the method call. extended classes should override this.
     * @see javax.jbi.component.Bootstrap#onInstall()
     */
    public void onInstall()  throws javax.jbi.JBIException {
        //TODO: do required initializaton and resource creation
        this.getLogger().fine(this.mInstallationContext.getComponentName() +
            " : Component installed.");
    }
    /**
     * default implementation just logs the method call.
     * @see javax.jbi.component.Bootstrap#onUninstall()
     */
    public void onUninstall() throws javax.jbi.JBIException {
        //TODO: do clean up the resource and other stuff.
        this.getLogger().fine(this.mInstallationContext.getComponentName() +
            " : Component uninstalled");
    }    
    /**
     * default noop implementation of the cleanup.
     * @see javax.jbi.component.Bootstrap#cleanUp()
     */
    public final void cleanUp() throws javax.jbi.JBIException {
        
        unregisterExtensionMBean(); // unregister extension mbean and remove it        
        doCleanUp(); // do any other extra cleanup specific to the component installation.
        this.getLogger().fine( this.mInstallationContext.getComponentName() +
            " Component Installer Cleaned up");
    }  
    
    /**
     * create jmx object name for the extension mbean. Extended classes should
     * return the component specific installer extension mbean name here.
     */
    protected ObjectName createExtensionMBeanName() {
        return null;
    }    
    /**
     * default implementation that does not have extension mbean return null.
     * @see javax.jbi.component.Bootstrap#getExtensionMBeanName()
     */
    public final javax.management.ObjectName getExtensionMBeanName() {
        if ( this.mExtensionMBeanName == null ) {
            this.mExtensionMBeanName = createExtensionMBeanName();
        }
        return this.mExtensionMBeanName;
    }
    /**
     * create mbean implementation for the extension mbean as a StandardMBean. Extended
     * classes should return the installer extension mbean implementation as the
     * standard mbean.
     */
    protected StandardMBean createExtensionMBean() {
        return null;
    }    
    /**
     * returns the installation extension mbean implementation.
     * @return StandardMBean for the extension mbean implementation.
     */
    protected final StandardMBean getExtensionMBean() {
        if ( this.mExtensionMBeanImpl == null ) {
            this.mExtensionMBeanImpl = this.createExtensionMBean();
        }
        return this.mExtensionMBeanImpl;
    }
    /**
     * Chance to extended classes to do the component installation specific init.
     * Installation context will be initialized before calling this method. So,
     * initialize other things if required in this method.
     * @throws javax.jbi.JBIException on error
     * @see com.sun.jbi.sample.component.common.ComponentInstaller#init(
     * javax.jbi.component.InstallationContext)
     */
    protected void doInit() throws JBIException {
        this.getLogger().info(this.getInstallationContext().getComponentName() +
            " : Component Installer initialized");
    }
    /**
     * Chance to extended classes to do the component installation specific cleanup.
     * ExtensionMbean will be unregistered before calling this method.
     * @throws javax.jbi.JBIException on error
     * @see com.sun.jbi.sample.component.common.ComponentInstaller#cleanUp(
     * javax.jbi.component.InstallationContext)
     */
    protected void doCleanUp() throws JBIException {
        this.getLogger().fine(this.getInstallationContext().getComponentName() +
            " : Component Installer doCleanUp");
    }
    /**
     * if there is an extension mbean supported by the component, then register
     * it with the mbean server.
     */
    protected void registerExtensionMBean() throws JBIException {
        // creatre the installation extension mbean's object name
        ObjectName mbeanName = this.getExtensionMBeanName();
        // create the extension mbean implemenation if the object name is created.
        StandardMBean mbeanImpl = this.getExtensionMBean();
        
        if ( mbeanName == null || mbeanImpl == null ) {
            this.getLogger().fine(this.getComponentName() +
                " Does not have Installation Extension MBean implemenation ");
            return;
        }
        // register with mbean only if object name and implementation are non null
        try {
            MBeanServer mbServer = this.getInstallationContext().getContext().getMBeanServer();
            mbServer.registerMBean(mbeanImpl, mbeanName);
        } catch (Exception e) {
            throw new JBIException("Failed to register Installation Extension MBean for "
                + this.getComponentName(), e);
        }
    }
    /**
     * remove the registered extension mbean from the mbean server.
     */
    protected void unregisterExtensionMBean() {
        ObjectName mbeanName = this.getExtensionMBeanName();
        if ( mbeanName == null ) {
            return;
        }
        try {
            MBeanServer mbServer = this.getInstallationContext().getContext().getMBeanServer();
            mbServer.unregisterMBean(mbeanName);
        } catch (Exception e) {
            this.getLogger().log(Level.WARNING,
                "Failed to unregister Installation extension MBean for " + this.getComponentName(), e);
        }
    }
    
}
