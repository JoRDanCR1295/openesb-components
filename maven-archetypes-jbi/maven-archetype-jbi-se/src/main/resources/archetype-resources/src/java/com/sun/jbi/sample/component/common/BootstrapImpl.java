 /*
  * BootstrapImpl.java
  */
package com.sun.jbi.sample.component.common;

import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import java.util.logging.Logger;
import javax.jbi.JBIException;

/**
 * This class implements the javax.jbi.component.Bootstrap. This class implements
 * the minimum required functionality for component installation contract
 * of the jbi component in the jbi environment. It also takes care of maintaining
 * the installation context referece and installation specific logger creation.
 *
 * It makes sure that the installation context and the loggers are available to
 * the classes extended from this class to add more installation specific funtionality.
 *
 * Service engine or a binding component installation implementation can directly use
 * this class or extend this class to add more functionality to their installation
 * process.
 *
 * @see javax.jbi.Bootstrap
 * @author chikkala
 */
public class BootstrapImpl implements Bootstrap {
    /** default logger*/
    private Logger mDefLogger;
    /** bootstrap logger */
    private Logger mBTLogger ;
    /** Installation Context . */
    private InstallationContext mInstallationContext;
    
    /** Constructor to creatre the ComponentInstaller. */
    public BootstrapImpl() {
    }
    /**
     * @return InstallationContext of the component.
     */
    public final InstallationContext getInstallationContext() {
        return this.mInstallationContext;
    }    
    /**
     * @return logger initialized from the installation context or a default
     * logger.
     */
    public Logger  getLogger() {
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
     * default noop implementation of the cleanup.
     * @see javax.jbi.component.Bootstrap#cleanUp()
     */
    public void cleanUp() throws javax.jbi.JBIException {
        this.getLogger().info( this.mInstallationContext.getComponentName() +
            " Component Installer Cleaned up");
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
        // initialize reference to component context
        this.mInstallationContext = installContext;
        doInit();
        // initialize logger
        this.getLogger().info(this.mInstallationContext.getComponentName() +
            " : Component Installer initialized");
    }
    /**
     * Chance to extended classes to do the component installation specific init.
     * Installation context will be initialized before calling this method. So,
     * initialize other things if required in this method.
     * @throws javax.jbi.JBIException on error
     * @see com.sun.jbi.sample.component.common.BootstrapImpl#init(
     * javax.jbi.component.InstallationContext)
     */
    protected void doInit() throws JBIException {
        this.getLogger().info(this.getInstallationContext().getComponentName() + 
            " : Component Installer initialized");
    }
    
    /**
     * default implementation that does not have extension mbean return null.
     * @see javax.jbi.component.Bootstrap#getExtensionMBeanName()
     */
    public javax.management.ObjectName getExtensionMBeanName() {
        return null;
    }
    /**
     * default implementation just logs the method call.
     * @see javax.jbi.component.Bootstrap#onInstall()
     */
    public void onInstall()  throws javax.jbi.JBIException {
        //TODO: do required initializaton and resource creation
        this.getLogger().info(this.mInstallationContext.getComponentName() +
            " : Component installed.");
    }
    /**
     * default implementation just logs the method call.
     * @see javax.jbi.component.Bootstrap#onUninstall()
     */
    public void onUninstall() throws javax.jbi.JBIException {
        //TODO: do clean up the resource and other stuff.
        this.getLogger().info(this.mInstallationContext.getComponentName() +
            " : Component uninstalled");
    }
}
