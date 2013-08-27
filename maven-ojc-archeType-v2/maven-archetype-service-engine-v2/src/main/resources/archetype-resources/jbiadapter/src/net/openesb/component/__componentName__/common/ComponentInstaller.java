#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
 /*
  * ComponentInstaller.java
  */
package net.openesb.component.${componentName}.common;

import java.util.List;
import java.util.logging.Level;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

/**
 * This class implements the javax.jbi.component.Bootstrap. This class implements
 * the minimum required functionality for component installation contract
 * of the jbi component in the jbi environment. It also takes care of maintaining
 * the installation context reference and installation specific logger creation and
 * installation extension mbean registration and unregistration during initialization
 * and cleanup. Extended classes only need to supply the mbean name and its implementation
 * object for the installation extensions.
 *
 * It makes sure that the installation context and the loggers are available to
 * the classes extended from this class to add more installation specific functionality.
 *
 * Service engine or a binding component installation implementation can directly use
 * this class or extend this class to add more functionality to their installation
 * process.
 *
 * @see javax.jbi.Bootstrap
 * @author chikkala
 */
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
     * @see javax.jbi.component.Bootstrap${symbol_pound}init(javax.jbi.component.InstallationContext)
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
     * @see javax.jbi.component.Bootstrap${symbol_pound}onInstall()
     */
    public void onInstall()  throws javax.jbi.JBIException {
        //TODO: do required initializaton and resource creation
        this.getLogger().fine(this.mInstallationContext.getComponentName() +
            " : Component installed.");
    }
    /**
     * default implementation just logs the method call.
     * @see javax.jbi.component.Bootstrap${symbol_pound}onUninstall()
     */
    public void onUninstall() throws javax.jbi.JBIException {
        //TODO: do clean up the resource and other stuff.
        this.getLogger().fine(this.mInstallationContext.getComponentName() +
            " : Component uninstalled");
    }    
    /**
     * default noop implementation of the cleanup.
     * @see javax.jbi.component.Bootstrap${symbol_pound}cleanUp()
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
     * @see javax.jbi.component.Bootstrap${symbol_pound}getExtensionMBeanName()
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
     * @see com.sun.jbi.sample.component.common.ComponentInstaller${symbol_pound}init(
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
     * @see com.sun.jbi.sample.component.common.ComponentInstaller${symbol_pound}cleanUp(
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
    /**
     * This method is called by the JBI runtime to allow a component to upgrade
     * it's workspace and any existing Service Units to match the new version of
     * the component. The JBI runtime copies the existing workspace root to the
     * new workspace root prior to calling this method, so that the component
     * can examine the contents of the workspace root to determine the version
     * of the component from which the upgrade is being made. All updates to the
     * workspace root are done in-place; in the event of a failure, the JBI
     * runtime reverts back to the original workspace root, the original install
     * root, and the original Service Unit roots for the component.
     *
     * Note1: The component must ensure that it never holds open references
     * to any files in the workspace root or any of the Service Unit roots upon
     * returning from this method. Failure to do so will cause problems when
     * the runtime attempts to complete the upgrade processing.
     * 
     * Note2: The installation context that is normally initialized
     * by the runtime by calling ${symbol_pound}init method of the javax.jbi.Bootstrap interface
     * before install and uninstall will not be called before calling this
     * upgrade method. So, installation context can not be used in this method's
     * implementation.
     * 
     * Note3: Since there won't be any installation context available, the logger
     * that returned from the ${symbol_pound}getLogger method is a default logger created using
     * the package name of this class or extended class if the method is overridden
     * in the extended class.
     * 
     * @param workspaceRoot the workspace root for the new version of the
     * component that will replace the currently installed version. This is
     * populated with the contents of the original workspace root and the
     * component must update it to match the new version of the component.
     * @param serviceUnitRoots a list of directory paths to all of the Service
     * Units currently deployed to the component. The component must update all
     * of these to match the new version of the component.
     * @exception JBIException when there is an error requiring that the upgrade
     * be terminated.
     */    
    public void upgrade(String workspaceRoot, List<String> serviceUnitRoots)
        throws javax.jbi.JBIException
    {
        //TODO: override this method in the extended class to handle the 
        // workspace root and service unit roots upgrade according to the 
        // component's requirement.
        this.getLogger().fine("Default upgrade implementation. " +
                "TODO: Implement workspace root and service unit roots upgrade.");
    }
}
