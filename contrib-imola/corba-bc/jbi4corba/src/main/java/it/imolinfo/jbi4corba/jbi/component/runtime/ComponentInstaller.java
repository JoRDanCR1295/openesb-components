 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.jbi.component.runtime;

import it.imolinfo.jbi4corba.jbi.Messages;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;


/**
 * This is a default implemenation of the Bootstrap interface.
 * The component implemenation can extend this class and implement
 * component specific installation such as configuration and creation of
 * of the resources.
 *
 * @see javax.jbi.Bootstrap
 *
 * @author Prem 
 */
public class ComponentInstaller implements Bootstrap {
    
    /** The logger. */
    private static final Logger LOG = 
    	LoggerFactory.getLogger(ComponentInstaller.class);
    private static final Messages MESSAGES = 
    	Messages.getMessages(ComponentInstaller.class);

    
    /**
     * Installation Context .
     */
    @SuppressWarnings("unused")
	private InstallationContext mContext;
    
    // Installer extension mbean 
    ObjectName mInstallerExtName;   
    
    /**
     * Constructor to create the ComponentInstaller.
     */
    public ComponentInstaller() {}
    
    /**
     * default noop implementation of the cleanup.
     * @see javax.jbi.component.Bootstrap#cleanUp()
     */
    public void cleanUp()
    throws javax.jbi.JBIException {
    	LOG.info("CRB000431_Component_Installer_Cleaned_up" );
    }
    
    /**
     * Initializes the installation environment for a component.
     *
     * @see javax.jbi.component.Bootstrap#init(javax.jbi.component.InstallationContext)
     */
    public void init(InstallationContext installContext)
    throws javax.jbi.JBIException {
              
        if ( installContext == null  ) {
        	LOG.info("CRB000432_Null_Installation_Context_received_in_init_Component_Bootstrap_initialization");
        }
        
        this.mContext = installContext;
        
        ComponentContext ctx = installContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();
        MBeanNames mbNames = ctx.getMBeanNames();
        
        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);
        
        InstallerExtMBean installerExt = new InstallerExt();
                    
        try {
            if (!mbServer.isRegistered(mInstallerExtName)) {
                StandardMBean installerExtMBean = new StandardMBean(installerExt, InstallerExtMBean.class);
                mbServer.registerMBean(installerExtMBean, mInstallerExtName);
            }
        } catch (Exception ex) {
        	String msg=MESSAGES.getString("CRB000423_InstallerMBean_registration_failed");
            LOG.error(msg,ex);
            throw new JBIException(msg,ex);
        }
        LOG.info("CRB000433_Received_MBean", new Object[]{mInstallerExtName});
               
        LOG.info("CRB000434_Component_Installer_initialized");
    }
    
    /**
     * default implemenation that does not have extension mbean return null.
     *
     * @see javax.jbi.component.Bootstrap#getExtensionMBeanName()
     */
    public javax.management.ObjectName getExtensionMBeanName() {
        return null;
    }
    
    /**
     * Default implemenation: just logs the method call.
     * @see javax.jbi.component.Bootstrap#onInstall()
     */
    public void onInstall()
    throws javax.jbi.JBIException {
    	LOG.info("CRB000435_Component_Installed");
    }
    
    /**
     * Default implemenation: just logs the method call.
     *
     * @see javax.jbi.component.Bootstrap#onUninstall()
     */
    public void onUninstall()
    throws javax.jbi.JBIException {
    	LOG.info("CRB000436_Component_Uninstalled");
    }
    
}
