/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * ComponentInstaller.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.jbi.Messages;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.jbi.JBIException;

/**
 * This is a default implemenation of the Bootstrap interface.
 * The component implemenation can extend this class and implement
 * component specific installation such as configuration and creation of
 * of the resources.
 *
 * @see javax.jbi.Bootstrap
 *
 * @author Sun Microsystems, Inc.
 */
public class ComponentInstaller implements Bootstrap {
    
	/** The logger. */
    private static final Logger LOG = LoggerFactory.getLogger(ComponentInstaller.class);    
    private static final Messages MESSAGES = Messages.getMessages(ComponentInstaller.class);
    
    /**
     * Installation Context .
     */
    private InstallationContext mContext;
    
    /**
     * Constructor to creatre the ComponentInstaller.
     */
    public ComponentInstaller() {
    }
    
    /**
     * default noop implementation of the cleanup.
     * @see javax.jbi.component.Bootstrap#cleanUp()
     */
    public void cleanUp()
    throws javax.jbi.JBIException {
    	LOG.info("EJB000210_Component_Installer_Cleaned_up");
    }
    
    /**
     * Initializes the installation environment for a component.
     *
     * @see javax.jbi.component.Bootstrap#init(javax.jbi.component.InstallationContext)
     */
    public void init(InstallationContext installContext)
    throws javax.jbi.JBIException {
        
        if ( installContext == null  ) {
        	String msg=MESSAGES.getString("EJB000211_Null_Installation_Context_received");
            LOG.error(msg);
            throw new JBIException(msg);   

        }
        
        
        // initialize reference to component context
        this.mContext = installContext;
        
        LOG.info("EJB000212_Component_Installer_initialized");
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
     * default implemenation just logs the method call.
     * @see javax.jbi.component.Bootstrap#onInstall()
     */
    public void onInstall()
    throws javax.jbi.JBIException {
    	LOG.info("EJB000213_Component_Installed");
    }
    
    /**
     * default implemenation just logs the method call.
     *
     * @see javax.jbi.component.Bootstrap#onUninstall()
     */
    public void onUninstall()
    throws javax.jbi.JBIException {
    	LOG.info("EJB000214_Component_Uninstalled");
    }
    
}
