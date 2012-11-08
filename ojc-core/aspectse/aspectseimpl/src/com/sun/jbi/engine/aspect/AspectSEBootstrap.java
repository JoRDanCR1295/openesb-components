/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)AspectSEBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect;

import javax.management.ObjectName;

import com.sun.jbi.crl.lifecycle.BootstrapFilterChain;

/**
 * 
 * 
 * 
 * This class implements the <code>javax.jbi.component.Bootstrap</code> interface is implemented by a JBI Component to provide any special
 * processing required at install/uninstall time. The methods defined here are
 * called by the JBI implementation during the installation (or uninstallation)
 * of the component that, among other things, supplies an implementation of this
 * interface. Initialization/cleanup tasks such as creation/deletion of
 * directories, files, and database tables can be done by the <b>onInstall()</b> 
 * and <b>onUninstall()</b>  methods, respectively. This also allows the component
 * to terminate the installation or uninstallation in the event of an error.
 * After calling onInstall()  or onUninstall() , regardless of outcome,
 * the JBI implementation must call the cleanUp() method afterwards.
 * Similarly, if <b>init(InstallationContext) </b> fails with an exception, the JBI
 * implementation must call the <b>cleanUp()</b> method. Component implementors
 * should note that there is no guarantee that the same instance of its
 * Bootstrap implementation will be used during both install and uninstall
 * operations on the component. Data that need to be retained between
 * installation-time and uninstallation-time must be persisted in such as
 * fashion that a separate instance of the bootstrap class can find them,
 * despite component or system shutdown.
 * 
 * 
 * @author Sujit Biswas
 * 
 */
public class AspectSEBootstrap extends BootstrapFilterChain {
	
	AspectSEConfigPersistenceFilter persistenceFilter;
	public AspectSEBootstrap() {
		super();
		persistenceFilter = new AspectSEConfigPersistenceFilter(
				this);
		addFilter(persistenceFilter);
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.crl.lifecycle.BootstrapFilterChain#getExtensionMBeanName()
	 */
	@Override
	public ObjectName getExtensionMBeanName() {
		// TODO Auto-generated method stub
		return persistenceFilter.getExtensionMBeanName();
	}
	
	
}
