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
 * @(#)BootstrapData.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.lifecycle;

import java.util.List;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.management.ObjectName;

/**
 * Utility class to encapsulate data required by a component's <code>Bootstrap</code>
 * implementation.  Per the JBI specification, the same instance of a <code>Bootstrap</code>
 * may not be used at both install and uninstall time.  Extensions of this class may
 * persist this data in different ways.  See {@link BootstrapFilterChain} for more
 * information.
 * 
 * @author Kevan Simpson
 */
public class BootstrapData {
    private List<BootstrapFilter> mChain = null;
    private InstallationContext mInstallationContext = null;
    private ComponentContext mComponentContext = null;
    private ObjectName mExtensionMBeanName = null;
    
	/**
	 * @return the <code>BootstrapFilterChain</code>.
	 */
	public List<BootstrapFilter> getChain() {
		return mChain;
	}
	/**
	 * @param chain the <code>BootstrapFilterChain</code> to set
	 */
	public void setChain(List<BootstrapFilter> chain) {
		mChain = chain;
	}
	/**
	 * @return the ComponentContext.
	 */
	public ComponentContext getComponentContext() {
		return mComponentContext;
	}
	/**
	 * @param componentContext the ComponentContext to set
	 */
	public void setComponentContext(ComponentContext componentContext) {
		mComponentContext = componentContext;
	}
	/**
	 * @return the ExtensionMBeanName
	 */
	public ObjectName getExtensionMBeanName() {
		return mExtensionMBeanName;
	}
	/**
	 * @param extensionMBeanName the ExtensionMBeanName to set
	 */
	public void setExtensionMBeanName(ObjectName extensionMBeanName) {
		mExtensionMBeanName = extensionMBeanName;
	}
	/**
	 * @return the InstallationContext
	 */
	public InstallationContext getInstallationContext() {
		return mInstallationContext;
	}
	/**
	 * @param installationContext the InstallationContext to set
	 */
	public void setInstallationContext(InstallationContext installationContext) {
		mInstallationContext = installationContext;
	}
}
