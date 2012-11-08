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
 * @(#)OpenESBAdministrationService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package test.jbi.integration.test.framework;

import com.sun.jbi.ui.client.JBIAdminCommandsClientFactory;
import com.sun.jbi.ui.common.JBIAdminCommands;
import com.sun.jbi.ui.common.JBIArchive;
import com.sun.jbi.ui.common.JBIRemoteException;
import com.sun.jbi.ui.common.ServiceAssemblyDD;

/**
 *
 * OpenESB administration service
 */
public class OpenESBInstaller implements InstallerService{

	private JBIAdminCommands jbiCommands;
	private String targetName;

	public OpenESBInstaller(String host, int port, String username,
			String password, String targetName) throws JBIRemoteException {
		try {
			jbiCommands = JBIAdminCommandsClientFactory.getInstance(host, port,
					username, password);
			this.targetName = targetName;
		} catch (Throwable t) {
			throw new JBIRemoteException(t);
		}
	}

	public String deployServiceAssembly(String zipFileName) throws Exception {
		jbiCommands.deployServiceAssembly(zipFileName, targetName);
		JBIArchive sa = new JBIArchive(zipFileName);
		return ((ServiceAssemblyDD) sa.getJbiDescriptor()).getName();
	}

	public void stopServiceAssembly(String saName) throws JBIRemoteException {
		jbiCommands.stopServiceAssembly(saName, targetName);
	}

	public void startServiceAssembly(String saName) throws JBIRemoteException {
		jbiCommands.startServiceAssembly(saName, targetName);
	}

	public void undeployServiceAssembly(String saName)
			throws JBIRemoteException {
		shutdownServiceAssembly(saName);
		jbiCommands.undeployServiceAssembly(saName, targetName);
	}

	public void shutdownServiceAssembly(String saName)
			throws JBIRemoteException {
		jbiCommands.shutdownServiceAssembly(saName, targetName);
	}

	public boolean isJBIRuntimeEnabled() throws JBIRemoteException {
		return jbiCommands.isJBIRuntimeEnabled();
	}

	public String installComponent(String componentZipFile)
			throws JBIRemoteException {
		return jbiCommands.installComponent(componentZipFile, targetName);
	}

	public void uninstallComponent(String componentName)
			throws JBIRemoteException {
		jbiCommands.shutdownComponent(componentName, targetName);
		jbiCommands.uninstallComponent(componentName, targetName);
	}

	public void startComponent(String componentName)
			throws JBIRemoteException {
		jbiCommands.startComponent(componentName, targetName);
	}

	public void close() {
	}
}
