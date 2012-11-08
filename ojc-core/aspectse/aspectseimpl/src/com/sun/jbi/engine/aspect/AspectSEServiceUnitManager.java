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
 * @(#)AspectSEServiceUnitManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;

import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.engine.aspect.utils.AnnotatedStandardMBean;
import com.sun.jbi.engine.aspect.utils.ReadWriteTextFile;

/**
 * 
 * This class implements the <code>javax.jbi.component.ServiceUnitManager</code> interface which defines component-supplied methods for managing service unit
 * deployments, and is implemented by the component. The JBI implementation
 * queries the component for the implementation of this object using the
 * Component.getServiceUnitManager()  method.
 * 
 * 
 * @author Sujit Biswas
 * 
 */
public class AspectSEServiceUnitManager extends AspectSEServiceUnitManagerAbs {
	private Map<String, RuntimeConfigurationHelper> serviceUnitConfigurationMBeanMap = new HashMap<String, RuntimeConfigurationHelper>();

	public AspectSEServiceUnitManager(ComponentContext componentCtx,
			EndpointManager emgr) {
		super(componentCtx, emgr);
	}

	/**
	 * @see com.sun.jbi.component.lifecycle.impl.AbstractServiceUnitManager#init(java.lang.String,
	 *      java.lang.String)
	 */
	public void init(String serviceUnitName, String serviceUnitRootPath)
			throws DeploymentException {
		if (getLogger().isLoggable(Level.INFO)) {
			getLogger().info("Initializing service unit " + serviceUnitName);
		}

		super.init(serviceUnitName, serviceUnitRootPath);

		if (getLogger().isLoggable(Level.INFO)) {
			getLogger().info(
					"Initialized service unit " + serviceUnitName
							+ " serviceUnitRootPath: " + serviceUnitRootPath
							+ " successfully.");
		}

		// /////////////////////////////////////////
		// Create Service Unit Configuration MBean
		this.registerServiceUnitConfigMBean(serviceUnitName,
				serviceUnitRootPath);
		// /////////////////////////////////////////
	}

	/** @see com.sun.jbi.component.lifecycle.impl.AbstractServiceUnitManager#shutDown(java.lang.String) */
	public void shutDown(String serviceUnitName) throws DeploymentException {
		if (getLogger().isLoggable(Level.INFO)) {
			getLogger().info("Shutting service unit " + serviceUnitName);
		}

		super.shutDown(serviceUnitName);

		// /////////////////////////////////////////
		// Unregister Service Unit Configuration MBean
		this.unregisterServiceUnitConfigMBean(serviceUnitName);
		// /////////////////////////////////////////
	}

	void registerServiceUnitConfigMBean(String serviceUnitName,
			String serviceUnitRootPath) throws DeploymentException {
		final String META_INF_DIR = "META-INF";
		final String POLICY_FILE_NAME = "aspectmap.xsd";
		final String ASPECTMAP_FILE_NAME = "aspectmap.xml";
		ComponentContext componentContext = this.getComponentContext();
		String policySchemaString = "";
		String policyFileLocation = serviceUnitRootPath + File.separator
				+ POLICY_FILE_NAME;
		File policyFile = new File(policyFileLocation);
		if (false == policyFile.exists()) {
			System.out.println("Policy file does not exist: "
					+ policyFile.getAbsolutePath());
		} else {
			policySchemaString = ReadWriteTextFile.getContents(policyFile);
		}
		String aspectMapString = "";
		String aspectmapFileLocation = serviceUnitRootPath + File.separator
				+ ASPECTMAP_FILE_NAME;
		File aspectMapFile = new File(aspectmapFileLocation);
		if (false == aspectMapFile.exists()) {
			System.out.println("aspectmap file does not exist: "
					+ aspectMapFile.getAbsolutePath());
		} else {
			aspectMapString = ReadWriteTextFile.getContents(aspectMapFile);
		}

		ObjectName objectName = null;
		MBeanServer mbeanServer = componentContext.getMBeanServer();
		RuntimeConfigurationHelper runtimeConfigHelper = null;
		Object mbean = null;

		AspectSEServiceUnitConfiguration config = null;
		try {
			config = new AspectSEServiceUnitConfiguration(policySchemaString,
					aspectMapString, serviceUnitRootPath);
			mbean = new AnnotatedStandardMBean(config,
					AspectSEServiceUnitConfigurationMBean.class);
		} catch (NotCompliantMBeanException ncme) {
			// log error
		}

		try {
			runtimeConfigHelper = new RuntimeConfigurationHelper(
					RuntimeConfigurationHelper.COMPONENT_TYPE_ENGINE, this
							.getComponentContext().getComponentName(),
					mbeanServer, serviceUnitName);
		} catch (MalformedObjectNameException ex) {
			String msg = ex.getMessage();
			throw new DeploymentException(msg, ex);

		}
		try {
			if (runtimeConfigHelper != null) {
				objectName = runtimeConfigHelper.getRuntimeConfigObjectName();
				if ((objectName != null)
						&& (false == mbeanServer.isRegistered(objectName))
						&& (mbean != null)) {
					runtimeConfigHelper.registerMBean(mbean);
					this.serviceUnitConfigurationMBeanMap.put(serviceUnitName,
							runtimeConfigHelper);
				}
			}
		} catch (MBeanRegistrationException ex) {
			String msg = ex.getMessage();
			throw new DeploymentException(msg, ex);

		} catch (InstanceAlreadyExistsException ex) {
			String msg = ex.getMessage();
			throw new DeploymentException(msg, ex);

		} catch (NotCompliantMBeanException ex) {
			String msg = ex.getMessage();
			throw new DeploymentException(msg, ex);

		}
	}

	void unregisterServiceUnitConfigMBean(String serviceUnitName)
			throws DeploymentException {
		ObjectName objectName = null;
		MBeanServer mbeanServer = this.getComponentContext().getMBeanServer();
		RuntimeConfigurationHelper serviceUnitConfigurationMBean = null;
		serviceUnitConfigurationMBean = this.serviceUnitConfigurationMBeanMap
				.get(serviceUnitName);
		try {
			if (serviceUnitConfigurationMBean != null) {
				objectName = serviceUnitConfigurationMBean
						.getRuntimeConfigObjectName();
				if ((objectName != null)
						&& (true == mbeanServer.isRegistered(objectName))) {
					serviceUnitConfigurationMBean.unregisterMBean();
				}
			}
			shutdownAspectSEEndpoints(serviceUnitName);
		} catch (MBeanRegistrationException ex) {
			String msg = ex.getMessage();
			throw new DeploymentException(msg, ex);
		} catch (InstanceNotFoundException ex) {
			String msg = ex.getMessage();
			throw new DeploymentException(msg, ex);
		}
	}

	// ////////////////////////////////////////////
	// Helpers
	// ////////////////////////////////////////////
	public StatusProviderHelper getStatusProvider() {
		return super.getStatusProvider();
	}

	public void setStatusProvider(StatusProviderHelper prov) {
		super.setStatusProvider(prov);
	}

}
