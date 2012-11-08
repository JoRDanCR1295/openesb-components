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
 * @(#)AspectSEComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect;

import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;
import javax.management.AttributeChangeNotification;
import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanRegistrationException;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.management.ObjectName;

import com.sun.jbi.component.config.ConfigPersistence;
import com.sun.jbi.component.config.RuntimeConfigurationHelper;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.endpoint.impl.DefaultEndpointManager;
import com.sun.jbi.component.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.impl.DefaultAcceptManager;
import com.sun.jbi.crl.mep.impl.PatternRoleKey;
import com.sun.jbi.crl.mep.impl.SimpleProcessorFactory;
import com.sun.jbi.crl.util.LogUtil;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.engine.aspect.endpoint.factory.AspectSEEndpointFactory;
import com.sun.jbi.engine.aspect.process.AspectSEInOnlyConsumer;
import com.sun.jbi.engine.aspect.process.AspectSEInOnlyProvider;
import com.sun.jbi.engine.aspect.process.AspectSEInOutConsumer;
import com.sun.jbi.engine.aspect.process.AspectSEInOutProvider;
import com.sun.jbi.engine.aspect.utils.AnnotatedStandardMBean;
import com.sun.jbi.engine.aspect.utils.ReadWriteTextFile;

/**
 * 
 * 
 * This class implements the <code>javax.jbi.component.Component</code>
 * interface, allows the JBI
 * implementation to query the component for various types of information. This
 * includes:
 * <ul>
 * 
 * <li>The component’s life cycle control interface.
 * <li>The component’s service unit manager, for handling deployments.
 * <li>A method for querying service metadata describing services provided by
 * this component.
 * <li>“Policy” methods that are called by the JBI implementation to query if
 * proposed matches of this component to a provider (or consumer) are
 * acceptable, according to this component’s policies.
 * <li>Endpoint reference (EPR) resolution.
 * 
 * </ul>
 * Some components will provide the ability to resolve EPRs (typically binding
 * components). This ability to resolve EPRs is used by JBI to facilitate
 * resolution of EPRs received by service consumers. The name of the class that
 * implements this interface for a component is specified in the installation
 * descriptor for that component.
 * 
 * 
 * @author Sujit Biswas
 * 
 */
public class AspectSEComponentManager extends AbstractComponentManager
		implements NotificationListener {

	private static final Logger logger = Logger
			.getLogger(AspectSEComponentManager.class.getName());

	private StatusProviderHelper statusProviderHelper;

	private RuntimeConfigurationHelper runtimeConfigHelper;

	/** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#init(javax.jbi.component.ComponentContext) */
	public void init(ComponentContext context) throws JBIException {
		ObjectName objectName = null;
                //LogUtil.registerContext(context);
		logger.info("Initializing Aspect service engine");
		Properties configProp = new Properties();
		String workspaceRoot = null;
		String configFile = null;
		try {
			// Prepare Installation Context: JBI 1.0 pr spec 6.4.2.1.1
			workspaceRoot = context.getWorkspaceRoot();
			configFile = workspaceRoot + File.separator
					+ ConfigPersistence.PERSISTENT_CONFIG_FILE_NAME;
			configProp.load(new FileInputStream(configFile));
			logger.info("Aspect se config properties: \n"
					+ String.valueOf(configProp));
		} catch (Exception ex) {
			String error = "Failed to initialize Aspect SE: " + ex.getMessage();
			logger.log(Level.SEVERE, error, ex);
			throw new JBIException(error, ex);
		}

        String displaySchema = "";
        String displaySchemaFileLocation = context.getInstallRoot() + File.separator
               +"META-INF"+File.separator+ "componentconfiguration.xsd";
        File displaySchemaFile = new File(displaySchemaFileLocation);
        if (false == displaySchemaFile.exists()) {
            System.out.println("Configuration Display Schema file does not exist: "
                    + displaySchemaFile.getAbsolutePath());
        } else {
            displaySchema = ReadWriteTextFile.getContents(displaySchemaFile);
        }
        
        String displayData = "";
        String displayDataFileLocation = context.getInstallRoot() + File.separator
                +"META-INF"+File.separator+ "componentconfiguration.xml";
        File displayDataFile = new File(displayDataFileLocation);
        if (false == displayDataFile.exists()) {
            System.out.println("Configuration Display Data file does not exist: "
                    + displayDataFile.getAbsolutePath());
        } else {
            displayData = ReadWriteTextFile.getContents(displayDataFile);
        }

		try {
			runtimeConfigHelper = new RuntimeConfigurationHelper(
                    RuntimeConfigurationHelper.constructObjectName(
        					RuntimeConfigurationHelper.COMPONENT_TYPE_ENGINE, 
                            context.getComponentName()), 
                    context.getMBeanServer());

			AspectSEComponentConfiguration mConfigMBean = new AspectSEComponentConfiguration(
					displaySchema, displayData, configFile);
			Object mbean = new AnnotatedStandardMBean(mConfigMBean,
					AspectSEComponentConfigurationMBean.class);

			objectName = runtimeConfigHelper.getRuntimeConfigObjectName();
			if (objectName != null) {
				runtimeConfigHelper.registerMBean(mbean);
			}
		} catch (MBeanRegistrationException ex) {
			throw new JBIException(
					"AspectSEComponentManager.unable_to_register_mbean", ex);
		} catch (NotCompliantMBeanException ex) {
			throw new JBIException(
					"AspectSEComponentManager.unable_to_create_runtime_configuration_mbean",
					ex);
		} catch (MalformedObjectNameException ex) {
			throw new JBIException(
					"AspectSEComponentManager.unable_to_create_runtime_configuration_helper",
					ex);
		} catch (InstanceAlreadyExistsException ex) {
			throw new JBIException(
					"AspectSEComponentManager.unable_to_create_runtime_configuration_mbean",
					ex);
		}

		try {
			statusProviderHelper = new StatusProviderHelper("AspectSE Status",
					StatusProviderMBean.COMPONENT_TYPE_ENGINE, context
							.getComponentName(), context.getMBeanServer());
			statusProviderHelper.registerMBean();
			if (logger.isLoggable(Level.INFO)) {
				logger.info("Registered Status Provider MBean for "
						+ context.getComponentName());
			}
		} catch (Exception e) {
			logger.log(Level.WARNING,
					"Failed to register status provider MBean", e);
			throw new JBIException("Failed to register status provider MBean",
					e);
		}
		super.init(context);

		logger.info("Initialized Aspect service engine successfully");

	}

	/** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#shutDown() */
	public void shutDown() throws JBIException {
		logger.info("Shutting down Aspect service engine");
		try {
			// Must close DeliveryChannel before stopping accept threads
			// because they may be pending on accept().
			// Closing channel will wake it up from accept()
			DeliveryChannel channel = getComponentContext()
					.getDeliveryChannel();
			if (channel != null) {
				channel.close();
			}
			super.shutDown();
		} catch (Exception e) {
			String msg = "Aspect service engine shut down error: "
					+ e.getMessage();
			logger.log(Level.WARNING, msg, e);
			throw new JBIException(msg, e);
		}

		try {
			statusProviderHelper.unregisterMBean();
		} catch (Exception e) {
			String msg = "Failed to unregister status provider MBean for "
					+ getComponentContext().getComponentName();
			logger.log(Level.WARNING, msg, e);
			throw new JBIException(msg, e);
		}

		try {
			runtimeConfigHelper.unregisterMBean();
		} catch (Exception e) {
			String msg = "Failed to unregister runtime configuration MBean for "
					+ getComponentContext().getComponentName();
			logger.log(Level.WARNING, msg, e);
			throw new JBIException(msg, e);
		}

		logger.info("Shut down Aspect service engine successfully");
	}

	/** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#start() */
	public void start() throws JBIException {
		logger.info("Starting Aspect service engine");
		super.start();
		logger.info("Started Aspect service engine successfully");
	}

	/** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#stop() */
	public void stop() throws JBIException {
		logger.info("Stopping Aspect service engine");
		super.stop();
		logger.info("Stopped Aspect service engine successfully");
	}

	/** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initAcceptManager() */
	protected AcceptManager initAcceptManager() {
		// initialize AcceptManager
		AcceptManager amgr = this.getAcceptManager();
		if (amgr == null) {
			amgr = new DefaultAcceptManager(this);
			this.setAcceptManager(amgr);
		}
		// register processors
		ExchangeRouter router = amgr.getExchangeRouter();
		router.register(PatternRoleKey.IN_OUT_PROVIDER,
				new SimpleProcessorFactory(new AspectSEInOutProvider()));
		router.register(PatternRoleKey.IN_OUT_CONSUMER,
				new SimpleProcessorFactory(new AspectSEInOutConsumer()));
		router.register(PatternRoleKey.IN_ONLY_PROVIDER,
				new SimpleProcessorFactory(new AspectSEInOnlyProvider()));
		router.register(PatternRoleKey.IN_ONLY_CONSUMER,
				new SimpleProcessorFactory(new AspectSEInOnlyConsumer()));

		return amgr;
	}

	/** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
	protected EndpointManager initEndpointManager() {
		EndpointManager endpointManager = (DefaultEndpointManager) this
				.getEndpointManager();
		AspectSEEndpointFactory factory = new AspectSEEndpointFactory();
		if (endpointManager == null) {
			endpointManager = new DefaultEndpointManager(factory);
			this.setEndpointManager(endpointManager);

		}
		return endpointManager;
	}

	/** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initServiceUnitManager(javax.jbi.component.ComponentContext) */
	protected ServiceUnitManager initServiceUnitManager() {
		AspectSEServiceUnitManagerAbs serviceUnitManager = (AspectSEServiceUnitManagerAbs) this
				.getServiceUnitManager();
		if (serviceUnitManager == null) {
			serviceUnitManager = new AspectSEServiceUnitManager(
					getComponentContext(), getEndpointManager());
			serviceUnitManager.setStatusProvider(statusProviderHelper);

			this.setServiceUnitManager(serviceUnitManager);
		}
		return serviceUnitManager;
	}

	public void handleNotification(Notification notification, Object obj) {
		logger
				.log(Level.INFO,
						"AspectSEComponentManager.Handling_notification");

		AttributeChangeNotification attributeNotification = null;

		if (notification instanceof AttributeChangeNotification) {
			attributeNotification = (AttributeChangeNotification) notification;

			// Check if configuration change is for Aspect SE component
			if (attributeNotification.getSource() instanceof AspectSEComponentConfigurationMBean) {
				String attributeName = attributeNotification.getAttributeName();
				attributeName.toString();

				// TODO take care of changes at the SE level
			}
			// Check if configuration change is for Aspect SE Service Unit
			if (attributeNotification.getSource() instanceof AspectSEServiceUnitConfigurationMBean) {
				// TODO take care of changes at the SU level
			}

		}
	}

}
