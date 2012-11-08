/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
/*
 * AbstractComponentLifeCycle.java
 *
 */

package it.imolinfo.jbi4corba.jbi.component.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaPerformanceMeasurement;
import it.imolinfo.jbi4corba.jbi.component.ReadWriteTextFile;

import java.io.File;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.internationalization.Messages;

/**
 * This class is an abstract implemenation of the ComponentLifeCycle. It
 * implements the component lifecycle methods and provides necessary hooks to
 * the extended classes to supply the component specific implemenation for the
 * lifecycle functionality.
 * 
 * @see javax.jbi.ComponentLifeCycle
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public abstract class AbstractComponentLifeCycle implements ComponentLifeCycle {

	/** The logger. */
	private static final Logger LOG = LoggerFactory
			.getLogger(AbstractComponentLifeCycle.class);

	private static final Messages MESSAGES = Messages
			.getMessages(AbstractComponentLifeCycle.class);

	/** reference to the component runtime */
	private ComponentRuntime mCompRuntime;
	/** Extension Mbean Name */
	private ObjectName mExtensionMBeanName;
	/** Extension Mbean Implemenation */
	private StandardMBean mExtensionMBeanImpl;
	/** a message receiver that accepts messages from delivery channel */
	private MessageExchangeReceiver mMsgExchReceiver;

	protected RuntimeConfiguration mRuntimeConfig;
	protected RuntimeConfigurationHelper mRuntimeConfigHelper;

	/** A short display Name **/
	public static final String SHORT_DISPLAY_NAME = "sun-jbi4corba-bc";

	// "Official" Performance Instrumentation Categories
	public static final String PERF_CAT_NORMALIZATION = "Normalization"; // NOI18N
	public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N
	public static final String[] JBI4CORBABC_PERF_CATEGORIES = new String[] {
			PERF_CAT_NORMALIZATION, PERF_CAT_DENORMALIZATION };
	protected ComponentContext jbiContext = null;
	protected StatusProviderHelper mStatusProviderHelper;

	/**
	 * constructor for the ComponentLifecycle implementation.
	 * 
	 * @param compRuntime
	 */
	protected AbstractComponentLifeCycle(ComponentRuntime compRuntime) {
		this.mCompRuntime = compRuntime;
	}

	// /////////////////////////////////////////////////////////////////////////
	// ComponentLifeCycle interface implemenation
	// /////////////////////////////////////////////////////////////////////////

	/**
	 * Initialize the component.
	 * 
	 * @param context
	 * @throws javax.jbi.JBIException
	 * @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext)
	 */
	public final void init(ComponentContext context) throws JBIException {

		if (context == null) {
			String msg = MESSAGES
					.getString("CRB000429_Null_Component_Context_received_in_Component_Lifecycle_init");
			LOG.error(msg);
			throw new JBIException(msg);
		}
		// register ComponentContext for retrieving component logger
		Messages.registerContext(context);
		// initialize the content
		initContext(context);

		// create and register extension mbean
		registerExtensionMBean();
		// create and register monitoring and management mbean
		registerMonitoringMbean();
		// create and register RuntimeConfig mbean
		registerRuntimeConfigurationMBean();

		// initialize the message exchange handler factory
		initMessageExchangeHandlerFactory();
		// initialize the message receiver
		initMessageExchangeReceiver(mRuntimeConfig);

		// call the onInit to allow other initialization that is specific to the
		// component
		onInit();

		LOG.info("CRB000401_Component_initialized",
				new Object[] { RuntimeHelper.getComponentName() });
	}

	/**
	 * Start the component.
	 * 
	 * @throws javax.jbi.JBIException
	 * @see javax.jbi.component.ComponentLifeCycle#start()
	 */
	public final void start() throws JBIException {

		// open the delivery channel from the component context
		openDeliveryChannel();

		activateServiceProviders();
		activateServiceConsumers();

		startMessageExchangeProcessing();
		// call onStart to perform component specific tasks on start
		onStart();
		LOG.info("CRB000402_Component_started", new Object[] { RuntimeHelper
				.getComponentName() });
	}

	/**
	 * Stop the component.
	 * 
	 * @throws javax.jbi.JBIException
	 * @see javax.jbi.component.ComponentLifeCycle#stop()
	 */
	public final void stop() throws JBIException {

		stopMessageExchangeProcessing();
		// deactivate endpoints
		deactivateServiceConsumers();
		deactivateServiceProviders();
		// close channel. No further messages to be processed.
		closeDeliveryChannel();
		// call onStop to perform component specific tasks on stop
		onStop();
		LOG.info("CRB000403_Component_stopped", new Object[] { RuntimeHelper
				.getComponentName() });
	}

	/**
	 * Shut down the component.
	 * 
	 * @throws javax.jbi.JBIException
	 * @see javax.jbi.component.ComponentLifeCycle#shutDown()
	 */
	public final void shutDown() throws JBIException {
		// remove the message receiver.
		shutdownMessageExchangeReceiver();
		// unregister extension mbean and remove it
		unregisterExtensionMBean();
		// unregister ManagementMbean and remove it
		unregisterManagementMbean();
		// unregister runtimeConfig mbean and remove it
		unregisterRuntimeConfigurationMBean();
		// call onShutdown to perform component specific tasks on shutdown
		onShutDown();
		LOG.info("CRB000404_Component_shut_downd", new Object[] { RuntimeHelper
				.getComponentName() });
	}

	/**
	 * this is a default implementation which does not have any extension
	 * mbeans. extended classes can return the mbean name for the extension
	 * mbeans if the component supports.
	 * 
	 * 
	 * @see javax.jbi.component.ComponentLifeCycle#getExtensionMBeanName()
	 * @return
	 */
	public final ObjectName getExtensionMBeanName() {
		return this.mExtensionMBeanName;
	}

	/**
	 * if there is an extension mbean supported by the component, then register
	 * it with the mbean server.
	 */
	private void registerExtensionMBean() throws JBIException {

		this.mExtensionMBeanName = this.createExtensionMBeanName();

		if (this.mExtensionMBeanName == null) {
			RuntimeHelper
					.logDebug("No Extension MBean is registerd with MBeanServer for "
							+ RuntimeHelper.getComponentName());
			return;
		}
		// create the extension mbean implemenation if the object name is
		// created.
		this.mExtensionMBeanImpl = this.createExtensionMBean();

		if (this.mExtensionMBeanImpl == null) {
			this.mExtensionMBeanName = null;
			RuntimeHelper
					.logDebug("No Extension MBean is registerd with MBeanServer for "
							+ RuntimeHelper.getComponentName());
			return;
		}
		// register with mbean only if object name and implementation are non
		// null
		try {
			MBeanServer mbServer = RuntimeHelper.getComponentContext()
					.getMBeanServer();
			mbServer.registerMBean(this.mExtensionMBeanImpl,
					this.mExtensionMBeanName);
		} catch (Exception e) {
			this.mExtensionMBeanName = null;
			this.mExtensionMBeanImpl = null;
			String msg = MESSAGES
					.getString(
							"CRB000405_Cannot_register_Extension_MBean_with_MBeanServer",
							new Object[] { RuntimeHelper.getComponentName() });
			LOG.error(msg, e);
			throw new JBIException(msg, e);
		}
	}

	private void registerMonitoringMbean() throws JBIException {
		MBeanNames mbnHndl = null;
		/** Runtime Monitoring Mbean **/
		try {
			jbiContext = RuntimeContext.getInstance().getComponentContext();
			mbnHndl = jbiContext.getMBeanNames();
			if (mbnHndl != null) {
				ObjectName statusMBeanObjName = mbnHndl
						.createCustomComponentMBeanName("Statistics");
				mStatusProviderHelper = new StatusProviderHelper(
						SHORT_DISPLAY_NAME, statusMBeanObjName, jbiContext
								.getMBeanServer());
				mStatusProviderHelper.registerMBean(
						JBI4CORBABC_PERF_CATEGORIES,
						new Jbi4CorbaPerformanceMeasurement());
			}
			LOG.info("CRB000406_Jbi4Corba_Register_mbean",
					new Object[] { jbiContext.getComponentName() });
		} catch (final Exception ex) {
			String msg = MESSAGES.getString(
					"CRB000407_Jbi4Corba_Failed_register_mbean",
					new Object[] { RuntimeHelper.getComponentName() });
			LOG.error(msg, ex);
			throw new JBIException(msg, ex);
		}
	}

	/**
	 * if there is an extension mbean supported by the component, then register
	 * it with the mbean server.
	 */
	private void registerRuntimeConfigurationMBean() throws JBIException {
		try {
			ComponentContext context = RuntimeContext.getInstance()
					.getComponentContext();
			String configData = "";
			String configSchema = "";
			String configDataFileLoc = context.getInstallRoot()
					+ File.separator + "META-INF" + File.separator
					+ "componentConfiguration.xml";
			File configDataFile = new File(configDataFileLoc);
			if (configDataFile.exists()) {
				configData = ReadWriteTextFile.getContents(configDataFile);
				String configSchemaFileLoc = context.getInstallRoot()
						+ File.separator + "META-INF" + File.separator
						+ "componentConfiguration.xsd";
				File configSchemaFile = new File(configSchemaFileLoc);
				if (configSchemaFile.exists()) {
					configSchema = ReadWriteTextFile
							.getContents(configSchemaFile);
				}
				mRuntimeConfig = new RuntimeConfiguration(context
						.getWorkspaceRoot(), configData, configSchema);
			} else {
				// if the file doesn't exists we assume we aren't running in
				// openesb and so create an empty runtime configuration
				mRuntimeConfig = new RuntimeConfiguration(context
						.getWorkspaceRoot(), configSchema);
				String msg = MESSAGES
						.getString("CRB000408_Failed_to_locate_configuration_file");
				LOG.warn(msg);

			}

			mRuntimeConfigHelper = new RuntimeConfigurationHelper(
					RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING, context
							.getComponentName(), context.getMBeanServer());
			mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
			RuntimeHelper.logInfo("Registering runtime config mbean for"
					+ RuntimeHelper.getComponentName());

		} catch (final Exception ex) {
			String msg = MESSAGES.getString(
					"CRB000409_Failed_to_register_runtime_configuration_mbean",
					new Object[] { ex.toString() });
			LOG.error(msg, ex);
			throw new JBIException(msg, ex);
		}
	}

	/**
	 * remove the registered extension mbean from the mbean server.
	 */
	private void unregisterExtensionMBean() throws JBIException {
		if (this.mExtensionMBeanName != null) {
			try {
				MBeanServer mbServer = RuntimeHelper.getComponentContext()
						.getMBeanServer();
				mbServer.unregisterMBean(this.mExtensionMBeanName);
			} catch (Exception e) {
				String msg = MESSAGES
						.getString(
								"CRB000410_Cannot_unregister_Extension_MBean_with_MBeanServer",
								new Object[] { RuntimeHelper.getComponentName() });
				LOG.error(msg, e);
				throw new JBIException(msg, e);
			} finally {
				this.mExtensionMBeanName = null;
				this.mExtensionMBeanImpl = null;
			}
		}
	}

	private void unregisterManagementMbean() throws JBIException {
		try {
			mStatusProviderHelper.unregisterMBean();
		} catch (final Exception ex) {
			String msg = MESSAGES.getString(
					"CRB000430_Failed_to_unregister_statusProviderMBean",
					new Object[] { jbiContext.getComponentName() });
			LOG.error(msg, ex);
			throw new JBIException(msg, ex);
		}
	}

	/**
	 * remove the registered extension mbean from the mbean server.
	 */
	private void unregisterRuntimeConfigurationMBean() throws JBIException {
		try {
			if (mRuntimeConfigHelper != null) {
				mRuntimeConfigHelper.unregisterMBean();
			}
		} catch (Exception ex) {
			LOG.info("CRB000425_Exception_during_runtimecfg_mbean_deregister",
					new Object[] { ex.getLocalizedMessage() });

		}
	}

	/**
	 * 
	 * @return
	 */
	public final ComponentRuntime getComponentRuntime() {
		return this.mCompRuntime;
	}

	/**
	 * initializes the RuntimeContext using ComponentContext passed by init
	 * method.
	 * 
	 * @param context
	 */
	private void initContext(ComponentContext context) {
		RuntimeContext.getInstance().setComponentContext(context);
		RuntimeContext.getInstance().setLogger(
				this.getClass().getPackage().getName(), null);
	}

	/**
	 * creates a message receiver object as part of the component
	 * initialization.
	 * 
	 * @throws javax.jbi.JBIException
	 */
	private void initMessageExchangeReceiver(RuntimeConfiguration mRuntimeConfig)
			throws JBIException {
		try {

			// create message receiver
			this.mMsgExchReceiver = createMessageExchangeReceiver();
			if (this.mMsgExchReceiver != null) {
				this.mMsgExchReceiver.initReceiver(mRuntimeConfig);
			}
		} catch (Exception ex) {
			String msg = MESSAGES
					.getString("CRB000426_Failure_in_init_MessageExchangeReceiver");
			LOG.error(msg, ex);
			throw new JBIException(msg, ex);

		}
	}

	/**
	 * removes the message receiver as part of the component shutdown process
	 * 
	 * @throws javax.jbi.JBIException
	 */
	private void shutdownMessageExchangeReceiver() throws JBIException {
		try {
			if (this.mMsgExchReceiver != null) {
				this.mMsgExchReceiver.shutdownReceiver();
			}
		} catch (Exception ex) {
			ex.printStackTrace();
		} finally {
			this.mMsgExchReceiver = null;
		}
	}

	/**
	 * allows the component to accept the message exchange objects from the
	 * delivery channel and process it as part of the component startup process.
	 * 
	 * @throws javax.jbi.JBIException
	 */
	private void startMessageExchangeProcessing() throws JBIException {
		try {
			// start message processing
			if (this.mMsgExchReceiver != null) {
				this.mMsgExchReceiver.startProcessing();
			}
		} catch (Exception ex) {
			String msg = MESSAGES
					.getString("CRB000427_Failure_in_start_MessageExchangeProcessing");
			LOG.error(msg, ex);
			throw new JBIException(msg, ex);
		}
	}

	/**
	 * stops the component from accepting the message exchange objects from the
	 * delivery channel as part of the component stop process
	 * 
	 * @throws javax.jbi.JBIException
	 */
	private void stopMessageExchangeProcessing() throws JBIException {
		try {
			// stop message processing
			if (this.mMsgExchReceiver != null) {
				this.mMsgExchReceiver.stopProcessing();
			}
		} catch (Exception ex) {
			String msg = MESSAGES
					.getString("CRB000428_Failure_in_stop_MessageExchangeProcessing");
			LOG.error(msg, ex);
			throw new JBIException(msg, ex);
		}

	}

	/**
	 * opens the delivery channel to accept the message exchange objects or send
	 * the message exchange objects
	 */
	private void openDeliveryChannel() {
		RuntimeContext.getInstance().openDeliveryChannel();
	}

	/**
	 * closes the delivery channel as part of the component shutdown process.
	 */
	private void closeDeliveryChannel() {
		RuntimeContext.getInstance().closeDeliveryChannel();
	}

	/**
	 * create jmx object name for the extension mbean
	 */
	protected abstract ObjectName createExtensionMBeanName();

	/**
	 * create mbean implemenation for the extension mbean as a StandardMBean
	 */
	protected abstract StandardMBean createExtensionMBean();

	/**
	 * initializes the message exchange handler factory. components implement
	 * this method to provide their own handler factory
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected abstract void initMessageExchangeHandlerFactory()
			throws JBIException;

	/**
	 * creates a message receiver object that handles receiving and processing
	 * the message exchanges from the delivery channel. Component should
	 * implement this method to provide the MessageReceiver.
	 * 
	 * Component may return null indicating that they don't need the message
	 * receiver that can receive and process message exchanges from delivery
	 * channel. For example, components that have only service consumers which
	 * send a synchronous messages to providers don't need this.
	 * 
	 * @throws java.lang.Exception
	 * @return
	 */
	protected abstract MessageExchangeReceiver createMessageExchangeReceiver()
			throws Exception;

	/**
	 * service providers initialization such as creating the service
	 * descriptions and activating the service endpoints should be done. If the
	 * component supports deployment, deployment manager should be notified to
	 * active service providers from the deployed service units. This method is
	 * invoked in the implementation of ComponentLifeCycle start method
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected abstract void activateServiceProviders() throws JBIException;

	/**
	 * disable the service providers so that the service consumers can not find
	 * the service endpoints for this service provider. This method is invoked
	 * in the implementation of ComponentLifeCycle stop method
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected abstract void deactivateServiceProviders() throws JBIException;

	/**
	 * service consumer initialization such as creating the service descriptions
	 * and activating the protocol specific external endpoints should be done.
	 * If the component supports deployment, deployment manager should be
	 * notified to initialize service consumers from the deployed service units.
	 * This method is invoked in the implementation of ComponentLifeCycle start
	 * method
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected abstract void activateServiceConsumers() throws JBIException;

	/**
	 * disables the service consumers so that the service consumer can not send
	 * message exchange to the providers. This is called in the implemenation of
	 * component lifecycle stop method This method is invoked in the
	 * implementation of ComponentLifeCycle stop method
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected abstract void deactivateServiceConsumers() throws JBIException;

	/**
	 * chance to extended classes to do the component specific init
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected void onInit() throws JBIException {
		// NOOP
	}

	/**
	 * chance to extended classes to do the component specific start
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected void onStart() throws JBIException {
		// NOOP
	}

	/**
	 * chance to extended classes to do the component specific stop
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected void onStop() throws JBIException {
		// NOOP
	}

	/**
	 * chance to extended classes to do the component specific shutdown
	 * 
	 * @throws javax.jbi.JBIException
	 */
	protected void onShutDown() throws JBIException {
		// NOOP
	}

}
