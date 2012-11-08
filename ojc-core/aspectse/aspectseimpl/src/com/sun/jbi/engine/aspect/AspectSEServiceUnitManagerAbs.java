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
 * @(#)AspectSEServiceUnitManagerAbs.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;

import com.sun.jbi.component.endpoint.Endpoint;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.descriptor.ServicesDescriptor;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.mgmt.task.ExceptionInfo;
import com.sun.jbi.component.mgmt.task.MsgLocInfo;
import com.sun.jbi.component.mgmt.task.TaskResult;
import com.sun.jbi.component.mgmt.task.TaskResultDetails;
import com.sun.jbi.component.mgmt.task.TaskXmlWriter;
import com.sun.jbi.component.mgmt.task.TaskResultDetails.MessageType;
import com.sun.jbi.crl.util.EntryRegistry;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;

public abstract class AspectSEServiceUnitManagerAbs implements
		ServiceUnitManager {
	public static final String DEPLOY_TASK = "deploy"; // NOI18N

	public static final String UNDEPLOY_TASK = "undeploy"; // NOI18N

	public static final String INIT_TASK = "init"; // NOI18N

	/** Maps service units by their names. */
	EntryRegistry<String, ServiceUnit> mServiceUnits = null;

	/** Maps endpoints by their service and interface qualified names. */
	EndpointManager mEndpoints = null;

	/** The component's context. */
	ComponentContext mComponentCtx = null;

	/** Helper for maintaining endpoint status information. */
	StatusProviderHelper mStatusProviderHelper;

	// TODO determine if Logger acquisition is appropriate
	private Logger mLogger = Logger.getLogger(getClass().getName());

	HashMap<String, ArrayList<AspectSEEndpoint>> provisioningEndpointMap = new HashMap<String, ArrayList<AspectSEEndpoint>>();

	public AspectSEServiceUnitManagerAbs(ComponentContext componentCtx,
			EndpointManager emgr) {
		mComponentCtx = componentCtx;
		mServiceUnits = new EntryRegistry<String, ServiceUnit>();
		mEndpoints = emgr;
	}

	/**
	 * This implementation performs the following actions:
	 * <ol>
	 * <li>logs a message at <code>Level.INFO</code></li>
	 * <li>tests for duplicate deployment
	 * <li>
	 * <li>creates and registers a service unit with this manager</li>
	 * <li>returns a <code>SUCCESS</code> task message string if the
	 * specified service unit is not already deployed</li>
	 * </ol>
	 *
	 * @see javax.jbi.component.ServiceUnitManager#deploy(java.lang.String,
	 *      java.lang.String)
	 */
	public String deploy(String serviceUnitName, String serviceUnitRootPath)
			throws DeploymentException {
		if (getLogger().isLoggable(Level.INFO)) {
			getLogger().info(
					"Deploying " + serviceUnitName + " to "
							+ serviceUnitRootPath);
		}
		if (isDeployed(serviceUnitName)) {
			// TODO should this return a FAILED task message instead?
			throw new DeploymentException("Service unit already deployed: "
					+ serviceUnitName);
		}

		ServiceUnit srvcUnit = ServicesDescriptor.parse(serviceUnitName, serviceUnitRootPath);
		getServiceUnits().register(serviceUnitName, srvcUnit);

		return createSuccessMessage(DEPLOY_TASK);
	}

	/**
	 * @see javax.jbi.component.ServiceUnitManager#init(java.lang.String,
	 *      java.lang.String)
	 */
	public void init(String serviceUnitName, String serviceUnitRootPath)
			throws DeploymentException {
		if (!isDeployed(serviceUnitName)) {
			/*
			 * JBI javadoc says to throw exception. Does not account for AS
			 * shutdown, since SUs are stored in memory and lost at that time.
			 * For now, adding service unit instance to registry.
			 */
			getServiceUnits().register(serviceUnitName,
			        ServicesDescriptor.parse(serviceUnitName, serviceUnitRootPath));
//					new DefaultServiceUnit(serviceUnitName, serviceUnitRootPath));
			// throw new DeploymentException("init failed for undeployed service
			// unit: "+ serviceUnitName);
		}

		/* START PROVISIONING */

		provisioningEndpointMap.put(serviceUnitName,
				new ArrayList<AspectSEEndpoint>());
		ServiceUnit srvcUnit = getServiceUnits().lookup(serviceUnitName);
		// parse jbi descriptor
//		srvcUnit.initDescriptor();
		EndpointInfo[] endpts = srvcUnit.getServices().getProvides();
		Map<EndpointInfo, ServiceEndpoint> activated = new HashMap<EndpointInfo, ServiceEndpoint>();
		Map<EndpointInfo, Endpoint> created = new HashMap<EndpointInfo, Endpoint>();
		try {
			for (EndpointInfo info : endpts) {
				// create endpoint
				Endpoint ept = getEndpointManager().getEndpointFactory()
						.createEndpoint(info, serviceUnitName,
								serviceUnitRootPath);

				registerProvisioningEndpointForStatusReporting(ept);

				// start provisioning
				if (info.isProvides()) {
					// activate with component context
					ServiceEndpoint srvcEPT = getComponentContext()
							.activateEndpoint(info.getServiceName(),
									info.getEndpointName());
					activated.put(info, srvcEPT);
				}

				created.put(info, ept);
				AspectSEEndpoint aept = (AspectSEEndpoint) ept;

				setComponentContextForAspectSEEndpoints(aept);

				registerMbeanForAspectSEEndpoints(aept);
				provisioningEndpointMap.get(serviceUnitName).add(aept);

				registerConsumingEndpointsForStatusReporting(aept);

				// TODO the endpoint may have a list of invokes, also take care
				// of the invokes, at this point assume that the provisioning
				// end point has only one invoke

				if (aept.getInvoke() != null) {

					created.put(aept.getInvoke().getInfo(), aept.getInvoke());
				}
			}
		} catch (Exception e) {
			String msg = "Service unit init failed: " + e.getMessage();
			mLogger.log(Level.SEVERE, msg, e);

			// rollback started endpoints - all or nothing
			for (EndpointInfo info : activated.keySet()) {
				try {
					if (mLogger.isLoggable(Level.INFO)) {
						mLogger.info("Deactivating endpoint during rollback: "
								+ String.valueOf(info));
					}
					ServiceEndpoint srvcEPT = activated.get(info);
					getComponentContext().deactivateEndpoint(srvcEPT);
				} catch (Exception e2) {
					mLogger.warning("Failed to deactivate endpoint: "
							+ e2.getMessage());
				}
			}

			throw new DeploymentException(msg, e);
		}

		// register created Endpoints with EndpointManager
		for (EndpointInfo info : created.keySet()) {
			// TODO do we "start" or "setActive" the endpoint here? after
			// register?
			getEndpointManager().registerEndpoint(info.getServiceName(),
					info.getEndpointName(), created.get(info));
		}
	}

	private void registerMbeanForAspectSEEndpoints(AspectSEEndpoint aept) {
		AspectSEEndpoint ept = aept;

		do {
			ept.registerMbean();
			ept = ept.getNext();

		} while (ept != null);
	}

	protected void shutdownAspectSEEndpoints(String serviceUnitName)
			throws InstanceNotFoundException, MBeanRegistrationException {
		ArrayList<AspectSEEndpoint> epts = provisioningEndpointMap
				.get(serviceUnitName);

		for (int i = 0; i < epts.size(); i++) {
			AspectSEEndpoint t = epts.get(i);
			do {
				t.shutdown();
				t = t.getNext();
			} while (t != null);

		}

	}

	private void setComponentContextForAspectSEEndpoints(AspectSEEndpoint aept) {
		AspectSEEndpoint ept = aept;
		ept.setComponentContext(this.getComponentContext());
		while (ept.getNext() != null) {
			ept = ept.getNext();
			ept.setComponentContext(this.getComponentContext());
		}

	}

	private void registerProvisioningEndpointForStatusReporting(Endpoint ept) {
		// register status info for provisioning

		EndpointInfo info = ept.getInfo();
		if (getStatusProvider() != null) {
			StatusReporting reporting = getStatusProvider().getStatusReporter();
			String statusId = getStatusProvider()
					.createProvisioningEndpointIdentifier(
							info.getServiceName(), info.getEndpointName());
			// previous line should be unnecessary, key by info not some
			// made up id
			reporting.addProvisioningEndpoint(statusId);
			// previous line should be: reporting.addEndpointInfo(info);
			EndpointStatus status = reporting.getEndpointStatus(statusId);
			// previous line should be: report.getEndpointStatus(info);
			// TODO copy the status into the crl
			// com.sun.jbi.component.endpoint.status.EndpointStatus
			((AspectSEEndpoint) ept).set_Status(status);
		}
	}

	private void registerConsumingEndpointsForStatusReporting(
			AspectSEEndpoint aept) {
		// register status info for consuming endpoints
		// TODO iterate over the list of invokes i.e consuming end
		// points and add them to status reporting

		AspectSEEndpoint lastEpt = aept;
		while (lastEpt.getNext() != null) {
			lastEpt = lastEpt.getNext();
		}

		if (getStatusProvider() != null && lastEpt.getInvoke() != null) {

			for (int i = 0; i < lastEpt.getInvokes().size(); i++) {

				AspectSEEndpoint cept = lastEpt.getInvokes().get(i);
				EndpointInfo cinfo = cept.getInfo();

				StatusReporting reporting = getStatusProvider()
						.getStatusReporter();
				String statusId = getStatusProvider()
						.createConsumingEndpointIdentifier(
								cinfo.getServiceName(), cinfo.getEndpointName());
				// previous line should be unnecessary, key by info not some
				// made up id
				reporting.addConsumingEndpoint(statusId);
				// previous line should be: reporting.addEndpointInfo(info);
				EndpointStatus status = reporting.getEndpointStatus(statusId);
				// previous line should be: report.getEndpointStatus(info);
				// TODO copy the status into the crl
				// com.sun.jbi.component.endpoint.status.EndpointStatus
				cept.set_Status(status);

			}
		}
	}

	/** @see javax.jbi.component.ServiceUnitManager#shutDown(java.lang.String) */
	public void shutDown(String serviceUnitName) throws DeploymentException {
		if (!isDeployed(serviceUnitName)) {
			throw new DeploymentException(
					"shutDown failed for undeployed service unit: "
							+ serviceUnitName);
		}

		/* STOP PROVISIONING */
		ServiceUnit srvcUnit = getServiceUnits().lookup(serviceUnitName);
		EndpointInfo[] endpts = srvcUnit.getServices().getEndpoints();

		for (EndpointInfo info : endpts) {
			try {
				if (getStatusProvider() != null) {
					StatusReporting reporting = getStatusProvider()
							.getStatusReporter();
					// reporting.removeEndpoint(info);
					/*
					 * Following code is temporary until StatusProvider is
					 * refactored
					 */
					String statusId = null;
					if (info.isProvides()) {
						statusId = getStatusProvider()
								.createProvisioningEndpointIdentifier(
										info.getServiceName(),
										info.getEndpointName());
						reporting
								.removeProvisioningEndpoints(new String[] { statusId });
					} else {
						statusId = getStatusProvider()
								.createConsumingEndpointIdentifier(
										info.getServiceName(),
										info.getEndpointName());
						reporting
								.removeConsumingEndpoints(new String[] { statusId });
					}
				}

				Endpoint ept = getEndpointManager().removeEndpoint(
						info.getServiceName(), info.getEndpointName(),
						info.isProvides());
				// if (ept.getInfo().isProvides()) ept.stop();
				ServiceEndpoint srvcEPT = getComponentContext().getEndpoint(
						info.getServiceName(), info.getEndpointName());
				getComponentContext().deactivateEndpoint(srvcEPT);
			} catch (Exception e) {

			}
		}
	}

	/** @see javax.jbi.component.ServiceUnitManager#start(java.lang.String) */
	public void start(String serviceUnitName) throws DeploymentException {
		if (!isDeployed(serviceUnitName)) {
			throw new DeploymentException(
					"start failed for undeployed service unit: "
							+ serviceUnitName);
		}

		/* START CONSUMING */
		try {
			ServiceUnit su = getServiceUnits().lookup(serviceUnitName);
			EndpointInfo[] endpts = su.getServices().getConsumes();
			for (EndpointInfo info : endpts) {
				Endpoint ept = getEndpointManager().lookupEndpoint(
						info.getServiceName(), info.getEndpointName(), false);
				// ept.start();
			}
		} catch (Exception ex) {
			String msg = "Service unit start failed for " + serviceUnitName
					+ ": " + ex.getMessage();
			getLogger().log(Level.SEVERE, msg, ex);
			throw new DeploymentException(msg, ex);
		}
	}

	/** @see javax.jbi.component.ServiceUnitManager#stop(java.lang.String) */
	public void stop(String serviceUnitName) throws DeploymentException {
		if (!isDeployed(serviceUnitName)) {
			throw new DeploymentException(
					"stop failed for undeployed service unit: "
							+ serviceUnitName);
		}

		/* STOP CONSUMING */
		try {
			ServiceUnit su = getServiceUnits().lookup(serviceUnitName);
			EndpointInfo[] endpts = su.getServices().getConsumes();
			for (EndpointInfo info : endpts) {
				Endpoint ept = getEndpointManager().lookupEndpoint(
						info.getServiceName(), info.getEndpointName(), false);
				// ept.stop();
			}
		} catch (Exception ex) {
			String msg = "Service unit stop failed for " + serviceUnitName
					+ ": " + ex.getMessage();
			getLogger().log(Level.SEVERE, msg, ex);
			throw new DeploymentException(msg, ex);
		}
	}

	/**
	 * This implementation logs undeployment at <code>Level.INFO</code> and
	 * returns a <code>SUCCESS</code> task message string.
	 *
	 * @see javax.jbi.component.ServiceUnitManager#undeploy(java.lang.String,
	 *      java.lang.String)
	 */
	public String undeploy(String serviceUnitName, String serviceUnitRootPath)
			throws DeploymentException {
		if (getLogger().isLoggable(Level.INFO)) {
			getLogger().info(
					"Undeploying " + serviceUnitName + " from "
							+ serviceUnitRootPath);
		}

		// TODO check state, use enum in ServiceUnit?
		getServiceUnits().remove(serviceUnitName);
		return createSuccessMessage(UNDEPLOY_TASK);
	}

	/* ********************** ACCESSORS ********************************* */

	protected ComponentContext getComponentContext() {
		return mComponentCtx;
	}

	protected EndpointManager getEndpointManager() {
		return mEndpoints;
	}

	protected Logger getLogger() {
		return mLogger;
	}

	protected EntryRegistry<String, ServiceUnit> getServiceUnits() {
		return mServiceUnits;
	}

	public StatusProviderHelper getStatusProvider() {
		return mStatusProviderHelper;
	}

	public void setStatusProvider(StatusProviderHelper prov) {
		mStatusProviderHelper = prov;
	}

	/* ********************** UTILITIES ********************************* */

	/**
	 * Builds a standard <code>SUCCESS</code> task message per the JBI
	 * specification.
	 *
	 * @param task
	 *            The task name.
	 * @return An XML string representing a task message.
	 */
	protected String createSuccessMessage(String task) {
		TaskResultDetails details = new TaskResultDetails(task, // task id
				true, // succeeded
				MessageType.INFO, null, // no task-status-msg elements
				null); // no exception-info elements
		TaskResult taskResult = new TaskResult(getComponentContext()
				.getComponentName(), details);
		return TaskXmlWriter.toXml(taskResult);
	}

	/**
	 * Builds a standard <code>FAILED</code> task message containing one
	 * <code>msg-loc-info</code> element per the JBI specification.
	 *
	 * @param task
	 *            The task name.
	 * @param locToken
	 *            The message key for looking up localized text for the message.
	 * @param locMessage
	 *            The default message, using {@link java.text.MessageFormat}
	 *            patterns.
	 * @param locParams
	 *            An array of parameters, may be <code>null</code>.
	 * @return An XML string representing a task message.
	 */
	protected String createExceptionMessage(String task, Throwable exception,
			String locToken, String locMessage, Object... locParams) {
		MsgLocInfo locInfo = new MsgLocInfo(locToken, locMessage, locParams);
		ExceptionInfo excInfo = new ExceptionInfo(1, locInfo, exception);
		TaskResultDetails details = new TaskResultDetails(task, // task id
				false, // failed
				MessageType.ERROR, null, // msg-loc-info used for
				// exception-info
				new ExceptionInfo[] { excInfo });
		TaskResult taskResult = new TaskResult(getComponentContext()
				.getComponentName(), details);
		return TaskXmlWriter.toXml(taskResult);
	}

	/**
	 * Builds a standard <code>FAILED</code> task message containing one
	 * <code>msg-loc-info</code> element per the JBI specification.
	 *
	 * @param task
	 *            The task name.
	 * @param locToken
	 *            The message key for looking up localized text for the message.
	 * @param locMessage
	 *            The default message, using {@link java.text.MessageFormat}
	 *            patterns.
	 * @param locParams
	 *            An array of parameters, may be <code>null</code>.
	 * @return An XML string representing a task message.
	 */
	protected String createFailedMessage(String task, String locToken,
			String locMessage, Object... locParams) {
		MsgLocInfo locInfo = new MsgLocInfo(locToken, locMessage, locParams);
		TaskResultDetails details = new TaskResultDetails(task, // task id
				false, // failed
				MessageType.WARNING, new MsgLocInfo[] { locInfo }, null); // no
		// exception-info
		// elements
		TaskResult taskResult = new TaskResult(getComponentContext()
				.getComponentName(), details);
		return TaskXmlWriter.toXml(taskResult);
	}

	/**
	 * Returns <code>true</code> if the specified service unit is deployed.
	 *
	 * @param serviceUnitName
	 *            The name of the service unit.
	 * @return <code>true</code> if the specified service unit is deployed.
	 */
	protected boolean isDeployed(String serviceUnitName) {
		return getServiceUnits().containsKey(serviceUnitName);
	}

}
