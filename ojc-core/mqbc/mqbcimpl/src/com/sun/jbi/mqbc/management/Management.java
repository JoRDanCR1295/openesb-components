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
 * @(#)$Id: Management.java,v 1.3 2008/12/10 21:54:53 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.management;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.management.MBeanException;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.mqbc.Endpoint;
import com.sun.jbi.mqbc.I18n;
import com.sun.jbi.mqbc.MQBindingDeployer;
import com.sun.jbi.mqbc.MQComponentContext;
import com.sun.jbi.mqbc.ServiceUnit;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;

/**
 * ManagementMBean implementation.
 *
 * @author Noel.Ang@sun.com
 * @see com.sun.jbi.mqbc.management.ManagementMBean
 */
public class Management implements ManagementMBean {

    public Management(MQComponentContext context) {
        this.context = context;
    }
    
    public void setServiceUnitManager(MQBindingDeployer deployer) {
        this.deployer = deployer;
    }

    /**
     * Suspend a endpoint.
     *
     * @param endpointName Endpoint identification. The identifier is not
     * necessarily a canonical name; it must be in the set of identifiers
     * returned by a preceding call to {@link #listActiveEndpoints()} or {@link
     * #listInactiveEndpoints()}.
     *
     * @throws javax.management.MBeanException if the endpoint cannot be
     * suspended for any reason.
     */
    public void suspend(String endpointName) throws MBeanException {
        if (endpointName == null || "".equals(endpointName.trim())) {
            throw new MBeanException(null,
                    I18n.msg("1204: Suspend aborted:"
                            + " unspecified endpoint name"
                    )
            );
        }
        Endpoint endpoint = findEndpoint(endpointName);
        if (endpoint == null) {
            throw new MBeanException(null,
                    I18n.msg("1205: Suspend failed:"
                            + " cannot resolve the specified endpoint {0}",
                            endpointName
                    )
            );
        }
        try {
            suspend(endpoint);
        } catch (JBIException e) {
            throw new MBeanException(e,
                    I18n.msg("1206: Suspend failed: {0}",
                            e.getLocalizedMessage()
                    )
            );
        }
    }

    /**
     * Resume a consuming endpoint.
     *
     * @param endpointName Endpoint identification. The identifier is not
     * necessarily a canonical name; it must be in the set of identifiers
     * returned by a preceding call to {@link #listActiveEndpoints()} or {@link
     * #listInactiveEndpoints()}.
     *
     * @throws javax.management.MBeanException if the endpoint cannot be resumed
     * for any reason.
     */
    public void resume(String endpointName) throws MBeanException {
        if (endpointName == null || "".equals(endpointName.trim())) {
            throw new MBeanException(null,
                    I18n.msg("1207: Resume aborted:"
                            + " unspecified endpoint name"
                    )
            );
        }
        Endpoint endpoint = findEndpoint(endpointName);
        if (endpoint == null) {
            throw new MBeanException(null,
                    I18n.msg("1208: Resume failed:"
                            + " cannot resolve the specified endpoint {0}",
                            endpointName
                    )
            );
        }
        try {
            resume(endpoint);
        } catch (JBIException e) {
            throw new MBeanException(e,
                    I18n.msg("1209: Resumed failed: {0}",
                            e.getLocalizedMessage()
                    )
            );
        }
    }

    /**
     * Indicates if the named endpoint is active (i.e., not suspended).
     *
     * @param endpointName Endpoint identification. The identifier is not
     * necessarily a canonical name; it must be in the set of identifiers
     * returned by a preceding call to {@link #listActiveEndpoints()} or {@link
     * #listInactiveEndpoints()}.
     *
     * @return true if the specified endpoint is active.
     * @throws javax.management.MBeanException if the status of the specified
     * endpoint cannot be obtained for any reason.
     */
    public boolean isEndpointActive(String endpointName) throws MBeanException {
        boolean isActive = false;
        Set<Endpoint> endpoints = getActiveEndpoints();
        for (Endpoint endpoint : endpoints) {
            if (endpointName.equals(makeEndpointIdentifier(endpoint))) {
                isActive = true;
                break;
            }
        }
        return isActive;
    }

    /**
     * Returns identifiers for all active managed endpoints.
     *
     * @return An array with zero or more endpoint identifiers.
     */
    public String[] listActiveEndpoints() {
        Collection<String> endpoints = createActiveEndpointNames();
        return endpoints.toArray(new String[endpoints.size()]);
    }

    /**
     * Returns identifiers for all suspended managed endpoints.
     *
     * @return An array with zero or more endpoint identifiers.
     */
    public String[] listInactiveEndpoints() {
        Collection<String> endpoints = createInactiveEndpointNames();
        return endpoints.toArray(new String[endpoints.size()]);
    }

    /**
     * Return the set of all non-suspended endpoints.
     *
     * @return Set of endpoints. The set will not be modified, and thus may be
     *         immutable.
     */
    private Set<Endpoint> getActiveEndpoints() {
        Set<Endpoint> endpoints = new HashSet<Endpoint>();
        MQBindingDeployer deployer = this.deployer;
        if (deployer != null) {
            for (ServiceUnit su : deployer.getServiceUnits()) {
                for (Endpoint endpoint : su.getEndpoints()) {
                    try {
                        if (!su.isSuspended(endpoint)) {
                            endpoints.add(endpoint);
                        }
                    } catch (JBIException e) {
                        mLogger.warning(NotificationEvent.SEVERITY_TYPE_MINOR,
                                NotificationEvent.OPERATIONAL_STATE_STARTED,
                                I18n.msg(
                                        "1200: Failed to obtain state of service endpoint {0},{1}"
                                                + " during attempted enumeration of active endpoints",
                                        endpoint.getServiceName().toString(),
                                        endpoint.getEndpointName()));
                    }
                }
            }
        }
        return endpoints;
    }

    /**
     * Return the set of all suspended endpoints.
     *
     * @return Set of endpoints. The set will not be modified, and thus may be
     *         immutable.
     */
    private Set<Endpoint> getInactiveEndpoints() {
        Set<Endpoint> endpoints = new HashSet<Endpoint>();
        MQBindingDeployer deployer = this.deployer;
        if (deployer != null) {
            for (ServiceUnit su : deployer.getServiceUnits()) {
                for (Endpoint endpoint : su.getEndpoints()) {
                    try {
                        if (su.isSuspended(endpoint)) {
                            endpoints.add(endpoint);
                        }
                    } catch (JBIException e) {
                        mLogger.warning(NotificationEvent.SEVERITY_TYPE_MINOR,
                                NotificationEvent.OPERATIONAL_STATE_STARTED,
                                I18n.msg(
                                        "1201: Failed to obtain state of service endpoint {0},{1}"
                                                + " during attempted enumeration of inactive endpoints",
                                        endpoint.getServiceName().toString(),
                                        endpoint.getEndpointName()));
                    }
                }
            }
        }
        return endpoints;
    }

    /**
     * Resume the indicated endpoint.
     *
     * @param endpoint The endpoint to resume.
     *
     * @throws javax.jbi.JBIException if the endpoint cannot be resumed for any
     * reason.
     */
    private void resume(Endpoint endpoint) throws JBIException {
        ServiceUnit su = findServiceUnit(endpoint);
        if (su == null) {
            throw new JBIException(I18n.msg(
                    "1202: Cannot resume unknown endpoint {0},{1}!",
                    endpoint.getServiceName().toString(),
                    endpoint.getEndpointName()));
        }
        su.resume(endpoint);
    }

    /**
     * Suspend the indicated endpoint.
     *
     * @param endpoint The endpoint to suspend.
     *
     * @throws JBIException if the endpoint cannot be resumed for any reason.
     */
    private void suspend(Endpoint endpoint) throws JBIException {
        ServiceUnit su = findServiceUnit(endpoint);
        if (su == null) {
            throw new JBIException(I18n.msg(
                    "1203: Cannot suspend unknown endpoint {0},{1}!",
                    endpoint.getServiceName().toString(),
                    endpoint.getEndpointName()));
        }
        su.suspend(endpoint);
    }

    /**
     * Generates unique names for a given endpoint.  The default implementation
     * is to create a name that is the concatenation of the fully qualified
     * service name, a comma, and the endpoint name.
     *
     * @param endpoint The endpoint for which to generate an identifier.
     *
     * @return A name for the endpoint.
     */
    private String makeEndpointIdentifier(Endpoint endpoint) {
        assert endpoint != null;
        return endpoint.getServiceName().toString() + ','
                + endpoint.getEndpointName();
    }

    private Collection<String> createActiveEndpointNames() {
        Set<Endpoint> endpoints = getActiveEndpoints();
        List<String> endpointNames = new LinkedList<String>();
        for (Endpoint endpoint : endpoints) {
            endpointNames.add(makeEndpointIdentifier(endpoint));
        }
        return endpointNames;
    }

    private Collection<String> createInactiveEndpointNames() {
        Set<Endpoint> endpoints = getInactiveEndpoints();
        List<String> endpointNames = new LinkedList<String>();
        for (Endpoint endpoint : endpoints) {
            endpointNames.add(makeEndpointIdentifier(endpoint));
        }
        return endpointNames;
    }

    private Endpoint findEndpoint(String endpointName) {
        assert endpointName != null;
        Endpoint target = null;
        Set<Endpoint> endpoints = getActiveEndpoints();
        endpoints.addAll(getInactiveEndpoints());
        for (Endpoint endpoint : endpoints) {
            if (endpointName.equals(makeEndpointIdentifier(endpoint))) {
                target = endpoint;
                break;
            }
        }
        return target;
    }

    private ServiceUnit findServiceUnit(Endpoint endpoint) {
        assert endpoint != null;

        ServiceUnit target = null;
        MQBindingDeployer deployer = this.deployer;
        if (deployer != null) {
            for (ServiceUnit su : deployer.getServiceUnits()) {
                for (Endpoint ep : su.getEndpoints()) {
                    if (ep.getEndpointName()
                            .equals(endpoint.getEndpointName())) {
                        if (ep.getServiceName()
                                .equals(endpoint.getServiceName())) {
                            target = su;
                            break;
                        }
                    }
                }
            }
        }
        return target;
    }

    private MQBindingDeployer deployer;
    private final MQComponentContext context;
    private volatile EventLogger mLogger = new EventLogger(Logger.getLogger(
            getClass().getName()), EventManagementFrameworkAlerter.alerter);
}
