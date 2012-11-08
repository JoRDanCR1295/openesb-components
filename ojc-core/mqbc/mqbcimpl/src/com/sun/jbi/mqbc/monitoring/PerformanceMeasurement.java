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
 * @(#)$Id: PerformanceMeasurement.java,v 1.1 2008/09/24 22:59:15 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.monitoring;

import java.util.HashMap;
import java.util.Map;
import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanException;
import javax.management.MBeanRegistrationException;
import javax.management.NotCompliantMBeanException;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.TabularData;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import net.java.hulp.measure.Group;

/**
 * Performance instrumentation callback implementation.
 *
 * @author Noel.Ang@sun.com
 */
public class PerformanceMeasurement
        implements com.sun.jbi.eManager.provider.PerformanceMeasurement {

    /**
     * Retrieves the performance insturmentation measurement for the specified
     * endpoint.
     *
     * @param endpoint The endpoint name qualified by service name.
     *
     * @return An instance of TabularData which holds the performance
     *         instrumentation measurement for the specified endpoint.
     * @throws javax.management.openmbean.OpenDataException if there's an error
     * related to the openmbean data.
     * @throws javax.management.MBeanException if the endpoint does not exist
     * for the component.
     */
    public TabularData getPerformanceInstrumentationMeasurement(String endpoint)
            throws OpenDataException, MBeanException {
        return findGroup(endpoint).fetchData();
    }

    /**
     * Resets the performance measurements on the endpoint.
     *
     * @param endpoint The endpoint name qualified by service name.
     *
     * @throws javax.management.MBeanException if the endpoint does not exist
     * for the component.
     */
    public void clearPeformaceInstrumentationMeasurement(String endpoint)
            throws MBeanException {
        Group gr = findGroup(endpoint);
        gr.clearData();
    }

    /**
     * Registers this PerformanceMeasurement object with a StatusProvider
     * MBean.
     *
     * @param helper StatusProviderHelper with a pre-registered StatusProvider
     * MBean.
     *
     * @throws MBeanRegistrationException see {@link StatusProviderHelper#registerMBean(String[],
     * com.sun.jbi.eManager.provider.PerformanceMeasurement)}
     * @throws InstanceAlreadyExistsException see {@link
     * StatusProviderHelper#registerMBean(String[], com.sun.jbi.eManager.provider.PerformanceMeasurement)}
     * @throws NotCompliantMBeanException see {@link StatusProviderHelper#registerMBean(String[],
     * com.sun.jbi.eManager.provider.PerformanceMeasurement)}
     */
    public void register(StatusProviderHelper helper) throws
                                                      MBeanRegistrationException,
                                                      InstanceAlreadyExistsException,
                                                      NotCompliantMBeanException {
        helper.registerMBean(categories, this);
    }

    private Group findGroup(String endpoint) {
        Group group;

        synchronized (groups) {

            if (groups.containsKey(endpoint)) {
                group = groups.get(endpoint);
            } else {
                group = makeGroup(endpoint);
                groups.put(endpoint, group);
            }
            return group;

        }
    }

    private Group makeGroup(String endpoint) {
        Group group = Group.createGroup();
        group.addPattern(sourceFilter, endpoint, categoryFilter);
        return group;
    }

    private final Map<String, Group> groups = new HashMap<String, Group>();
    private final String[] categories = {"Normalization", "Denormalization"};
    private final String categoryFilter = "Normalization|Denormalization";
    private final String sourceFilter = ".*httpsoapbc.*";

}
