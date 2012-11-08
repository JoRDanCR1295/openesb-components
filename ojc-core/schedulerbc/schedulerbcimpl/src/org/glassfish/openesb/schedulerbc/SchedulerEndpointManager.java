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
 * @(#)SchedulerEndpointManager.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.schedulerbc;

import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.glassfish.openesb.schedulerbc.domain.SchedulerEndPoint;
import org.glassfish.openesb.schedulerbc.domain.SchedulerServiceDef;
import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;

/**
 * The {@link EndpointManager} for Scheduler-SE.
 * @author sunsoabi_edwong
 */
public class SchedulerEndpointManager extends AbstractEndpointManager {

    private ComponentContext mCompCntxt;
    private SchedulerComponentManager mCompMgr;
    private Map<String, Date> mSuspendedEPs = new HashMap<String, Date>();
    private Map<String, Date> mActivatedEPs = new HashMap<String, Date>();
    
    /**
     * Constructs an <code>SchedulerEndpointManager</code>.
     */
    public SchedulerEndpointManager(SchedulerComponentManager compMgr,
            ComponentContext compCntxt) {
        super(compCntxt);

        SchedulerJobFactory.setEndpointManager(this);
        mCompMgr = compMgr;
        mCompCntxt = compCntxt;
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager
     * #createEndpoint(com.sun.jbi.common.descriptor.EndpointInfo,
     * com.sun.jbi.common.descriptor.ServiceUnit)
     */
    public Endpoint<SchedulerServiceDef> createEndpoint(EndpointInfo info,
            ServiceUnit srvcUnit) throws DeploymentException {
        I18n.info(log(), "SCHEDBC-5004: "                               //NOI18N
                + "Creating endpoint for {0}", info.toString());        //NOI18N
        return new SchedulerEndPoint(this, info, srvcUnit);
    }
    
    public ComponentContext getComponentContext() {
        return mCompCntxt;
    }
    
    public SchedulerComponentManager getComponentManager() {
        return mCompMgr;
    }

    public synchronized Map<String, Date> getSuspendedEndpointsMap() {
        Map<String, Date> m = new HashMap<String, Date>();
        m.putAll(mSuspendedEPs);
        return m;
    }

    public synchronized Map<String, Date> getActivatedEndpointsMap() {
        Map<String, Date> m = new HashMap<String, Date>();
        m.putAll(mActivatedEPs);
        return m;
    }

    public String toCompositeEndpointName(QName serviceQN, String endptName) {
        return serviceQN.toString() + "," + endptName;                  //NOI18N
    }

    public synchronized void initializeEndpointActive(String compEndptName) {
        mSuspendedEPs.remove(compEndptName);
        mActivatedEPs.put(compEndptName, new Date());
    }

    public void initializeEndpointActive(QName serviceQN, String endptName) {
        initializeEndpointActive(toCompositeEndpointName(serviceQN, endptName));
    }

    public synchronized boolean suspendEndpoint(String compEndptName) {
        Date dateActivated = mActivatedEPs.remove(compEndptName);
        if (dateActivated != null) {
            mSuspendedEPs.put(compEndptName, new Date());
        }
        return (dateActivated != null);
    }

    public boolean suspendEndpoint(QName serviceQN, String endptName) {
        return suspendEndpoint(toCompositeEndpointName(serviceQN, endptName));
    }

    public synchronized boolean activateEndpoint(String compEndptName) {
        Date dateSuspended = mSuspendedEPs.remove(compEndptName);
        if (dateSuspended != null) {
            mActivatedEPs.put(compEndptName, new Date());
        }
        return (dateSuspended != null);
    }

    public boolean activateEndpoint(QName serviceQN, String endptName) {
        return activateEndpoint(toCompositeEndpointName(serviceQN, endptName));
    }

    public synchronized boolean isEndpointSuspended(String compEndptName) {
        return mSuspendedEPs.containsKey(compEndptName);
    }

    public boolean isEndpointSuspended(QName serviceQN, String endptName) {
        return isEndpointSuspended(
                toCompositeEndpointName(serviceQN, endptName));
    }

    public synchronized boolean isEndpointActivated(String compEndptName) {
        return mActivatedEPs.containsKey(compEndptName);
    }

    public boolean isEndpointActivated(QName serviceQN, String endptName) {
        return isEndpointActivated(
                toCompositeEndpointName(serviceQN, endptName));
    }

    public synchronized String[] getSuspendedEndpoints() {
        return mSuspendedEPs.keySet().toArray(new String[0]);
    }

    public synchronized String[] getActivatedEndpoints() {
        return mActivatedEPs.keySet().toArray(new String[0]);
    }
}
