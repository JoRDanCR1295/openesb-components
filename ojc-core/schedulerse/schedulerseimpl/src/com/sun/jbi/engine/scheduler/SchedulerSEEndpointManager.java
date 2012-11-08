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
 * @(#)SchedulerSEEndpointManager.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.scheduler;

import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;
import com.sun.jbi.engine.scheduler.domain.SchedulerEndPoint;
import com.sun.jbi.engine.scheduler.domain.SchedulerServiceDef;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;

/**
 * The {@link EndpointManager} for Scheduler-SE.
 * @author sunsoabi_edwong
 */
public class SchedulerSEEndpointManager extends AbstractEndpointManager {

    private ComponentContext mCompCntxt;
    private SchedulerSEComponentManager mCompMgr;
    private Logger mLogger;
    
    /**
     * Constructs an <code>SchedulerSEEndpointManager</code>.
     */
    public SchedulerSEEndpointManager(SchedulerSEComponentManager compMgr,
            ComponentContext compCntxt) {
        super(compCntxt);
        
        mCompMgr = compMgr;
        mCompCntxt = compCntxt;
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager
     * #createEndpoint(com.sun.jbi.common.descriptor.EndpointInfo,
     * com.sun.jbi.common.descriptor.ServiceUnit)
     */
    public Endpoint<SchedulerServiceDef> createEndpoint(EndpointInfo info,
            ServiceUnit srvcUnit) throws DeploymentException {
        // TODO create Scheduler-SE endpoint and change template type in signature
        getLogger().info("Creating EndPoint for: " + info.toString());
        return new SchedulerEndPoint(this, info, srvcUnit);
    }
    
    public ComponentContext getComponentContext() {
        return mCompCntxt;
    }
    
    public SchedulerSEComponentManager getComponentManager() {
        return mCompMgr;
    }
    
    public Logger getLogger() {
        if (null == mLogger) {
            mLogger = Util.getLogger(mCompCntxt, getClass().getName());
        }
        return mLogger;
    }
}
