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
 * @(#)CorrelationUtility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import java.util.Properties;

import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;

/**
 * Persistence utility class for correlation unit tests.
 * 
 * @author Kevan Simpson
 */
public class CorrelationUtility extends UtilityClass {
    public void initiateBPInstanceForCorrelation(Properties props, 
                                                 Engine eng, 
                                                 DeploymentBindings deplBindings) 
            throws Exception {
        // initiate process
        initiateBPInstance(props, eng, deplBindings);
        // send message for second receive
        sendMessage("CORRELATING_", props, eng, deplBindings);
    }

    public void recoverForCorrelation(Properties props, 
                                      Engine eng, 
                                      DeploymentBindings deplBindings) 
            throws Exception {
        // initiate recovery
        recover(props, eng, deplBindings);
        // send message for second receive
        sendMessage("CORRELATING_", props, eng, deplBindings);
    }

    public void associateCorrelateChannel(Properties props, 
                                          Engine eng, 
                                          DeploymentBindings deplBindings) throws Exception {
        eng.setOutChannel(new EngineChannelSimulatorAdaptor());
    }
    
    public void associateRecInvOnMsgCorrelateChannel(Properties props,
                                                     Engine eng, 
                                                     DeploymentBindings deplBindings) 
            throws Exception {
        associate2WayInvokeChannel(props, eng, deplBindings);
    }

    public void initiateBPInstanceForFlowCorrelation(Properties props, 
                                                     Engine eng, 
                                                     DeploymentBindings deplBindings) 
            throws Exception {
        // initiate process
        initiateBPInstance(props, eng, deplBindings);
        // send message for second receive
        sendMessage("Pick2_", props, eng, deplBindings);
        sendMessage("Pick3_", props, eng, deplBindings);
        sendMessage("Pick4a_", props, eng, deplBindings);
        sendMessage("Pick4b_", props, eng, deplBindings);
    }
    
    public void recoverForFlowCorrelation(Properties props, 
                                          Engine eng, 
                                          DeploymentBindings deplBindings) 
            throws Exception {
        // initiate recovery
        recover(props, eng, deplBindings);
        // send message for second receive
        sendMessage("Pick2_", props, eng, deplBindings);
        sendMessage("Pick3_", props, eng, deplBindings);
        sendMessage("Pick4a_", props, eng, deplBindings);
        sendMessage("Pick4b_", props, eng, deplBindings);
    }
}
