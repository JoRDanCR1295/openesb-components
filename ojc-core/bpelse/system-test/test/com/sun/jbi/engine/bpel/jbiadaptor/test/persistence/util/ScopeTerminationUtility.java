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

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence.util;

import java.rmi.server.UID;
import java.util.Properties;

import javax.transaction.Transaction;
import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;
import com.sun.jbi.engine.bpel.jbiadaptor.test.persistence.UtilityClass;

/**
 * Persistence utility class for Scope Termination tests
 * 
 * @author Prashant Bhagat
 */
public class ScopeTerminationUtility extends UtilityClass {

	String testMode;
	int crashpoint;
	
	public void runWhileTerminationInProcessTest(final Properties props, final Engine eng, 
			DeploymentBindings deplBindings) throws Exception {
		
		crashpoint = Integer.valueOf(System.getProperty("PERSIST.CRASHPOINT")).intValue();
        testMode = System.getProperty("TEST.MODE");
        System.out.println("crashpoint: " + crashpoint);
        System.out.println("testMode: " + testMode);        
        if(testMode.equals("PERSIST")) {
    		initiateBPInstance(props, eng, deplBindings);        	
        	Thread.sleep(1100);
        	eng.process();
        	Thread.sleep(6100);
        	eng.process();
        } else if(testMode.equals("CRASH")) {
    		initiateBPInstance(props, eng, deplBindings);
        	if(crashpoint == 3) {
                Thread.sleep(1100);
                eng.process();
        	} else if(crashpoint == 4) {
                Thread.sleep(1100);
                eng.process();
            	Thread.sleep(6100);
                eng.process();            	
        	}
        } else if(testMode.equals("RECOVER")) {
            Thread.sleep(eng.getEngineExpiryInterval() + 1000); // Engine Expiration value for the persistence tests is 0,         	
        	if(crashpoint == 3) {
                eng.process();
            	Thread.sleep(6100);
            	eng.process();          
        		//System.out.println("time7 " + (System.currentTimeMillis())/1000);
        	} else if(crashpoint == 4) {
            	eng.process();
        	}
        } 
	}
	
    public void associateEngineChannelForReceiveTerminationInScope(final Properties props, 
    		final Engine eng, final DeploymentBindings deplBindings) 
    throws Exception {
    	final InComingEventModel model = associateChannel(props, deplBindings);
    	EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
    		public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, 
    				QName operation, boolean oneWay, RBPELProcess process) {
    			return sendFaultFor2WayInvoke(props, msgContainer, model, eng);
    		}

    		public void reply(MessageContainer msgContainer) {
    			acceptMessageForTest(props, msgContainer, model, eng, msgContainer.getId());
    		}
    	};
    	eng.setOutChannel(channel);        
    }
}
