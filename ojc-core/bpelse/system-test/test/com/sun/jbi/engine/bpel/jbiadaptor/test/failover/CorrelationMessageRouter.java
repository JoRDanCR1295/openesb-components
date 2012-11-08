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
 * @(#)CorrelationMessageRouter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;


public class CorrelationMessageRouter extends Thread {
    
    LinkedList mMessages = new LinkedList();
    List mEngines = new ArrayList();

    static Random generator = new Random();
    static int engineCursor = 0;
    
    /**
     * @param engine
     */
    public void addEngine (EngineSimulator engine) {
        this.mEngines.add(engine);
    }

    /**
     * @param message
     */
    public void addMessage (JBIMessageWrapper message) {
        this.mMessages.addLast(message);
    }
    
    public void run() {
        for (int j = 0; j < mMessages.size(); j++) {
            JBIMessageWrapper message = (JBIMessageWrapper) mMessages.get(j);

            try {
                sendMessage(message);
            } catch (Exception e) {
                e.printStackTrace();
                System.exit(1);
            }
        }
    }

    /**
     * In order to test the correlation, the messages will be sent to different engines
     * 
     * @param message
     */
    private void sendMessage(JBIMessageWrapper message) throws Exception {
        EngineSimulator engine = null;
        
        engine = message.getEngine();
        
        if (engine == null) {
            // if no engine is specified for the message, pick the engines 
            // in round-robin order and send the message to it.
            
            int enginesCount = mEngines.size();
            if ((engineCursor + 1) > enginesCount) {
                engineCursor = 0;
            }
            
            // To send random messages across the appservers. not implemented
            /*int randomIndex = 0;
             randomIndex = generator.nextInt(enginesCount);
             System.out.println("Selected random number" + randomIndex);
             int engineIndex = randomIndex;
             EngineSimulatorForCorrelation engine = (EngineSimulatorForCorrelation) engines.get(engineIndex);*/
            
            engine = (EngineSimulator) mEngines.get(engineCursor++);
        }
        
        if (engine != null) {
            engine.sendMessage(message);
        } else {
            throw new RuntimeException("Engine Not initialized..");
        }
    }
}
