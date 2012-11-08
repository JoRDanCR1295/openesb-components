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
 * @(#)EngineChannel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package driver;

import java.util.ArrayList;
import java.util.List;

import org.netbeans.enterprise.bpel.troubleticketwrapper.TestOptionsType;

/**
 * @author mbhasin
 *
 */
class CorrelatedMesssageSenderThread extends MessageSender {

    /**
     * @param driver
     * @throws Exception 
     */
    public CorrelatedMesssageSenderThread(Driver driver) throws Exception {
        this.driver = driver;
        seedTestOptionMessages();
    }
    
    
    /* (non-Javadoc)
     * @see driver.MessageSender#runTest()
     */
    public void runTest(int threadId) throws Exception {

        TestOptionsType testOptionsMessage = getTestOptionsTypeMessage(threadId);

        int iterations = driver.getIterations();
        
        for (int i = 0; i < iterations; i++) {
            // set the message id, this is used for correlating with the first message.
            setMessageId(testOptionsMessage);
            
            // send the correlated message
            org.netbeans.enterprise.bpel.troubleticketwrapper.CorrelatedMessagePortType port1 = driver.getService().getCorrelatedPort1();
            // The message construction is done during class loading, to save processing
            port1.scalabilityTest1Operation(testOptionsMessage);
        }
    }
    
    public void run() {
        createExcutorServiceAndsubmitWorkers();
    }
}