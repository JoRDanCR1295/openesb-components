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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;

import org.netbeans.enterprise.bpel.troubleticketwrapper.AssingmentsLoad;
import org.netbeans.enterprise.bpel.troubleticketwrapper.TestOptionsType;

/**
 * @author mbhasin
 *
 */
public abstract class MessageSender extends Thread {
    
    protected Driver driver;

    private int messageId = 1;
    
    List testOptionsMessagePool;
    
    Object mutex = new Object();
    
    /**
     * @throws Exception
     */
    public abstract void runTest(int threadId) throws Exception;
    
    /**
     * @return
     */
    protected int getMessageId() {
        synchronized (mutex) {
            return messageId++;
        }
    }
    
    protected void seedTestOptionMessages() throws Exception {
        this.testOptionsMessagePool = new ArrayList();

        int count = driver.getThreads();
        
        for(int i =0; i < count; i++) {
            testOptionsMessagePool.add(createTestOptionsMessage());
        }
    }

    protected TestOptionsType getTestOptionsTypeMessage(int threadId) {
        return(TestOptionsType) testOptionsMessagePool.get(threadId);
    }
    
    protected void setMessageId(TestOptionsType testOptionsMessage) {
        // the getMessageId method must be serialized.
        testOptionsMessage.setMessageId(getMessageId());
    }
    
    /**
     * 
     */
    public void createExcutorServiceAndsubmitWorkers() {
        
        int threads = driver.getThreads();
        int iterations = driver.getIterations();
        
        ExecutorService es = Executors.newFixedThreadPool(threads);
        Worker workerArray[] = new Worker[threads];
        for (int i = 0; i < threads; i++) {
            Worker runObj = new Worker(this, i);
            workerArray[i] = runObj;
            es.submit(runObj);
        }
        es.shutdown();
        try {
            es.awaitTermination(5760l, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * @param messageId
     * @return
     * @throws Exception
     */
    protected TestOptionsType createTestOptionsMessage() throws Exception {
        
        TestOptionsType testOptions = new TestOptionsType();
        
        // we are using the messageid as correlation id, hence it will be 
        // put when the message is ready to be sent.
        //testOptions.setMessageId(messageId);
        
        AssingmentsLoad load = null;
        if (driver.getAssignmentLoad().equals("MEDIUM")) {
            load = AssingmentsLoad.MEDIUM;
        } else if (driver.getAssignmentLoad().equals("LARGE")) {
            throw new UnsupportedOperationException();
        }
        testOptions.setAssignActivityLoad(load);
        Duration duration = null;

        duration = DatatypeFactory.newInstance().newDuration(driver.getBpelWaitDuration());
        testOptions.setBpelWaitDuration(duration);

        duration = DatatypeFactory.newInstance().newDuration(driver.getPartnerWaitDuration());
        testOptions.setPartnerWaitDuration(duration);

        testOptions.setOutputReplyMessage(driver.isMSetOutput());
        return testOptions;
    }
}
