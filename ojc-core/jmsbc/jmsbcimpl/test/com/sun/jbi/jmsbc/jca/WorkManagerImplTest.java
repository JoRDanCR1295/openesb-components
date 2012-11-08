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
 * @(#)WorkManagerImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jca;

import javax.resource.spi.work.Work;
import javax.resource.spi.work.WorkException;

import org.jmock.core.*;
import org.jmock.*;


/**
 *
 * Unit tests for WorkManagerImpl class
 */
public class WorkManagerImplTest extends org.jmock.cglib.MockObjectTestCase {
    
    private class WorkImpl implements Work {
        private int val;
        private int wait;
        
        public WorkImpl(int startVal, int secondsWait) {
            val = startVal;
            wait = secondsWait;
        }
        
        public void run () {
            try {
                if (wait > 0) {
                    Thread.sleep(wait*1000);
                }
            } catch (InterruptedException ex) {
                ;
            }
            val++;            
        }
       
        public int getVal() {
            return val;
        }
        
        public void release() {
            
        }
    }
            
    public WorkManagerImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /*
    public void testDoWork() throws Exception {
        System.out.println("doWork");
        Work arg0 = null;
        WorkManagerImpl instance = new WorkManagerImpl();
        instance.doWork(arg0);
        fail("The test case is a prototype.");
    }
    
    public void testStartWork() throws Exception {
        System.out.println("startWork");
        Work arg0 = null;
        WorkManagerImpl instance = new WorkManagerImpl();
        long expResult = 0L;
        long result = instance.startWork(arg0);
        assertEquals(expResult, result);
        fail("The test case is a prototype.");
    }
    */
    
    public void testScheduleWork() throws Exception {
        System.out.println("scheduleWork - schedule non time consuming work tasks");
        WorkImpl workNoWait1 = new WorkImpl(1, 0);
        WorkImpl workNoWait2 = new WorkImpl(2, 0);
        WorkImpl workNoWait3 = new WorkImpl(3, 0);
        WorkManagerImpl instance = new WorkManagerImpl();
        instance.scheduleWork(workNoWait1);
        instance.scheduleWork(workNoWait2);
        instance.scheduleWork(workNoWait3);
        instance.shutdownAndWaitForWorkersToComplete(2L);
        assertTrue(workNoWait1.getVal()==2 &&
                   workNoWait2.getVal()==3 &&
                   workNoWait3.getVal()==4);
    }

    public void testShutdownAndWaitForWorkersToComplete() throws Exception {
        System.out.println("shutdownAndWaitForWorkersToComplete - schedule time consuming work tasks, shutdown, and wait for all to finish");
        WorkImpl workNoWait1 = new WorkImpl(1, 0);
        WorkImpl workNoWait2 = new WorkImpl(2, 0);
        WorkImpl workNoWait3 = new WorkImpl(3, 0);
        WorkImpl workWait1 = new WorkImpl(1, 1);
        WorkImpl workWait2 = new WorkImpl(2, 2);
        WorkImpl workWait3 = new WorkImpl(3, 3);
        long waitSeconds = 4L;
        WorkManagerImpl instance = new WorkManagerImpl();
        instance.scheduleWork(workNoWait1);
        instance.scheduleWork(workNoWait2);
        instance.scheduleWork(workNoWait3);
        instance.scheduleWork(workWait1);
        instance.scheduleWork(workWait2);
        instance.scheduleWork(workWait3);
        boolean expResult = true;
        boolean result = instance.shutdownAndWaitForWorkersToComplete(waitSeconds);
        assertEquals(expResult, result);
        assertTrue(workNoWait1.getVal()==2 &&
                   workNoWait2.getVal()==3 &&
                   workNoWait3.getVal()==4 &&
                   workWait1.getVal()==2 &&
                   workWait2.getVal()==3 &&
                   workWait3.getVal()==4);        
    }

    public void testShutdownAndWaitForWorkersToComplete2() throws Exception {
        System.out.println("shutdownAndWaitForWorkersToComplete2 - schedule time consuming work tasks, shutdown, and time out before all tasks gets to finish");
        WorkImpl workNoWait1 = new WorkImpl(1, 0);
        WorkImpl workNoWait2 = new WorkImpl(2, 0);
        WorkImpl workNoWait3 = new WorkImpl(3, 0);
        WorkImpl workWait1 = new WorkImpl(1, 2);
        WorkImpl workWait2 = new WorkImpl(2, 3);
        WorkImpl workWait3 = new WorkImpl(3, 4);
        long waitSeconds = 2L;
        WorkManagerImpl instance = new WorkManagerImpl();
        instance.scheduleWork(workNoWait1);
        instance.scheduleWork(workNoWait2);
        instance.scheduleWork(workNoWait3);
        instance.scheduleWork(workWait1);
        instance.scheduleWork(workWait2);
        instance.scheduleWork(workWait3);
        boolean expResult = false;
        boolean result = instance.shutdownAndWaitForWorkersToComplete(waitSeconds);
        assertEquals(expResult, result);        
    }

    public void testShutdownAndWaitForWorkersToComplete3() throws Exception {
        System.out.println("shutdownAndWaitForWorkersToComplete3 - schedule time consuming work tasks, shutdown, and ensure additional work submit requests are denied");
        WorkImpl workNoWait1 = new WorkImpl(1, 0);
        WorkImpl workNoWait2 = new WorkImpl(2, 0);
        WorkImpl workNoWait3 = new WorkImpl(3, 0);
        WorkManagerImpl instance = new WorkManagerImpl();
        instance.scheduleWork(workNoWait1);
        instance.scheduleWork(workNoWait2);
        instance.scheduleWork(workNoWait3);
        long waitSeconds = 2L;
        boolean expResult = true;
        boolean result = instance.shutdownAndWaitForWorkersToComplete(waitSeconds);
        assertEquals(expResult, result);
        WorkImpl workNoWait4 = new WorkImpl(4, 0);
        try {
            instance.scheduleWork(workNoWait4);
        } catch (WorkException ex) {
            assertTrue(workNoWait4.getVal()==4);
        }
    }
    
}
