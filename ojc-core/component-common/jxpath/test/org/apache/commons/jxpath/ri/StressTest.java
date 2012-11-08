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
 * @(#)StressTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri;

import junit.framework.TestCase;

import org.apache.commons.jxpath.JXPathContext;

/**
 * Test thread safety.
 *
 * @author Dmitri Plotnikov
 * @version  
 */

public class StressTest extends TestCase {
    
    private static final int THREAD_COUNT = 50;
    private static final int THREAD_DURATION = 1000;
    private static JXPathContext context;
    private static int count;
    private static Throwable exception;
        
    /**
     * Construct a new instance of this test case.
     *
     * @param name Name of the test case
     */
    public StressTest(String name) {
        super(name);
    }

    public void testThreads() throws Throwable {
        context = JXPathContext.newContext(null, new Double(100));
        Thread[] threadArray = new Thread[THREAD_COUNT];
        for (int i = 0; i < THREAD_COUNT; i++) {
            threadArray[i] = new Thread(new StressRunnable());
        }
        
        for (int i = 0; i < threadArray.length; i++) {
            threadArray[i].start();
        }

        for (int i = 0; i < threadArray.length; i++) {
            try {
                threadArray[i].join();
            }
            catch (InterruptedException e) {
                assertTrue("Interrupted", false);
            }
        }

        if (exception != null) {
            throw exception;
        }
        assertEquals("Test count", THREAD_COUNT * THREAD_DURATION, count);
    }    

    private final class StressRunnable implements Runnable {
        public void run() {
            for (int j = 0; j < THREAD_DURATION && exception == null; j++) {
                try { 
                    double random = 1 + Math.random();
                    double sum =
                        ((Double) context.getValue("/ + " + random))
                            .doubleValue();
                    assertEquals(100 + random, sum, 0.0001);
                    synchronized (context) {
                        count++;
                    }
                }                    
                catch (Throwable t) {
                    exception = t;
                }
            }
        }
    }
}
