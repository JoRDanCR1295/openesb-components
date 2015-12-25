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
 * @(#)TransformerPoolTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.util.HashSet;
import java.util.Set;
import junit.framework.*;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;

/**
 *
 * @author nang
 */
public class TransformerPoolTest extends TestCase {
    
    public TransformerPoolTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of retrieve method, of class com.sun.jbi.httpsoapbc.util.TransformerPool.
     */
    public void testRetrieve() throws Exception {
        TransformerPool instance = new TransformerPool();
        Transformer result = instance.retrieve();
        assert(result != null);
    }
    
    /**
     * Test of retrieve method, of class com.sun.jbi.httpsoapbc.util.TransformerPool.
     */
    public void testRetrieveMulti() throws Exception {
        // Call retrieve() concurrently
        // Each call must yield a unique Transformer instance
        final int THREAD_COUNT = 100;
        final TransformerPool POOL = new TransformerPool();
        Thread[] threads = new Thread[THREAD_COUNT];
        RetrievalRun[] runs = new RetrievalRun[THREAD_COUNT];
        Set<Transformer> transformers = new HashSet<Transformer>(THREAD_COUNT);
        
        for (int i = 0; i < THREAD_COUNT; ++i) {
            runs[i] = new RetrievalRun(POOL);
            threads[i] = new Thread(runs[i]);
        }
        for (int i = 0; i < THREAD_COUNT; ++i) {
            threads[i].start();
        }
        for (int i = 0; i < THREAD_COUNT; ++i) {
            threads[i].join();
            // add returns false if object already in the set
            assertTrue(transformers.add(runs[i].getTransformer()));
        }
    }

    /**
     * Test of relinquish method, of class com.sun.jbi.httpsoapbc.util.TransformerPool.
     */
    public void testRelinquish() {
        TransformerPool instance = new TransformerPool();
        Transformer transformer = null;
        try {
            transformer = instance.retrieve();
        } catch (TransformerConfigurationException ex) {
            fail("TransformerConfigurationException during transformer retrieval. "
                    + ex.getLocalizedMessage());
        }
        assertTrue(instance.relinquish(transformer));
        
        // null reference test
        assertFalse(instance.relinquish(null));
        
        // idempotence test
        assertFalse(instance.relinquish(transformer));
    }
    
    /**
     * Test of relinquish method, of class com.sun.jbi.httpsoapbc.util.TransformerPool.
     */
    public void testRelinquishMulti() throws Exception {
        // Call relinquish concurrently
        // Each call must succeed
        final int THREAD_COUNT = 100;
        final TransformerPool POOL = new TransformerPool();
        Thread[] threads = new Thread[THREAD_COUNT];
        RelinquishRun[] runs = new RelinquishRun[THREAD_COUNT];
        Set<Transformer> transformers = new HashSet<Transformer>(THREAD_COUNT);
        
        for (int i = 0; i < THREAD_COUNT; ++i) {
            runs[i] = new RelinquishRun(POOL, POOL.retrieve());
            threads[i] = new Thread(runs[i]);
        }
        for (int i = 0; i < THREAD_COUNT; ++i) {
            threads[i].start();
        }
        for (int i = 0; i < THREAD_COUNT; ++i) {
            threads[i].join();
            assertTrue(runs[i].isRelinquished());
        }
    }
    
    private class RetrievalRun implements Runnable {
        RetrievalRun(TransformerPool pool) {
            mPool = pool;
        }
        public void run() {
            try {
                mTransformer = mPool.retrieve();
            } catch (TransformerConfigurationException ex) {
                mTransformer = null;
            }
        }
        public Transformer getTransformer() {
            return mTransformer;
        }
        private TransformerPool mPool;
        private Transformer mTransformer;
    }
    
    private class RelinquishRun implements Runnable {
        RelinquishRun(TransformerPool pool, Transformer transformer) {
            mPool = pool;
            mTransformer = transformer;
            mResult = false;
        }
        public void run() {
            mResult = mPool.relinquish(mTransformer);
        }
        public boolean isRelinquished() {
            return mResult;
        }
        private TransformerPool mPool;
        private Transformer mTransformer;
        private boolean mResult;
    }
}
