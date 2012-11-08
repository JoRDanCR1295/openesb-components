/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
/*
 * Copyright 2007 Sun Microsystems, Inc. All Rights Reserved.  
 */

package com.sun.jbi.snmpengine.test;

import com.sun.jbi.snmpengine.impl.BatchQueue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import junit.framework.TestCase;

/**
 * @author fkieviet
 */
public class BatchQueueTest extends TestCase {

    /**
     * When the batchsize is a multiple of the items queued, it should process all 
     * items immediately
     * 
     * @throws Throwable
     */
    public void testFullBatches() throws Throwable {
        poolTest(100000, 3, 9, 0, 1, false);
    }

    /**
     * When the batchsize is not a multiple of the items queued, it should wait
     * MAXWAIT before processing
     * 
     * @throws Throwable
     */
    public void testHalfFullBatches() throws Throwable {
        poolTest(500, 3, 10, 0, 1, false);
    }
    
    /**
     * When the batchsize is not a multiple of the items queued, it should wait
     * MAXWAIT before processing
     * 
     * @throws Throwable
     */
    public void testSlowHalfFullBatches() throws Throwable {
        poolTest(10, 10, 50, 1, 1, false);
    }
    
    /**
     * Buffer with size 1 should work
     * 
     * @throws Throwable
     */
    public void testOne() throws Throwable {
        poolTest(10, 1, 50, 0, 1, false);
    }
    
    /**
     * When the batchsize is a multiple of the items queued, it should process all 
     * items immediately
     * 
     * @throws Throwable
     */
    public void testFullBatchesMT() throws Throwable {
        poolTest(100000, 3, 9, 50, 3, false);
    }

    /**
     * When the batchsize is not a multiple of the items queued, it should wait
     * MAXWAIT before processing
     * 
     * @throws Throwable
     */
    public void testHalfFullBatchesMT() throws Throwable {
        poolTest(500, 3, 10, 50, 3, false);
    }
    
    /**
     * When the batchsize is not a multiple of the items queued, it should wait
     * MAXWAIT before processing
     * 
     * @throws Throwable
     */
    public void testSlowHalfFullBatchesMT() throws Throwable {
        poolTest(10, 10, 50, 1, 3, false);
    }
    
    /**
     * Buffer with size 1 should work
     * 
     * @throws Throwable
     */
    public void testOneMT() throws Throwable {
        poolTest(10, 1, 50, 50, 3, false);
    }
    
    /**
     * When the batchsize is a multiple of the items queued, it should process all 
     * items immediately
     * 
     * @throws Throwable
     */
    public void testFullBatchesMTA() throws Throwable {
        poolTest(100000, 3, 9, 50, 3, true);
    }

    /**
     * When the batchsize is not a multiple of the items queued, it should wait
     * MAXWAIT before processing
     * 
     * @throws Throwable
     */
    public void testHalfFullBatchesMTA() throws Throwable {
        poolTest(500, 3, 10, 50, 3, true);
    }
    
    /**
     * When the batchsize is not a multiple of the items queued, it should wait
     * MAXWAIT before processing
     * 
     * @throws Throwable
     */
    public void testSlowHalfFullBatchesMTA() throws Throwable {
        poolTest(10, 10, 50, 1, 3, true);
    }
    
    /**
     * Buffer with size 1 should work
     * 
     * @throws Throwable
     */
    public void testOneMTA() throws Throwable {
        poolTest(10, 1, 50, 50, 3, true);
    }

    private class Concurrency {
        int v;
        int max;
        public synchronized void enter() {
            v++;
            if (v > max) {
                max = v;
            }
        }

        public synchronized void exit() {
            v--;
        }
        
        public synchronized int getMax() {
            return max;
        }
    }
    
    private void poolTest(final int maxWait, final int batchSize, final int nItemsToTest, 
        final long processingTime, int maxConcurrency, final boolean async) throws Throwable {
        
        ScheduledThreadPoolExecutor pool = new ScheduledThreadPoolExecutor(maxConcurrency * 2);
        final List<String> processed = Collections.synchronizedList(new ArrayList<String>());
        final Set<Integer> batches = Collections.synchronizedSet(new HashSet<Integer>());
        final Semaphore waiter = new Semaphore(0);
        final long startat = System.currentTimeMillis();
        final Concurrency concurrency = new Concurrency();
        BatchQueue<String> q = new BatchQueue<String>(batchSize, maxWait, pool, maxConcurrency, async) {
            @Override
            public String[] newArray(int size) {
                return new String[size];
            }

            @Override
            public void processBatch(String[] batch, int batchId) {
                for (int i = 0; i < batch.length; i++) {
                    System.out.printf("%d Proccesed %s in batch %d\r\n", 
                        (System.currentTimeMillis() - startat), batch[i], batchId);
                    processed.add(batch[i]);
                    concurrency.enter();
                    try {
                        Thread.sleep(processingTime);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    concurrency.exit();
                    batches.add(batchId);
                    waiter.release();
                }
                if (async) {
                    batchComplete(batchId);
                }
            }
        };
        
        for (int i = 0; i < nItemsToTest; i++) {
            q.add("X" + i);
        }
        
        waiter.tryAcquire(nItemsToTest, 100, TimeUnit.SECONDS);
        final long endat = System.currentTimeMillis();
        
        assertTrue(processed.size() == nItemsToTest);
        System.out.printf("batches: %d\r", batches.size());
        assertTrue(batches.size() == nItemsToTest / batchSize + (nItemsToTest % batchSize != 0 ? 1 : 0));
        if (nItemsToTest % batchSize != 0) {
            assertTrue(endat - startat >= maxWait);
        }
        
        assertEquals(maxConcurrency, concurrency.getMax());
        
        pool.shutdown();
    }
}
