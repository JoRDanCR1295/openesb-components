/**
 * 
 */
package com.sun.jbi.httpsoapbc;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

/**
 * @author Sujit Biswas
 * 
 */
public class ThreadPoolQueueTest extends TestCase {

    /**
     * @throws java.lang.Exception
     */

    public void setUp() throws Exception {
    }

    /**
     * @throws java.lang.Exception
     */

    public void tearDown() throws Exception {
    }

    /**
     * 
     * trying to simulate the real life scenario
     * 
     * Test method for
     * {@link com.sun.jbi.httpsoapbc.ThreadPoolQueue#offer(java.lang.Runnable)}.
     * 
     * @throws Exception
     */

    public void testOfferRunnable() throws Exception {

	ThreadPoolQueue q = new ThreadPoolQueue();

	ThreadPoolExecutor ex = new ThreadPoolExecutor(5, 15, 600,
		TimeUnit.SECONDS, q);

	q.setThreadPoolExecutor(ex);

	for (int i = 0; i < 20; i++) {
	    ex.execute(new TestRunnable());
	    Thread.sleep(100);
	}

	//System.out.println(ex.getPoolSize());
	//System.out.println(ex.getMaximumPoolSize());
	//System.out.println(ex.getActiveCount());

	assertEquals(5, q.size());

    }

    /**
     * Test method for
     * {@link com.sun.jbi.httpsoapbc.ThreadPoolQueue#getThreadPoolExecutor()}.
     */

    public void testGetThreadPoolExecutor() {

    }

    private class TestRunnable implements Runnable {

	public void run() {
	    try {
		Thread.sleep(30000);
	    } catch (InterruptedException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	    }

	}

    }

}
