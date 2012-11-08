package com.sun.jbi.jmsbc.util;

import junit.framework.Assert;
import junit.framework.TestCase;


public class SynchronizedStateChangeTest extends TestCase{

	private SynchronizedStateChange mSync = new SynchronizedStateChange();
	
    public void testAll() throws Exception {
    	String key = "key1";
    	mSync.clear();
    	Assert.assertEquals(mSync.acquire(key), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.acquire(key), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.release(key), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.release(key), SynchronizedStateChange.PASS);
    	
    	Assert.assertEquals(mSync.acquireChangeState(key,1000), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.acquireChangeState(key,1000), SynchronizedStateChange.CHANGE_IN_PROGRESS);
    	Assert.assertEquals(mSync.releaseChangeState(key), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.release(key), SynchronizedStateChange.FAIL);
    	
    	mSync.clear();
    	Assert.assertEquals(mSync.acquireChangeState(key,1000), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.acquire(key), SynchronizedStateChange.CHANGE_IN_PROGRESS);
    	Assert.assertEquals(mSync.acquireChangeState(key,1000), SynchronizedStateChange.CHANGE_IN_PROGRESS);
    	Assert.assertEquals(mSync.releaseChangeState(key), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.acquire(key), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.release(key), SynchronizedStateChange.PASS);
    	Assert.assertEquals(mSync.release(key), SynchronizedStateChange.FAIL);
    }
}
