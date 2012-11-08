package com.sun.jbi.jmsbc.util;

import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

public class SynchronizedStateChange {
    private static final Messages mMessages =
        Messages.getMessages(SynchronizedStateChange.class);
    private static final Logger mLog =
        Messages.getLogger(SynchronizedStateChange.class);
    
    public static final byte PASS = 1;
    public static final byte FAIL = 2;
    public static final byte TIMEOUT = 3;
    public static final byte CHANGE_IN_PROGRESS = 4;
    
	
	private HashMap<String, DS> map = new HashMap<String, DS>();
	private Object lock = new Object();
	
	public byte acquire(String key){
		
		//Get the monitor first
		DS ds = null;
		synchronized (this) {
			ds = map.get(key);
			if(ds == null){
				ds = new DS();
				ds.count++;
				map.put(key, ds);
				return PASS;
			}
		}
		
		synchronized (ds) {
			if(ds.stateChangeInitiated)
				return CHANGE_IN_PROGRESS;
			ds.count++;
		}
		return PASS;
	}

	public byte release(String key){
		//Always get the monitor first
		DS ds = null;
		synchronized (this) {
			ds = map.get(key);
			if(ds == null){
				logWarning1(key);
				return FAIL;
			}
		}
		synchronized (ds) {
			if(ds.count == 0){
				logWarning1(key);
				return FAIL;
			}
			ds.count--;
			if(ds.stateChangeInitiated && ds.count == 0)
				ds.notify();
		}
		return PASS;
	}

	public byte acquireChangeState(String key, long waitTime){
		long startTime = System.currentTimeMillis();
		//Always get the monitor first
		DS ds = null;
		synchronized (this) {
			ds = map.get(key);
			if(ds == null){
				ds = new DS();
				ds.stateChangeInitiated = true;
				map.put(key, ds);
				return PASS;
			}
		}

		
		synchronized (ds) {
			if(ds.stateChangeInitiated)
				return CHANGE_IN_PROGRESS;
			
			ds.stateChangeInitiated = true;
			if(ds.count == 0){
				return PASS;
			}
			//wait for the notification
			long currentTime = System.currentTimeMillis();
			while(ds.count > 0 && ((currentTime - startTime) < waitTime)){
				waitTime = waitTime - (currentTime - startTime);
				startTime = currentTime;
				try {
					ds.wait(waitTime);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupted(); //clear the interrupted flag
				}
				currentTime = System.currentTimeMillis();
			}
			if(ds.count == 0)
				return PASS;
		}
		
		return TIMEOUT;

	}
	
	public byte releaseChangeState(String key){
		long startTime = System.currentTimeMillis();
		//Always get the monitor first
		DS ds = null;
		synchronized (this) {
			ds = map.get(key);
			if(ds == null){
				logWarning2(key);
				return FAIL;
			}
		}

		synchronized (ds) {
			if(!ds.stateChangeInitiated){
				logWarning2(key);
				return FAIL;
			}
			ds.stateChangeInitiated = false;
			assert_synch(ds.count == 0);
		}
		
		return PASS;

	}

	synchronized public void clear() {
		map.clear();
	}
	
	private void logWarning1(String key) {
		String errMSG = mMessages.getString(
				"JMSBC-W0701.AcquireReleaseNotCalledInProperOrder",
				new String[] { key });
		mLog.log(Level.WARNING, errMSG, new Exception("Stack trace"));
	}

	private void logWarning2(String key) {
		String errMSG = mMessages.getString(
				"JMSBC-W0702.AcquireReleaseNotCalledInProperOrder",
				new String[] { key });
		mLog.log(Level.WARNING, errMSG, new Exception("Stack trace"));
	}
	
	private void assert_synch(boolean flag){
		if(flag)
			return;
		String errMSG = mMessages.getString("JMSBC-W0703.AssertFailure");
		mLog.log(Level.WARNING, errMSG, new Exception("Stack trace"));
	}

	private static class DS{
		int count = 0;
		boolean stateChangeInitiated = false;
	}
}
