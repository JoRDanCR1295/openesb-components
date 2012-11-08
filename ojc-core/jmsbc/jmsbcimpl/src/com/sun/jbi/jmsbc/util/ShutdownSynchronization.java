package com.sun.jbi.jmsbc.util;

public class ShutdownSynchronization {
	
	private static SynchronizedStateChange sShutdownSynch = new SynchronizedStateChange();
	
	public static byte acquire(String key){
		return sShutdownSynch.acquire(key);
	}

	public static byte release(String key){
		return sShutdownSynch.release(key);
	}

	public static byte acquireChangeState(String key, long waitTime){
		return sShutdownSynch.acquireChangeState(key, waitTime);
	}

	public static byte releaseChangeState(String key){
		return sShutdownSynch.releaseChangeState(key);
	}
	
	public static void clear(){
		sShutdownSynch.clear();
	}
}
