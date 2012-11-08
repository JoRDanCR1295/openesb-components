package com.sun.jbi.jmsbc.util;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class BlockingQueueFactory {

	public static <T> BlockingQueue<T> getBlockedQueue(int initialCapacity){
		      BlockingQueue<T> queue = (BlockingQueue<T>) Proxy.newProxyInstance(BlockingQueue.class.getClassLoader(),
				                                           new Class[] { BlockingQueue.class },
				                                           new InvocationHandlerImpl<T>(initialCapacity));
		      return queue;
	}
	
	
	private static class InvocationHandlerImpl <T> implements InvocationHandler{
		private LinkedBlockingQueue<T> delegate = new LinkedBlockingQueue<T>(); 
		private int initialCapacity;

		public InvocationHandlerImpl(int i){
			initialCapacity = i;
		}
		public Object invoke(Object proxy, Method method, Object[] args)
				throws Throwable {
			
			//this offer is called from the thread pool
			if(method.getName().equals("offer") && delegate.size() >= initialCapacity){
				return Boolean.FALSE;
			}

			return method.invoke(delegate, args);
		}
		
	}
}
