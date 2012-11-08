package com.sun.jbi.jmsbc.util;

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

final public class Executor
{

    private ThreadPoolExecutor mPool;
    private ThreadPoolExecutor mDonePool;
    private static Executor ref = new Executor();

    private Executor()
    {
    }

    public static Executor getReference()
    {
        return ref;
    }

    public void execute(Runnable task)
    {
        mPool.execute(task);
    }

    public void executeDoneOrError(Runnable task)
    {
    	mDonePool.execute(task);
    }

    public void setMaxThreads(int i)
    {
    	setPoolSize(mPool ,i);
    	setPoolSize(mDonePool ,i);
    }

    public void start()
    {
		final BlockingQueue<Runnable> queue1 = BlockingQueueFactory.getBlockedQueue(20);
		mPool = new ThreadPoolExecutor(5, 16, 10, TimeUnit.SECONDS, queue1,
				new ThreadFactoryImpl("JMSBC"),
				new RejectedExecutionHandlerImpl(queue1));

		final BlockingQueue<Runnable> queue2 = BlockingQueueFactory.getBlockedQueue(20);
		mDonePool = new ThreadPoolExecutor(5, 16, 10, TimeUnit.SECONDS, queue2,
				new ThreadFactoryImpl("JMSBC-DONE-WAIT"),
				new RejectedExecutionHandlerImpl(queue2));
    }

    public void shutdown()
    {
        shutdownPool(mPool);
        shutdownPool(mDonePool);
    }

	private void shutdownPool(ThreadPoolExecutor pool) {
		pool.shutdown();
        pool = null;
	}

	private void setPoolSize(ThreadPoolExecutor pool, int i) {
		if(pool.getCorePoolSize() > i){
    		pool.setMaximumPoolSize(pool.getCorePoolSize());
    	} else{
    		pool.setMaximumPoolSize(i);
    	}
	}

    static class ThreadFactoryImpl implements ThreadFactory {
        static final AtomicInteger poolNumber = new AtomicInteger(1);
        final ThreadGroup group;
        final AtomicInteger threadNumber = new AtomicInteger(1);
        final String namePrefix;

        ThreadFactoryImpl(String prefix) {
            SecurityManager s = System.getSecurityManager();
            group = (s != null)? s.getThreadGroup() :
                                 Thread.currentThread().getThreadGroup();
            namePrefix = prefix + "-pool-" + 
                          poolNumber.getAndIncrement() + 
                         "-thread-";
        }

        public Thread newThread(Runnable r) {
            Thread t = new Thread(group, r, 
                                  namePrefix + threadNumber.getAndIncrement(),
                                  0);
            if (t.isDaemon())
                t.setDaemon(false);
            if (t.getPriority() != Thread.NORM_PRIORITY)
                t.setPriority(Thread.NORM_PRIORITY);
            return t;
        }
    }
	
    static class RejectedExecutionHandlerImpl implements RejectedExecutionHandler{
    	
    	BlockingQueue<Runnable> mQueue;
    	public RejectedExecutionHandlerImpl(BlockingQueue<Runnable> queue){
    		this.mQueue = queue;
    	}
		public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
			// add this to the queue. would be picked up by the next
			// available thread.
			// This way task would be never rejected.
			mQueue.add(r);
		}
    	
    }
}
