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
 * @(#)AbstractPool.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.util;

import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Abstract implementation of a resource pool, where the templated type is the
 * type of pooled resource.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractPool<E> {
    private static final int DEFAULT_RESIZE_INCREMENT = 4;
    
    // for logging pool resizes, lazy-loaded
    private Logger mLogger = null;
    private int mMaxPoolSize, mResizeIncrement = DEFAULT_RESIZE_INCREMENT;
    private LinkedList<E> mPool;
   
    /**
     * Constructs a pool with a maximum resource count.
     * @param maxPoolSize The maximum number of resources in pool.
     */
    protected AbstractPool(int maxPoolSize) {
        this(maxPoolSize, true);
    }
    
    protected AbstractPool(int maxPoolSize, boolean initResources) {
        mMaxPoolSize = maxPoolSize;
        if (initResources) {
            init();
        }
    }
    
    /**
     * Empties the pool of resources and unitializes the pool.
     * <p>
     * <b>NOTE:</b> After this method is called, this pool is unusable until
     * its {@link #init()} method is called.
     */
    public void cleanup() {
        synchronized (mPool) {
            while (!mPool.isEmpty()) {
                E rsrc = mPool.remove();
                cleanup(rsrc);
            }
        }
    }
    
    /**
     * Initializes this pool by loading {@link #getMaxPoolSize()} resources.
     * <p>
     * <b>NOTE:</b> If this pool is already initialized, no action is taken.
     */
    public void init() {
        if (mPool == null) {
            mPool = new LinkedList<E>();
            resizePool(getMaxPoolSize());
        }
    }
    
    /**
     * Sets the maximum pool size, with a minimum size of 1.
     * @param newMaxPoolSize The maximum pool size, with a minimum size of 1.
     */
    public void resizePool(int newMaxPoolSize) {
        if (log().isLoggable(Level.CONFIG)) {
            log().config(I18n.loc(
                    "UTIL-4001: Resizing pool: new size={0}", 
                    String.valueOf(newMaxPoolSize)));
        }
        
        // ensure minimum size of 1
        if (newMaxPoolSize < 1) newMaxPoolSize = 1;
        
        synchronized(mPool) {
        	int size = getMaxPoolSize();
            //We need to add more resources
            if (newMaxPoolSize > size) {
                for (int i = 0, n = newMaxPoolSize - size; i < n; i++) {
                    mPool.addLast(createResource());
                }
            } 
            else if (newMaxPoolSize < size) {
                for (int i = 0, n = size - newMaxPoolSize; i < n; i++) {
                    if (mPool.size() > 0) {
                        cleanup(mPool.removeFirst());
                    }
                }
            }
            
            mMaxPoolSize = newMaxPoolSize;
        }
    }

    /**
     * Acquire a pooled resource, which should be {@link #release(Object) released}
     * after use.
     * @return a pooled resource.
     */
    public E acquire() { 
        synchronized(mPool) {
            if (mPool.isEmpty()) {
                resizePool(getMaxPoolSize() + getResizeIncrement());
            }
            
            return mPool.removeFirst(); // FIFO
        }
    }
    
    /**
     * Releases a pooled resource back to the pool.
     * @param resource The resource to release.
     */
    public void release(E resource) {
    	if (resource == null) return;
    	
        synchronized (mPool) {
            //This ensures that the pool size is maintained
            if (mPool.size() >= getMaxPoolSize()) {
                return;
            }
            
            mPool.addLast(resource);
        }        
    }

    protected void cleanup(E resource) {
        if (log().isLoggable(Level.FINEST)) {
            log().finest(I18n.format(
                    "UTIL-1003: Cleaning up resource: {0}", 
                    String.valueOf(resource)));
        }
    }
    
    protected int getMaxPoolSize() {
        return mMaxPoolSize;
    }
    
    protected int getResizeIncrement() {
        return mResizeIncrement;
    }
    
    protected void setResizeIncrement(int incr) {
        mResizeIncrement = (incr > 0) ? incr : DEFAULT_RESIZE_INCREMENT;
    }
    
    protected Logger log() {
        if (mLogger == null) {
            mLogger = Logger.getLogger(this.getClass().getName());
        }
        return mLogger;
    }
    
    /**
     * Creates a new resource for the pool.
     * @return a new resource for the pool.
     */
    protected abstract E createResource();
}
