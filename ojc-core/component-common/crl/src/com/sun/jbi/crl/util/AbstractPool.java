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

package com.sun.jbi.crl.util;

import java.util.LinkedList;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractPool<E> {
    private int mMaxPoolSize;
    private LinkedList<E> mPool = new LinkedList<E>();
    
    public AbstractPool(int maxPoolSize) {
        mMaxPoolSize = maxPoolSize;
        resizePool(mMaxPoolSize);
    }
    
    protected void resizePool (int newMaxPoolSize) {
        synchronized(mPool) {
        	int size = mPool.size();
            //We need to add more resources
            if(newMaxPoolSize > size) {
                
                for (int i = 0, n = newMaxPoolSize - size; i < n; i++) {
                    mPool.addLast(createResource());
                }
                
            } 
            else if (newMaxPoolSize < size) {
                for (int i = 0, n = size - newMaxPoolSize; i < n; i++) {
                    if(mPool.size() > 0) {
                        mPool.removeFirst();
                    }
                }
            }
            
            mMaxPoolSize = newMaxPoolSize;
        }
    }

    public E acquire() { 
        synchronized(mPool) {
            return mPool.removeFirst(); // FIFO
        }
    }
    
    public void release(E resource) {
    	if (resource == null) return;
    	
        synchronized (mPool) {
            //This ensures that the pool size is maintained
            if (mPool.size() >= mMaxPoolSize) {
                return;
            }
            
            mPool.addLast(resource);
        }        
    }

    protected abstract E createResource();
}
