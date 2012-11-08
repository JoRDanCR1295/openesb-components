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

package org.glassfish.openesb.pojose.core.pool;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * Lazily create a objects. Objects are pooled making sure that pool size 
 * is never exceeds maximum pool size. 
 * Pool does not make the assumption that all the object will be returned.
 * Clients are guaranteed to get the object irrespective of the number of objects 
 * are in use or leased out.
 * 
 * @author gpatil
 */
public abstract class AbstractPool<E> {
    private volatile List<E> objPool = Collections.synchronizedList( new LinkedList<E>());
    private volatile int maxPoolSize = 10;
    
    protected AbstractPool(){
    }

    protected AbstractPool(int maxPoolSz){
        this.maxPoolSize = maxPoolSz;
    }
    
    protected synchronized E acquire(){
        E ret = null;
        if (objPool.size() > 0){
            ret = this.objPool.remove(0);
        } else {
            ret = create();
        }
        
        return ret;
    }
    
    /**
     * Object will not be pooled or saved when returned 
     * after maximum pool size is reached for re-use later.
     * 
     * @param o
     */
    protected synchronized void release(E o){
        if (this.objPool.size() < this.maxPoolSize){
            this.objPool.add(o);
        }
    }
    
    abstract protected E create();

    public int getMaxPoolSize() {
        return maxPoolSize;
    }

    /**
     * Maximum pool size, the maximum number of objects to be re-used at a time.
     * Any request for objects after reaching maximum size, new objects are 
     * continued to be created. Object will not be pooled or saved when returned 
     * after maximum pool size is reached for re-use later.
     * 
     * Max pool size can be zero, so that effect is same as not using the pool.
     * @param mxPoolSize
     */
    public synchronized void setMaxPoolSize(int mxPoolSize) {
        if (mxPoolSize > -1){
            int size = this.objPool.size();
            while (size > mxPoolSize){
                size--;
                if (size >= 0){
                    this.objPool.remove(size);
                }
            }
            this.maxPoolSize = mxPoolSize;
        }
    }
}

