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
 * @(#)XmlResourceProviderPoolImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.common;

import java.util.LinkedList;

import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;

public class XmlResourceProviderPoolImpl implements XmlResourceProviderPool {

    private int maxPoolSize;
    private LinkedList pool = new LinkedList();
    
    public XmlResourceProviderPoolImpl(int maxPoolSize) throws Exception {
        this.maxPoolSize = maxPoolSize;
        
        for (int i=0; i<maxPoolSize; i++) {
            pool.addLast(new XmlResourceProviderImpl());
        }
    }
    
    void resizePool (int newMaxPoolSize) throws Exception {
        synchronized(pool) {
            
            //We need to add more resources
            if(newMaxPoolSize > maxPoolSize) {
                
                for (int i=0; i < newMaxPoolSize-maxPoolSize; i++) {
                    pool.addLast(new XmlResourceProviderImpl());
                }
                
            } else if (newMaxPoolSize < maxPoolSize) {
                for (int i=0; i < maxPoolSize-newMaxPoolSize; i++) {
                    if(pool.size() > 0) {
                        pool.removeFirst();
                    }
                }
            }
            
            this.maxPoolSize = newMaxPoolSize;
        }
    }

    public XmlResourceProvider acquireXmlResourceProvider() { 
        synchronized(pool) {
            return (XmlResourceProvider)pool.removeFirst(); // FIFO
        }
    }
    
    public void releaseXmlResourceProvider(XmlResourceProvider resource) {
        synchronized (pool) {
            //This ensures that the pool size is maintained
            if (pool.size() >= maxPoolSize) {
                return;
            }
            
            pool.addLast(resource);
        }        
    }
}
