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
 * @(#)EntryRegistry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.HashMap;
import java.util.Map;

/**
 * Generic thread-safe registry of JBI component configuration entries, 
 * intended to be a mostly-read/rarely-update utility.
 * 
 * @author Kevan Simpson
 */
public class EntryRegistry<K, E> {
    private volatile Map<K, E> mReadMap = null;
    private Map<K, E> mWriteMap = null;
    
    public EntryRegistry() {
        mReadMap = new HashMap<K, E>();
        mWriteMap = new HashMap<K, E>();
    }
    
    public E lookup(K key) {
        return mReadMap.get(key);
    }
    
    public void registerAll(Map<K, E> map) {
        // TODO is this needed?
        if (map != null) {
            synchronized (mWriteMap) {
                mWriteMap.putAll(map);
            }
            mReadMap = new HashMap<K, E>(mWriteMap);
        }
    }
    
    public void register(K key, E entry) {
        if (key != null) {
            synchronized (mWriteMap) {
                mWriteMap.put(key, entry);
            }
            mReadMap = new HashMap<K, E>(mWriteMap);
        }
    }
    
    public E remove(K key) {
        E prev = null;
        synchronized (mWriteMap) {
            prev = mWriteMap.remove(key);
        }
        mReadMap = new HashMap<K, E>(mWriteMap);
        return prev;
    }
}
