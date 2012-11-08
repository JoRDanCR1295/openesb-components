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

package com.sun.jbi.common.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Generic thread-safe registry, intended to be a mostly-read/rarely-update utility.
 * 
 * @author Kevan Simpson
 */
public class EntryRegistry<K, E> {
    private volatile Map<K, E> mReadMap = null;
    private Map<K, E> mWriteMap = null;
    
    /** Constructs an <code>EntryRegistry</code> instance. */
    public EntryRegistry() {
        clear();    // starts with fresh maps
    }
    
    /** Clears all entries from registry. */
    public void clear() {
        mReadMap = new HashMap<K, E>();
        mWriteMap = new HashMap<K, E>();
    }
    
    /**
     * Returns <code>true</code> if registry contains specified key.
     * @param key The specified key.
     * @return <code>true</code> if registry contains specified key, else <code>false</code>.
     */
    public boolean containsKey(K key) {
        return mReadMap.containsKey(key);
    }
    
    /**
     * Returns <code>true</code> if registry contains specified entry.
     * @param entry The specified entry.
     * @return <code>true</code> if registry contains specified entry, else <code>false</code>.
     */
    public boolean containsEntry(E entry) {
        return mReadMap.containsValue(entry);
    }
    
    /**
     * Returns the collection of entries in this registry.
     * @return the collection of entries in this registry.
     */
    public Collection<E> entries() {
        return mReadMap.values();
    }
    
    /**
     * Returns <code>true</code> if this registry contains no entries, else <code>false</code>.
     * @return <code>true</code> if this registry contains no entries, else <code>false</code>.
     */
    public boolean isEmpty() {
    	return mReadMap.isEmpty();
    }
    
    /**
     * Fetches the set of keys in this registry.
     * @return the set of keys in this registry.
     */
    public Set<K> keySet() {
        return mReadMap.keySet();
    }
    
    /**
     * Looks up a value in the registry.
     * @param key The specified key.
     * @return The value assigned to the specified key or <code>null</code>.
     */
    public E lookup(K key) {
        return mReadMap.get(key);
    }
    
    /**
     * Inserts all entries from the specified <code>Map</code> into this registry.
     * @param map A map of the same type as this registry.
     */
    public void registerAll(Map<K, E> map) {
        if (map != null) {
            synchronized (mWriteMap) {
                mWriteMap.putAll(map);
            }
            mReadMap = new HashMap<K, E>(mWriteMap);
        }
    }
    
    /**
     * Inserts the specified entry using the specified key.
     * @param key The specified key.
     * @param entry The specified entry.
     */
    public void register(K key, E entry) {
        if (key != null) {
            synchronized (mWriteMap) {
                mWriteMap.put(key, entry);
            }
            mReadMap = new HashMap<K, E>(mWriteMap);
        }
    }
    
    /** 
     * Removes the entry assigned to the specified key.
     * @param key The specified key.
     * @return the entry assigned to the specified key or <code>null</code>.
     */
    public E remove(K key) {
        E prev = null;
        synchronized (mWriteMap) {
            prev = mWriteMap.remove(key);
        }
        mReadMap = new HashMap<K, E>(mWriteMap);
        return prev;
    }

    public List<E> removeAll(K... keys) {
        List<E> list = new ArrayList<E>();
        if (keys != null) {
            synchronized (mWriteMap) {
                for (K k : keys) {
                    list.add(mWriteMap.remove(k));
                }
            }
            mReadMap = new HashMap<K, E>(mWriteMap);
        }
        
        return list;
    }
    
	/** @see java.lang.Object#toString() */
	public String toString() {
		return String.valueOf(mReadMap);
	}
}
