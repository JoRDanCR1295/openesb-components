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
 * @(#)Cache.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 
 * @author graj
 */
public class Cache<K, V> extends LinkedHashMap<K, V> implements Serializable {
    static final int DEFAULT_INITIAL_CAPACITY = 1000;

    static final int DEFAULT_MAXIMUM_ENTRIES = 100;

    static final float DEFAULT_LOAD_FACTOR = 0.75f;

    private CacheStrategyType cachingStrategy;

    private int maximumEntries;

    // /////////////////////////////////////////
    // -- FirstInFistOut Cache Constructors --
    // /////////////////////////////////////////
    /**
     * Creates a new instance of Cache instance with a default capacity (1000)
     * and load factor (0.75) and FIFO caching
     */
    public Cache() {
        super(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR, true);
        this.maximumEntries = DEFAULT_MAXIMUM_ENTRIES;
        this.cachingStrategy = CacheStrategyType.FirstInFirstOut;
    }

    /**
     * Constructs a new instance of Cache instance with the specified initial
     * capacity, load factor and ordering mode and FIFO caching
     */
    public Cache(int initialCapacity, float loadFactor, boolean accessOrder,
            int maximumEntries) {
        super(initialCapacity, loadFactor, accessOrder);
        this.maximumEntries = maximumEntries;
        this.cachingStrategy = CacheStrategyType.FirstInFirstOut;
    }

    /**
     * Constructs a new instance of Cache instance with with a default capacity
     * (1000), default load factor (0.75), ordering mode and maximum entries and
     * FIFO caching
     */
    public Cache(boolean accessOrder, int maximumEntries) {
        this(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR, accessOrder,
                maximumEntries);
    }

    // /////////////////////////////////////////
    // -- Generic Cache Constructors --
    // /////////////////////////////////////////
    /**
     * Constructs a new instance of Cache instance with the specified initial
     * capacity, load factor, ordering mode and GenericCache
     */
    public Cache(int initialCapacity, float loadFactor, boolean accessOrder) {
        super(initialCapacity, loadFactor, accessOrder);
        this.maximumEntries = DEFAULT_MAXIMUM_ENTRIES;
        this.cachingStrategy = CacheStrategyType.GenericCache;
    }

    // /////////////////////////////////////////
    // -- Overridden operations --
    // /////////////////////////////////////////
    /**
     * This method will implement a FIFO strategy since the base method is
     * over-ridden.
     */
    protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
        boolean result = false;
        if (true == this.cachingStrategy.equals(CacheStrategyType.GenericCache)) {
            result = super.removeEldestEntry(eldest);
        }
        if (true == this.cachingStrategy
                .equals(CacheStrategyType.FirstInFirstOut)) {
            result = size() > this.maximumEntries;
        }
        return result;
    }

    // /////////////////////////////////////////
    // -- Other operations --
    // /////////////////////////////////////////
    /**
     * 
     */
    public CacheStrategyType getCachingStrategy() {
        return cachingStrategy;
    }

    /**
     * 
     */
    public void setFirstInFirstOutCachingStrategy(int maximumNumberOfEntries) {
        this.maximumEntries = maximumNumberOfEntries;
        this.cachingStrategy = CacheStrategyType.FirstInFirstOut;
    }

    /**
     * 
     */
    public void setGenericCachingStrategy() {
        this.cachingStrategy = CacheStrategyType.GenericCache;
    }

    /**
     * 
     */
    public int getMaximumEntries() {
        return maximumEntries;
    }

    public void setMaximumEntries(int maximumEntries) {
        this.maximumEntries = maximumEntries;
    }

}
