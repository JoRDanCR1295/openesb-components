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
 * @(#)RowSection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;


/**
 * RowSection.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class RowSection implements Comparable<RowSection> {
    private Object[] mData;
    private int mStart;
    private int mLen;
    
    public RowSection(Object[] data) {
        if (data == null) {
            mStart = 0;
            mLen = 0;
        } else {
            mStart = 0;
            mLen = data.length;
            mData = new Object[data.length];
            System.arraycopy(data, mStart, mData, mStart, mLen);
        }
    }
    
    public RowSection(Object[] data, int start, int len) {
        mData = new Object[start + len];
        System.arraycopy(data, start, mData, start, len);
        mStart = start;
        mLen = len;
    }
    
    public Object[] getData() {
        Object[] data = new Object[mStart + mLen];
        System.arraycopy(mData, mStart, data, mStart, mLen);
        return data;
    }
    
    public int getStart() {
        return mStart;
    }
    
    public int getLen() {
        return mLen;
    }
    
    public Object getData(int i) {
        return mData[mStart + i];
    }
    
    public int hashCode() {
        int c = 0;
        if (mData == null) {
            return c;
        }
        for (int i = mStart, I = mStart + mLen; i < I; i++) {
            if (mData[i] != null) {
                c += (2*i + 1) * mData[i].hashCode();
            }
        }
        return c;
    }
    
    public boolean equals(RowSection p) {
        if (mData == null && p.mData == null) {
            return true;
        }
        if (mData == null && p.mData != null) {
            return false;
        }
        if (mData != null && p.mData == null) {
            return false;
        }
        if (mLen!= p.mLen) {
            return false;
        }
        for (int i = 0; i < mLen; i++) {
            if (!mData[mStart + i].equals(p.mData[p.mStart + i])) {
                return false;
            }
        }
        return true;
    }

    @SuppressWarnings("unchecked")
    // Compare mData and p.mData
    public int compareTo(RowSection p) {
        if (mData == null && p.mData == null) {
            return 0;
        }
        if (mData == null && p.mData != null) {
            return -1;
        }
        if (mData != null && p.mData == null) {
            return 1;
        }
        int minLen = Math.min(mLen, p.mLen);
        for (int i = 0; i < minLen; i++) {
            Comparable c1 = (Comparable)mData[i + mStart];
            int r = c1.compareTo(p.mData[i + p.mStart]);
            if (r == 0) {
                continue;
            }
            return r;
        }
        return 0;
    }
}
