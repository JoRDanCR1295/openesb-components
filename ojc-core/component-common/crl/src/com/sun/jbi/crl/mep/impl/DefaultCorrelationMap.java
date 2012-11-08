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
 * @(#)DefaultCorrelationMap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.HashMap;
import java.util.Map;

import com.sun.jbi.crl.mep.CorrelationMap;

/**
 * Default implementation of a {@link CorrelationMap}.
 * 
 * @author Kevan Simpson
 */
public class DefaultCorrelationMap implements CorrelationMap {
    private Map<String, Object> mCorrelationMap = new HashMap<String, Object>();

    /** @see com.sun.jbi.crl.mep.CorrelationMap#correlate(java.lang.String, java.lang.Object) */
    public void correlate(String exchangeId, Object data) {
        if (exchangeId != null && data != null) {
            synchronized (mCorrelationMap) {
                mCorrelationMap.put(exchangeId, data);
            }
        }
    }

    /** @see com.sun.jbi.crl.mep.CorrelationMap#decorrelate(java.lang.String) */
    public Object decorrelate(String exchangeId) {
        Object data = null;
        if (exchangeId != null) {
            synchronized (mCorrelationMap) {
                data = mCorrelationMap.remove(exchangeId);
            }
        }
        return data;
    }

    /** @see com.sun.jbi.crl.mep.CorrelationMap#lookup(java.lang.String) */
    public Object lookup(String exchangeId) {
        if (exchangeId != null) {
            synchronized (mCorrelationMap) {
                return mCorrelationMap.get(exchangeId);
            }
        }
        return null;
    }

    protected Map<String, Object> getCorrelationMap() {
        return mCorrelationMap;
    }
    protected void setCorrelationMap(Map<String, Object> map) {
        if (map != null) mCorrelationMap = map;
    }
}
