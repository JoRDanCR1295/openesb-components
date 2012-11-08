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
 */

/*
 * @(#)$Id: ContextImpl.java,v 1.1 2008/11/12 23:00:19 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.util.HashMap;
import java.util.Map;

/**
 * An implementation of Deflatable.Context.
 *
 * @author Noel.Ang@sun.com
 */
final class ContextImpl implements Deflatable.Context {
    private final Map<Object, Object> stuff = new HashMap<Object, Object>();

    ContextImpl() {
    }

    /**
     * Adds a datum.
     *
     * @param key Symbol for a something. If null, the call does not modify the
     * context and returns quietly.
     * @param value The something.
     */
    public void addContext(Object key, Object value) {
        if (key != null) {
            stuff.put(key, value);
        }
    }

    /**
     * Recalls a datum from the context.
     *
     * @param key The symbol with which the datum was entered.
     *
     * @return The datum corresponding to the key given. null may be returned if
     *         the key is null or not reconcilable, OR the corresponding datum
     *         is itself null.
     */
    public Object getContext(Object key) {
        return stuff.get(key);
    }
}
