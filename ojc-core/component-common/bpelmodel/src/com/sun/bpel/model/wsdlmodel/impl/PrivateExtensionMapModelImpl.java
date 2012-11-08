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
 * @(#)PrivateExtensionMapModelImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.wsdlmodel.impl;


import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.sun.bpel.xml.common.model.PrivateExtensionMapModel;

/**
 * Implements the Private Extension Map model.
 *
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public class PrivateExtensionMapModelImpl implements PrivateExtensionMapModel {
    
    /** Holds the map */
    protected Map mMap = null;
    
    /** Creates a new instance of PrivateExtensionMapModelImpl.
     */
    public PrivateExtensionMapModelImpl() {
        super();
        mMap = new HashMap();
    }
    
    /** @see com.sun.bpel.model.common.model.PrivateExtensionMapModel#getNextUID()
     */
    public synchronized String getNextUID() {
        return null;
    }
    
    /** @see com.sun.bpel.model.common.model.PrivateExtensionMapModel#remove(java.lang.String)
     */
    public synchronized void remove(String key) {
        mMap.remove(key);
    }
    
    /** @see com.sun.bpel.model.common.model.PrivateExtensionMapModel#values()
     */
    public Collection values() {
        return mMap.values();
    }
    
    /** @see com.sun.bpel.model.common.model.PrivateExtensionMapModel#keys()
     */
    public Set keys() {
        return mMap.keySet();
    }
    
    /** @see com.sun.bpel.model.common.model.PrivateExtensionMapModel#clear()
     */
    public synchronized void clear() {
        mMap.clear();
    }
    
    /** @see com.sun.bpel.model.common.model.PrivateExtensionMapModel#clearAllInUse()
     */
    public synchronized void clearAllInUse() {
    }
    
    /** @see com.sun.bpel.model.common.model.PrivateExtensionMapModel#removeAllUnused()
     */
    public synchronized void removeAllUnused() {
    }
}
