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
 * @(#)RequiredObjectsImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.HashMap;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;


/**
 * Required objects implementation
 *
 * @author Sun Microsystems
 */
public class RequiredObjectsImpl implements RequiredObjects {
    /** engine */
    Engine mEng;

    /** BPEL processor */
    BPELInterpreter mInterp;

    /** properties */
    HashMap mProps = new HashMap();
    
    /** BPELProcessManagerImpl instance */
    BPELProcessManager mProcessManager;


    
    /**
     * Creates a new RequiredObjectsImpl object.
     *
     * @param engine engine
     * @param interp BPEL processor
     */
    public RequiredObjectsImpl(Engine engine, BPELInterpreter interp,
    		BPELProcessManager procManager) {
        mEng = engine;
        mInterp = interp;
        mProcessManager = procManager;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects#getEngine()
     */
    public Engine getEngine() {
        return mEng;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects#getInterp()
     */
    public BPELInterpreter getInterp() {
        return mInterp;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects#getValue(java.lang.String)
     */
    public Object getValue(String propName) {
        return mProps.get(propName);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects#setValue(java.lang.String,
     *      Object)
     */
    public void setValue(String propName, Object val) {
        mProps.put(propName, val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects#removeValue(java.lang.String)
     */
    public Object removeValue(String propName) {
        return mProps.remove(propName);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects#getBPELProcessManager()
     */
    public BPELProcessManager getBPELProcessManager() {
    	return mProcessManager;
    }
}
