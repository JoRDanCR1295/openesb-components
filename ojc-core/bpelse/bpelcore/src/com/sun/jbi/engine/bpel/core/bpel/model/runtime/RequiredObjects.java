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
 * @(#)RequiredObjects.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;


/**
 * Required objects interface
 *
 * @author Sun Microsystems
 */
public interface RequiredObjects {
    
    String CRMP_INVOKE_ID = "CRMP_INVOKE_ID";
    /**
     * gets engine
     *
     * @return Engine engine
     */
    Engine getEngine();

    /**
     * gets BPEL processor
     *
     * @return BPELInterpreter BPEL processor
     */
    BPELInterpreter getInterp();

    /**
     * gets property value
     *
     * @param propName property name
     *
     * @return Object property value
     */
    Object getValue(String propName);

    /**
     * sets property value
     *
     * @param propName property name
     * @param val property value
     */
    void setValue(String propName, Object val);

    /**
     * removes property value
     *
     * @param propName property name
     *
     * @return Object property value
     */
    Object removeValue(String propName);
    
    /**
     * public access for the BPELProcessManager instance
     * @return BPELProcessManager
     */
    BPELProcessManager getBPELProcessManager();

}
