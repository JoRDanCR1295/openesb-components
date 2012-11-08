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
 * @(#)BPELEngine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger;

import com.sun.bpel.model.Assign;



/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface BPELEngine {

    /**
     * Assign a debugger
     *
     * @param debugger The debugger
     */
    public void setDebugger(BPELDebugger debugger);

    /**
     * Return the current debugger
     *
     * @return The debugger
     */
    public BPELDebugger getDebugger();

    /**
     * DOCUMENT ME!
     */
    public void enableDebugger();

    /**
     * DOCUMENT ME!
     */
    public void disableDebugger();

    /**
     * DOCUMENT ME!
     *
     * @param port DOCUMENT ME!
     */
    public void setDebuggerPort(String port);

    /**
     * DOCUMENT ME!
     *
     * @author Sun Microsystems
     */
    public interface Assigner {
        /**
         * DOCUMENT ME!
         *
         * @param model DOCUMENT ME!
         * @param callFrame DOCUMENT ME!
         */
        public void assignFunction(Assign model, ICallFrame callFrame);
    }
}
