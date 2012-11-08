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
 * @(#)DebugListener.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.api;

/**
 * Interface to listen for a BPELDebugger for closing  up sockets cleanly
 * @author Sun Microsystems
 * @version 
 *
 */
public interface DebugListener {
    /**
     * Set the debugger instance on which socketClose will invoke a detach of the debugger
     *
     * @param debugger The bpel debugger to set
     */
    public void setDebugger(BPELDebugger debugger);

    /**
     * Detach the debugger when socket close is received.
     *
     * @param socket The socket to close
     */
    public void socketClosed(Object socket);
}
