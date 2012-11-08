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
 * @(#)BPELDebugger.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.api;

/**
 * The BPELDebugger interface. BPELDebugger facilitates the communication between
 * bpel debugger server and bpel debugger client through {@link DebuggableEngine}. BPELDebugger
 * initiates the communication when a new {@link DebugFrame} is created usually when bpel
 * engine enters into a new call frame or scope and bpel debugger client uses the
 * <tt>DebuggableEngine</tt> to communicate with bpel debugger server for variables available in the current
 * call frame or scope.
 *
 * <p>When a debugger session finishes either intentionally or unintentionally, the BPELDebugger
 * should use {@link #detach()} to clean up the connection so that next attachment of debugger can proceed successfully
 * 
 * <p> This interface should be implemented by both bpel debugger server and bpel debugger client
 *
 * @author Sun Microsystems
 * @version 
 * @see DebugFrame
 * @see DebuggableEngine
 */
public interface BPELDebugger {
    /**
     * BPEL Debugger Server MUST implement this method to pass 
     * the information of  parent DebugFrame Id, bpelFile name and target uri the DebuggableEngine
     * belongs and returns its local <tt>DebugFrame</tt> that encapsulates the remote <tt>DebugFrame</tt> that will 
     * be used to inform the remote debugger client on runtime activities
     * 
     * This method is invoked from debugger server to debugger client
     *
     * @param id the id of the DebugFrame
     * @param parentFrameId The id of parent debug frame
     * @param bpelFile The name of bpel file
     * @param uri The targetNameSpace uri
     *
     * @return DOCUMENT ME!
     */    
    DebugFrame enterFrame(String id,  String processInstanceId, String parentFrameId, String bpelFile, String uri);

    /**
     * Called when detached from the other end of self-initiated, both can be intentional or accidental.
     * <p>The implementation should account for both situations, the result should be the debugger client and server
     * are both disconnected cleanly
     * 
     * @return boolean true if detach succeeds
     */
    boolean detach();

    /**
     * Invoked when the BPEL process is added to bpel engine
     * @param process The bpel process
     * @return boolean true if the process remove will finish the sesssion
     */
    void processRemoved(BPELProcessRef process);
    
    /**
     * Invoked when the BPEL process is added to bpel engine
     * @param process The bpel process
     */
    void processAdded(BPELProcessRef process);
    
    
    /**
     * Invoked when the BPEL process instance is created
     * @param instance The rocess instance
     */
    void processInstanceStarted(BPELProcessInstanceRef instance);
    
    /**
     * Invoked when the BPEL process instance is completed
     * @param instanceID  The process instance
     */
    void processInstanceDied(BPELProcessInstanceRef instance);
    
    
    /**
     * Debugger server set the instance of VirtualBPELEngine to debugger client.
     * @param engine     the object of VirtualBPELEngine.
     */
    void setVirtualBPELEngine(VirtualBPELEngine engine);
}
