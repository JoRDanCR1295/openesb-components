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
 * @(#)VirtualBPELEngine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.api;

import java.util.Collection;
import java.util.List;

/**
 * VirtualBPELEngine is a virtual BPEL engine for debugging. Instance of this interface 
 * is created by an instance of debugger server and debugger client can get it from the 
 * callback method in <tt>BPELDebugger.setVirtualBPELEngine()</tt>.
 * 
 * @author Sun Microsystems
 * @version 
 *
 */
public interface VirtualBPELEngine {
    
    /** 
     * Returns all deployed BPEL target namespaces. BPEL target namesapce can be used 
     * as a key to get its associated BPELProcessRef object.
     * @return  the array of deployed BPEL
     */    
    String[] allDeployedBPELs();

    
    /**
     * Returns the BPELProcessRef object associated with the target namespace.
     * Return  the bpel process object
     */
    BPELProcessRef getBPELProcess(String tns);
    
    /**
     * Terminates the running process instance
     * @param globalGUID The global ID of the process instance to be terminated
     */
    void terminatePI(String  globalGUID);  
}
