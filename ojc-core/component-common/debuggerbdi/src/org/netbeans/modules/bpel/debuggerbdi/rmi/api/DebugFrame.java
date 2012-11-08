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
 * @(#)DebugFrame.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.api;

/**
 * DebugFrame interface. DebugFrame acts as a bridge between bpel debugger client and server.
 * <p>This interface should be implemented by both bpel debugger server and bpel debugger client.
 * Bpel debugger server implements this interface to inform bpel debugger client on each activity bpel
 * server executes while bpel debugger client implements this interface to receive the activity information
 * from bpel debugger server.
 *
 * @author Sun Microsystems
 * @version 
 * @see BPELDebugger
 */
public interface DebugFrame {
    
    public static final String BPEL_NS_PREFIX = "bpws";
    
    public static final String BPEL_NS_URI = "http://docs.oasis-open.org/wsbpel/2.0/process/executable";
    
    /**
     * Bpel debugger server implements this method to invoke onLineChange on
     * the remote <tt>DebugFrame</tt>
     * <p> Bpel debugger client implents this method to receive the bpel runtime activity information
     *
     * @param bpelFile The bpel file name
     * @param uri The bpel target namespace uri
     * @param lineNumber The current activity's line number
     * @param xpath The xpath of current activity
     * @param engine The DebuggableEngine used for querying the variables
     */
    public void onLineChange(String bpelFile, String uri, int lineNumber, String xpath, DebuggableEngine engine);

    /**
     * Bpel debugger server implements this method to notify debugger a Fault is thrown. There are various sources of faults:<p>
     * (1) A fault response to an &lt;invoke&gt; activity.
     * (2) A explicity &lt;throw&gt; activity.
     * (3) WS-BPEL defined standand faults.
     * (4) BPEL engine specific faults.
     *
     * @param bpelFile The bpel file name
     * @param uri The bpel target namespace uri
     * @param lineNumber The current line number where throws the fault
     * @param xpath The xpath of the current activity which throws the fault 
     * @param faultName The QName of the fault
     * @param faultData  The optional information about the fault. <code>faultData</code> can be a real BPEL variable, 
     * for example, if this fault is thrown in &lt;throw&gt; activity; or it can be a BPEL variable wrapper which holds
     * a WSDL Message or XSD schema element, for example, if this fault is thrown in &lt;invoke&gt; activity.
     * @param engine The DebuggableEngine used for querying the variables
     */
    public void onFault(String bpelFile, String uri, int lineNumber, String xpath, String faultQName, 
        BPELVariable faultData, DebuggableEngine engine);

    /**
     * Bpel debugger server implements this method to invoke onXPathException on the remote
     * <tt>DebugFrame</tt>
     * <p> Bpel debugger client implements this method to receive the bpel runtime XPathException information
     *
     * @param bpelFile The bpel file name
     * @param uri The bpel target namespace uri
     * @param lineNumber The current XPathException line number
     * @param xpath The xpath of the current XPathException
     */
    public void onXPathException(String bpelFile, String uri, int lineNumber, String message, String xpath);

    /**
     * Bpel debugger server implements this method to invoke onTerminate on the remote
     * <tt>DebugFrame</tt>
     * <p> Bpel debugger client implements this method to receive the bpel runtime abrupt termination information
     *
     * @param bpelFile The bpel file name
     * @param uri The bpel target namespace uri
     * @param lineNumber The current termination line number
     * @param xpath The xpath of the current termination
     */
    public void onTerminate(String bpelFile, String uri, int lineNumber, String xpath);

    /**
     * Bpel debugger server implements this method to invoke onExit on the remote
     * <tt>DebugFrame</tt>
     * <p> Bpel debugger client implements this method to receive the bpel instance complete information
     *
     * @param bpelFile The bpel file name
     * @param uri The bpel target namespace uri
     */
    public void onExit(String bpelFile, String uri);
    
    /**
     * Bpel debugger server implements this method to on 
     * the remote <tt>DebugFrame</tt>  when an activity is completed
     * @param bpelFile The bpel file name
     * @param uri  The bpel target namespace uri
     * @param lineNumber The line number of the bpel activity   
     * @param xpath The xpath of the bpel actvity
     */
    public void onActivityComplete(String bpelFile, String uri, int lineNumber, String xpath);    
    
    public void onSubActivityComplete(String bpelFile, String uri, int lineNumber, String xpath);
}
