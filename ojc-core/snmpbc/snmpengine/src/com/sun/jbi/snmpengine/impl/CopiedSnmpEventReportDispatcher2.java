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
 * @(#)CopiedSnmpEventReportDispatcher2.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.impl;


import com.sun.jdmk.tasks.Task;
import com.sun.jdmk.tasks.TaskServer;
import com.sun.management.snmp.JdmkEngineFactory;
import com.sun.management.snmp.SnmpEngine;
import com.sun.management.snmp.SnmpEngineFactory;
import com.sun.management.snmp.SnmpEngineParameters;

import java.net.InetAddress;
import java.net.SocketException;

/**
 * Implements an SNMP event report dispatcher.
 * <P>
 * Listener objects can be registered in an 
 * <CODE>SnmpEventReportDispatcher</CODE>.
 * <BR>The <CODE>SnmpEventReportDispatcher</CODE> listens for any incoming 
 * trap or inform PDU: each time it receives a trap or an inform PDU, it 
 * informs all the listeners.
 * <BR>By default, the dispatcher listens to the UDP port 162.
 * <P>
 * The <CODE>SnmpEventReportDispatcher</CODE> class implements 
 * <CODE>Runnable</CODE>: it is expected to be used as a 
 * <CODE>Thread</CODE> object. The run method never returns.
 * <P>
 * An <CODE>SnmpEventReportDispatcher</CODE> object maintains 2 lists of
 * listener objects. The listener objects implement the 
 * <CODE>SnmpTrapListener</CODE> or <CODE>SnmpInformListener</CODE>
 * interface and will be activated each time an SNMP event report (trap 
 * or inform PDU) is received. 
 * <P>
 * By default, the SnmpEventReportDispatcher will create one 
 * {@link com.sun.jdmk.tasks.Task} per incoming message (inform or trap) and 
 * triggered listener, and will execute this task in a new thread. 
 * This default threading policy can be overridden by providing a specific 
 * {@link com.sun.jdmk.tasks.TaskServer} in the constructor of this object.
 *
 * @see com.sun.management.snmp.manager.SnmpTrapListener
 * @see com.sun.management.snmp.manager.SnmpInformListener
 * @see com.sun.jdmk.tasks.TaskServer
 *
 *
 * @since Java DMK 5.1
 */


public class CopiedSnmpEventReportDispatcher2 
    extends CopiedSnmpEventReportDispatcher {

    private final static class TaskHandler implements Task {
	private final Runnable task;
	public TaskHandler(Runnable task) {
	    this.task = task;
	}
	public void run() {
	    if (task == null) return;
	    task.run();
	}
	public void cancel() {
	}
    }
    

    private final static class DefaultTaskServer implements TaskServer {
	public final void submitTask(Task task) {
	    (new Thread(task)).start();
	}
    }

    // PRIVATE VARIABLES
    //------------------
    
    private final TaskServer packetTaskServer;
    private final TaskServer callbackTaskServer;

    // PACKAGE VARIABLES
    //------------------
//BC-1    
//    private final static String dbgTag = "SnmpEventReportDispatcher";
    
    // PUBLIC METHODS
    //---------------

    /**
     * Initializes an <CODE>SnmpEventReportDispatcher</CODE> which 
     * listens on the port 162.   
     * It instantiates an <CODE> SnmpEngine. </CODE>  
     *  <P> WARNING : The SnmpEngineId is computed as follows:
     * <ul>
     * <li> If an LCD file is provided containing the property "
     *      localEngineID", this property value is used.</li>.
     * <li> If not, if the passed engineId is not null, this engine Id 
     *      is used.</li>
     * <li> If not, the dispatcher computes a time-based
     *      <CODE>SnmpEngineId</CODE>.</li>
     * </ul>
     * @exception java.lang.IllegalArgumentException If one of the 
     *            specified configuration files do not exist.
     * @exception SocketException If the object cannot bind to the port 162.
     */
    public CopiedSnmpEventReportDispatcher2() 
	throws SocketException, IllegalArgumentException {
        this(new SnmpEngineParameters(),
	     new JdmkEngineFactory(),
	     162, null, new DefaultTaskServer(), null);
    }
    
    /**
     * Initializes an <CODE>SnmpEventReportDispatcher</CODE> which listens 
     * on the specified port.
     * It instantiates an <CODE> SnmpEngine. </CODE>  
     *  <P> WARNING : The SnmpEngineId is computed as follows:
     * <ul>
     * <li> If an LCD file is provided containing the property "
     *      localEngineID", this property value is used.</li>.
     * <li> If not, if the passed engineId is not null, this engine Id 
     *      is used.</li>
     * <li> If not, the session computes a time-based
     *      <CODE>SnmpEngineId</CODE>.</li>
     * </ul>
     * @exception java.lang.IllegalArgumentException If one of the 
     *            specified configuration files don't exist.
     * @param portNumber The port number.
     *
     * @exception SocketException If the object cannot bind to the 
     *            specified port.
     */
    public CopiedSnmpEventReportDispatcher2(int portNumber) 
	throws SocketException, IllegalArgumentException{
	this(new SnmpEngineParameters(),
	     new JdmkEngineFactory(),
	     portNumber, null,
	     new DefaultTaskServer(),null);
    }

    /**
     * Initializes an <CODE>SnmpEventReportDispatcher</CODE> which listens 
     * on the specified port.
     * It instantiates an <CODE> SnmpEngine. </CODE>  
     *  <P> WARNING : The SnmpEngineId is computed as follows:
     * <ul>
     * <li> If an LCD file is provided containing the property "
     *      localEngineID", this property value is used.</li>.
     * <li> If not, if the passed engineId is not null, this engine Id 
     *      is used.</li>
     * <li> If not, the dispatcher computes a time-based
     *      <CODE>SnmpEngineId</CODE>.</li>
     * </ul>
     * @exception java.lang.IllegalArgumentException If one of the 
     *            specified configuration files don't exist.
     * @param portNumber The port number.
     * @param address The Ip address the dispatcher will listen for events
     *        on. If null, the dispatcher will listen on localhost.
     * @param packetTaskServer The task server to use when a trap or
     *        inform PDU is received.
     * @param callbackTaskServer The task server to use when invoking
     *        a callback. This can be the same object as 
     *        <code>packetTaskServer</code>
     * @exception SocketException If the object cannot bind to the 
     *            specified port.
     */    
    public CopiedSnmpEventReportDispatcher2(int portNumber, InetAddress address,
				     TaskServer packetTaskServer,
				     TaskServer callbackTaskServer)
	throws SocketException, IllegalArgumentException {
	this(new SnmpEngineParameters(),
	     new JdmkEngineFactory(),
	     portNumber, address,
	     packetTaskServer,
	     callbackTaskServer);
    }
    
    /**
     * Initializes an <CODE>SnmpEventReportDispatcher</CODE> which listens 
     * on the specified port.
     * It instantiates an <CODE> SnmpEngine. </CODE>  
     *  <P> WARNING : The SnmpEngineId is computed as follows:
     * <ul>
     * <li> If an LCD file is provided containing the property "
     *      localEngineID", this property value is used.</li>.
     * <li> If not, if the passed engineId is not null, this engine Id 
     *      is used.</li>
     * <li> If not, the dispatcher computes a time-based
     *      <CODE>SnmpEngineId</CODE>.</li>
     * </ul>
     * @exception java.lang.IllegalArgumentException If one of the 
     *            specified configuration files don't exist.
     * @param parameters The engine parameters to use. 
     * @param factory The factory to use in order to create the engine.
     * @param portNumber The port number.
     * @param packetTaskServer The task server to use when a trap or
     *        inform PDU is received.
     * @param callbackTaskServer The task server to use when invoking
     *        a callback. This can be the same object as 
     *        <code>packetTaskServer</code>
     * @exception SocketException If the object cannot bind to the 
     *            specified port.
     */    
    public CopiedSnmpEventReportDispatcher2(SnmpEngineParameters parameters, 
				     SnmpEngineFactory factory,
				     int portNumber, 
				     TaskServer packetTaskServer,
				     TaskServer callbackTaskServer)
	throws SocketException, IllegalArgumentException {
	this(parameters, factory, portNumber, null,
	     packetTaskServer, callbackTaskServer);
    }

    /**
     * Initializes an <CODE>SnmpEventReportDispatcher</CODE> which listens 
     * on the specified port.
     *
     * @param engine The <CODE> SnmpEngine </CODE> to use.
     * @param portNumber The port number.
     * @param packetTaskServer The task server to use when a trap or
     *        inform PDU is received.
     * @param callbackTaskServer The task server to use when invoking
     *        a callback. This can be the same object as 
     *        <code>packetTaskServer</code>
     * @exception IllegalArgumentException if the passed engine is null.
     * @exception SocketException If the object cannot bind to the 
     *            specified port.
     */    
    public CopiedSnmpEventReportDispatcher2(SnmpEngine engine,
				     int portNumber, 
				     TaskServer packetTaskServer,
				     TaskServer callbackTaskServer)
	throws SocketException, IllegalArgumentException {
	this(engine, portNumber, null, packetTaskServer, callbackTaskServer);
    }

    /**
     * Initializes an <CODE>SnmpEventReportDispatcher</CODE> which listens 
     * on the specified port.
     * It instantiates an <CODE> SnmpEngine. </CODE>  
     *  <P> WARNING : The SnmpEngineId is computed as follows:
     * <ul>
     * <li> If an LCD file is provided containing the property "
     *      localEngineID", this property value is used.</li>.
     * <li> If not, if the passed engineId is not null, this engine Id 
     *      is used.</li>
     * <li> If not, the dispatcher computes a time-based  
     *      <CODE>SnmpEngineId</CODE>.</li>
     * </ul>
     * @exception java.lang.IllegalArgumentException If one of the 
     *            specified configuration files don't exist.
     * @param parameters The engine parameters to use. 
     * @param factory The factory to use in order to create the engine.
     * @param portNumber The port number.
     * @param address The Ip address the dispatcher will listen for events 
     *        on. If null, the dispatcher will listen on localhost.
     * @param packetTaskServer The task server to use when a trap or
     *        inform PDU is received.
     * @param callbackTaskServer The task server to use when invoking
     *        a callback. This can be the same object as 
     *        <code>packetTaskServer</code>
     * @exception SocketException If the object cannot bind to the 
     *            specified port.
     */    
    public CopiedSnmpEventReportDispatcher2(SnmpEngineParameters parameters, 
				     SnmpEngineFactory factory,
				     int portNumber, InetAddress address,
				     TaskServer packetTaskServer,
				     TaskServer callbackTaskServer)
	throws SocketException, IllegalArgumentException {
	super(parameters, factory,portNumber,address);
	this.packetTaskServer   = packetTaskServer;
	this.callbackTaskServer = callbackTaskServer;
    }
    
    /**
     * Initializes an <CODE>SnmpEventReportDispatcher</CODE> which listens 
     * on the specified port.
     * @param engine The <CODE> SnmpEngine </CODE> to use.
     * @param portNumber The port number.
     * @param address The Ip address the dispatcher will listen for events
     *        on. If null, the dispatcher will listen on localhost.
     * @param packetTaskServer The task server to use when a trap or
     *        inform PDU is received.
     * @param callbackTaskServer The task server to use when invoking
     *        a callback. This can be the same object as 
     *        <code>packetTaskServer</code>
     * @exception IllegalArgumentException if the passed engine is null.
     * @exception SocketException If the object cannot bind to the 
     *            specified port.
     */    
    public CopiedSnmpEventReportDispatcher2(SnmpEngine engine,
				     int portNumber, InetAddress address,
				     TaskServer packetTaskServer, 
				     TaskServer callbackTaskServer)
	throws SocketException, IllegalArgumentException {

	super(engine,portNumber,address);
	this.packetTaskServer   = packetTaskServer;
	this.callbackTaskServer = callbackTaskServer;
    }

    /**
     * This method invokes a listener callback. Do not call this method
     * directly. It is called by the super class when a callback
     * needs to be invoked.
     * <p>This method uses the <var>callbackTaskServer</var> in 
     * order to execute the given <var>callbackHandler</var>. 
     * @param callbackHandler A Runnable object wrapping the listener 
     *        callback to be invoked.
     **/
    protected final void handleCallback(final Runnable callbackHandler) {
	if (callbackTaskServer == null) { 
	    callbackHandler.run();
	} else {
	    callbackTaskServer.submitTask(new TaskHandler(callbackHandler));
	}
    }

    /**
     * This method handles a received SNMP DatagramPacket. Do not 
     * call this method directly. It is called by the super class 
     * when an SNMP DatagrapPacket is received.
     * <p>This method uses the <var>packetTaskServer</var> in 
     * order to execute the given <var>packetHandler</var>. 
     * @param packetHandler A Runnable object wrapping the packet 
     *        to be handled.
     **/
    protected final void handlePacket(final Runnable packetHandler) {
	if (packetTaskServer == null) {
	    packetHandler.run();
	} else {
	    packetTaskServer.submitTask(new TaskHandler(packetHandler));
	}
    }

}
