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
 * @(#)DebugServer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;


import java.io.IOException;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugListener;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.ObjectAdapter;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIServer;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIService;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIServiceFactory;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 * @version 
 */
public class DebugServer extends Thread {

    private static final Logger LOGGER = Logger.getLogger(DebugServer.class.getName());

//    /** DOCUMENT ME! */
//    BPELEngine interp;

    /** DOCUMENT ME! */
    DefaultDebugListener dbl;

    /** DOCUMENT ME! */
    ObjectAdapter oAdapter;

    /** DOCUMENT ME! */
    RMIServer rmis;

    /** DOCUMENT ME! */
    RMIService rmi;

//    /** DOCUMENT ME! */
//    int port;

    /**
     * Creates a new DebugServer object.
     *
     * @param interp DOCUMENT ME!
     * @param port DOCUMENT ME!
     * @param bpels DOCUMENT ME!
     * @param wsdls DOCUMENT ME!
     */
    public DebugServer(Engine engine, BPELEngine interp, int port, Map bpels, Map wsdls) {
//        this.interp = interp;
//        this.port = port;
    	
        rmi = getRMIService();

        try {
            rmis = rmi.createServer(port);
            oAdapter = rmis.createObjectAdapter("root");
            rmis.setDefaultAdaptor(oAdapter);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.getMessage(), e);
        }
        dbl = new DefaultDebugListener(engine, interp, oAdapter, bpels, wsdls);
    }

    private RMIService getRMIService() {
        RMIService service = null;

        try {
            RMIServiceFactory factory = (RMIServiceFactory) getClass().getClassLoader()
                                                                .loadClass(
                    "org.netbeans.modules.bpel.debuggerbdi.rmi.wp.impl.DefaultRMIServiceFactory"
                ).newInstance();
            service = factory.createRMIService(getClass().getClassLoader());
        } catch (Exception e) {
            // TODO sure, just ignore?
            e.printStackTrace();
            LOGGER.log(Level.SEVERE, e.getMessage(), e);
        }

        return service;
    }

    /**
     * DOCUMENT ME!
     */
    public void destroy() {
        dbl.detach();
        dbl.destroy();
        rmis.destroy();
        rmi.destroy();
    }

    /**
     * DOCUMENT ME!
     */
    public void run() {
        try {
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-5000: Starting BPEL Debug Server"));

            final DefaultDebugListener dbListener = dbl;
            final ObjectAdapter oa = oAdapter;
            oa.exportObject("debugListener", dbListener);
            oa.start();
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-5001: BPEL-Debug-Server started"));
            rmis.run();
        } catch (Throwable e) {
            // TODO sure, just ignore?
            LOGGER.log(Level.SEVERE, e.getMessage(), e);
            e.printStackTrace();
        }
    }
}


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
class DefaultDebugListener implements DebugListener {

    private static final Logger LOGGER = Logger.getLogger(DefaultDebugListener.class.getName());

    /** DOCUMENT ME! */
    BPELEngine interp;

    /** DOCUMENT ME! */
    DefaultDebugger defaultDebugger;

    /** DOCUMENT ME! */
    ObjectAdapter oa;

    /**
     * Creates a new DefaultDebugListener object.
     *
     * @param eng DOCUMENT ME!
     * @param oa DOCUMENT ME!
     * @param bpels DOCUMENT ME!
     * @param wsdls DOCUMENT ME!
     */
    DefaultDebugListener(Engine eng, BPELEngine bpelEng, ObjectAdapter oa, Map bpels, Map wsdls) {        
        defaultDebugger = new DefaultDebugger(eng);
        this.oa = oa;
        oa.registerListerner(this);
        interp = (BPELEngine) eng;
        defaultDebugger.setBpels(bpels);
        defaultDebugger.setWsdls(wsdls);
        interp.setDebugger(defaultDebugger);
    }

    /**
     * DOCUMENT ME!
     *
     * @param debugger DOCUMENT ME!
     */
    public void setDebugger(BPELDebugger debugger) {
        final BPELDebugger fDebugger = debugger;

        // the setRemoteDebugger method would start calling methods on the debugger
        // if the methods are called on the debugger without returning this thread,
        // deadlock would occur on the client side.
        // A thread is started which would delegate the debugger
        defaultDebugger.setRemoteDebugger(fDebugger);

//        Runnable runner = new Runnable() {
//                public void run() {
//                    defaultDebugger.setRemoteDebugger(fDebugger);
//                }
//            };
//
//        new Thread(runner).start();
    }

    /**
     * Detach the client, close the client sockets already established, but leave the server socket
     * open
     */
    public void detach() {
        defaultDebugger.detach();
        oa.getServer().closeClients();
    }

    /**
     * Close the server socket, called when the EAR is being unloaded.
     */
    public void destroy() {
        oa.getServer().destroy();
    }

    /**
     * DOCUMENT ME!
     *
     * @param socket DOCUMENT ME!
     */
    public void socketClosed(Object socket) {
        LOGGER.log(Level.CONFIG, I18n.loc("BPCOR-4000: Detatch client for the closed socket:") + socket);
        detach();
    }
}
