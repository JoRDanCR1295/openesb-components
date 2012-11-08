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
 * @(#)DefaultDebugger.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessInstanceRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugFrame;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.VirtualBPELEngine;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * DOCUMENT ME!
 * 
 * @author Sun Microsystems
 * @version 
 */
public class DefaultDebugger implements BPELDebugger {

    private static final Logger LOGGER = Logger.getLogger(DefaultDebugger.class.getName());

    private VirtualBPELEngineImpl mEngMir;

    /** DOCUMENT ME! */
    BPELDebugger remoteDebugger = null;

    /**
     * debugFrames from the engine. engineDebugFrame - DefaultDebugFrame when engineDebugFrame is
     * garbage collected, the entry is removed from this map
     */

    // @SuppressWarnings("unchecked") Map debugFrames = Collections.synchronizedMap(new
    // WeakHashMap());
    Map debugFrames = Collections.synchronizedMap(new HashMap());

    Map processes = Collections.synchronizedMap(new HashMap());

    Map invalidDebugFrames = Collections.synchronizedMap(new HashMap());

    /** DOCUMENT ME! */
    Map bpels = new HashMap();

    /** DOCUMENT ME! */
    Map wsdls = new HashMap();

    /** DOCUMENT ME! */
    Map xsds = new HashMap();

    boolean isDetached = true;

    Object detachLock = new Object();

    /**
     * DOCUMENT ME!
     * 
     * @param debugger
     *            DOCUMENT ME!
     */
    protected DefaultDebugger(Engine engine) {
        mEngMir = new VirtualBPELEngineImpl(engine);
    }

    public void setRemoteDebugger(BPELDebugger debugger) {
        synchronized (detachLock) {
            try {
                if (debugger != null) {
                    isDetached = false;
                    remoteDebugger = debugger;
                    remoteDebugger.setVirtualBPELEngine(mEngMir);
                    synchronized (debugFrames) {
                        Iterator debugFramesItr = debugFrames.values().iterator();
                        while (debugFramesItr.hasNext()) {
                            DefaultDebugFrame myDebugFrame = (DefaultDebugFrame) (((WeakReference) debugFramesItr
                                    .next()).get());
                            if (myDebugFrame != null) {
                                myDebugFrame.setRemoteDebugger(remoteDebugger);
                            }
                        }
                    }

                } else if (!isDetached) {
                    isDetached = true;
                    detachClient(null);
                }
            } catch (Throwable e) {
                LOGGER.log(Level.SEVERE, I18n.loc("BPCOR-7009: exception in setRemoteDebugger:"), e);
                detachClient(e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger#detach()
     */
    public boolean detach() {
        synchronized (detachLock) {
            if (isDetached) {
                return true;
            }
            isDetached = true;
            detachClient(null);
            return true;
        }
    }

    private void detachClient(Throwable e) {
        if (e != null) {
            LOGGER.log(Level.WARNING, e.getMessage(), e);
        }
        synchronized (debugFrames) {
            Iterator debugFramesItr = debugFrames.values().iterator();

            while (debugFramesItr.hasNext()) {
                DefaultDebugFrame myDebugFrame = (DefaultDebugFrame) (((WeakReference) debugFramesItr
                        .next()).get());
                if (myDebugFrame != null) {
                    myDebugFrame.setRemoteDebugger(null);
                }
            }
        }

        if (remoteDebugger != null) {
            final BPELDebugger bpelDebugger = remoteDebugger;
            Runnable runner = new Runnable() {
                public void run() {
                    try {
                        bpelDebugger.detach();
                    } catch (Throwable t) {
                        remoteDebugger = null;
                        LOGGER.log(Level.SEVERE, t.getMessage(), t);
                    }
                }
            };
            new Thread(runner).start();
            // try {
            // remoteDebugger.detach();
            // } catch (Throwable t) {
            // remoteDebugger = null;
            // LOGGER.log(Level.SEVERE, t.getMessage(), t);
            // }

            remoteDebugger = null;
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6111: bpel-debugger-client detached, the client must reconnect"));
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param bpels2
     *            collection of bpels
     */
    public void setBpels(Map bpels2) {
        // TODO Auto-generated method stub
        bpels.putAll(bpels2);
    }

    /**
     * sets a collection of bpel definitions to wsdls map
     * 
     * @param wsdls2
     *            collection of wsdl files
     */
    public void setWsdls(Map wsdls2) {
        // TODO Auto-generated method stub
        wsdls.putAll(wsdls2);
    }

    /**
     * Remove the debug frame when it exits
     * 
     * @param debugFrame
     *            DOCUMENT ME!
     */
    public void frameExit(DebugFrame debugFrame) {
        synchronized (debugFrames) {
            Set frameSet = debugFrames.entrySet();
            Iterator debugFramesItr = frameSet.iterator();

            while (debugFramesItr.hasNext()) {
                Entry entry = (Entry) debugFramesItr.next();
                DebugFrame myDebugFrame = (DebugFrame) (((WeakReference) entry.getValue()).get());

                if (myDebugFrame == debugFrame) {
                    debugFrames.remove(entry.getKey());
                    break;
                }
            }
        }
    }

    public DebugFrame enterFrame(String id, String processInstanceId, String parentFrameId,
            String bpelFile, String uri) {
        // TODO Auto-generated method stub
        // Integer count;
        // synchronized (processes) {
        // count = (Integer) processes.get(uri);
        // if (count == null) {
        // count = new Integer(1);
        // processes.put(uri, count);
        // } else {
        // synchronized (count) {
        // count = new Integer(count.intValue() + 1);
        // processes.put(uri, count);
        // }
        // }
        // }
        // String id = uri + "fr:" + count;
        // if (processInstanceId == null) {
        // processInstanceId = id;
        // }
        DefaultDebugFrame myFrame = new DefaultDebugFrame(processInstanceId, parentFrameId, id,
                bpelFile, uri);

        // when 'engine' is garbage collected, the entry is removed from
        // the WeakHashMap
        synchronized (debugFrames) {
            debugFrames.put(id, new WeakReference(myFrame));
        }

        if (remoteDebugger != null) {
            myFrame.setRemoteDebugger(remoteDebugger);
        }
        return myFrame;
    }

    public boolean isDetached() {
        synchronized (detachLock) {
            return isDetached;
        }
    }

    public void setVirtualBPELEngine(VirtualBPELEngine engine) {
        // debugger client implements it.
    }

    public void processAdded(BPELProcessRef bpelProcessMir) {
        if (remoteDebugger != null) {
            remoteDebugger.processAdded(bpelProcessMir);
        }
    }

    public void processRemoved(BPELProcessRef bpelProcessMir) {
        if (remoteDebugger != null) {
            remoteDebugger.processRemoved(bpelProcessMir);
        }
        String targetNamespace = bpelProcessMir.uri();
        processes.remove(targetNamespace);
        synchronized (debugFrames) {
            Iterator debugFramesItr = debugFrames.values().iterator();

            while (debugFramesItr.hasNext()) {
                WeakReference debugFrameRef = (WeakReference) debugFramesItr.next();
                DefaultDebugFrame myDebugFrame = (DefaultDebugFrame) debugFrameRef.get();

                if (myDebugFrame != null && myDebugFrame.getUri().equals(targetNamespace)) {
                    invalidDebugFrames.put(myDebugFrame.getId(), debugFrameRef);
                    debugFramesItr.remove();
                }
            }
        }
    }

    /**
     * Notified remote debugger
     */
    public void processInstanceStarted(BPELProcessInstanceRef instanceMir) {
        if (remoteDebugger != null) {
            remoteDebugger.processInstanceStarted(instanceMir);
        }

    }

    public void processInstanceDied(BPELProcessInstanceRef instanceMir) {
        if (remoteDebugger != null) {
            remoteDebugger.processInstanceDied(instanceMir);
        }
    }

    // ///////////////////////////////////////////////////////////////////////
    public boolean isDebugFrameValid(String id) {
        return !(invalidDebugFrames.containsKey(id));
    }

    public void processAdded(RBPELProcess bpelProcess) {
        BPELProcessRef ref = mEngMir.addProcess(bpelProcess);
        processAdded(ref);
    }

    public void processRemoved(RBPELProcess bpelProcess) {
        BPELProcessRef ref = mEngMir.removeProcess(bpelProcess);
        /*CR 6527110: The user has the same BPEL in two separate SU, udeploying one and then 
         * undeploying another one leads to a NPE if processRemoved() is called with null
         * ref.
         * IMP NOTE: The Question is why is the internal data structure in the debugger storing 
         * BPELProcess reference by targetnamespace as the key. This could be an issue as there
         * could be different BPELProcess(activities wise) with the same targetnamespace 
         */
        if (ref != null) {
        	processRemoved(ref);
        }
    }

    public synchronized void processInstanceStarted(BPELProcessInstance instance) {
        BPELProcessInstanceRef ref = mEngMir.addProcessInstance(instance);
        processInstanceStarted(ref);

    }

    public synchronized void processInstanceDied(BPELProcessInstance instance) {
        BPELProcessInstanceRef ref = mEngMir.removeProcessInstance(instance);
        processInstanceDied(ref);
    }

}
