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
 * @(#)VirtualBPELEngineImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessInstanceRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.VirtualBPELEngine;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;

/**
 * Implementation of VirtualBPELEngine.
 * @author Sun Microsystems
 * @version 
 */
public class VirtualBPELEngineImpl implements VirtualBPELEngine {
    private Engine mEng;
    private Map mTNS2Ref = Collections.synchronizedMap(
        new HashMap());
    private Map<String, BPELProcessInstanceRef> mGUI2Ref = Collections.synchronizedMap(
        new HashMap()); 
    

    /** Creates a new instance of VirtualBPELEngineImpl */
    public VirtualBPELEngineImpl(Engine engine) {
    	mEng = engine;
        initializeProcessMap ();
    }
    
    private void initializeProcessMap () {
        Collection <RBPELProcess> bpelModels =  mEng.getBPELProcesses();
        for (RBPELProcess model : bpelModels ) {
            BPELProcessManager manager = mEng.getBPELProcessManager(model.getBPELId());
            BPELProcessRef processRef = new BPELProcessRefImpl(manager, model);
            List <BPELProcessInstanceRef> procInstanceRefs = 
                    Collections.synchronizedList(new ArrayList());
            if (manager != null) {                
                Collection <BPELProcessInstance> procInstColl = manager.getProcessInstances();
                synchronized (procInstColl) {
                    for (BPELProcessInstance processInstance : procInstColl ) { 
                        BPELProcessInstanceRef processInstanceRef = ((BPELProcessRefImpl)processRef).wrapProcessInstance(processInstance);
                        procInstanceRefs.add(processInstanceRef);
                        mGUI2Ref.put(processInstance.getId(), processInstanceRef);
                    }
                }
            }
            mTNS2Ref.put(model.getTargetNamespace(), processRef);
        }                 
    }
    
    public String[] allDeployedBPELs() {
        Collection <String> tns = mTNS2Ref.keySet();
        String[] tnsArray = new String[tns.size()];
        int index = 0;
        for (String item : tns) {
            tnsArray[index] = item;
            index ++;           
        }
        return tnsArray;
    }
    
    public BPELProcessRef getBPELProcess(String tns) {
        return (BPELProcessRef) mTNS2Ref.get(tns);
    }
    
    public void terminatePI(String globalGUID) {
        BPELProcessInstanceRefImpl processInstanceRef = (BPELProcessInstanceRefImpl)mGUI2Ref.get(globalGUID);
        BPELProcessInstance processInstance = processInstanceRef.getProcessInstance();
        //removeProcessInstance(processInstance);
        processInstance.getBPELProcessManager().terminate(processInstance);
	}

    public BPELProcessInstanceRef getProcessInstance(String globalGUID) {
        return (BPELProcessInstanceRef) mGUI2Ref.get(globalGUID);
    }
    
    protected synchronized BPELProcessInstanceRef addProcessInstance(BPELProcessInstance instance) {
        BPELProcessRef processRef = (BPELProcessRefImpl)
            mTNS2Ref.get(instance.getBPELProcessManager().getBPELProcess().getTargetNamespace());
        if (processRef == null) {
            initializeProcessMap ();
        }
        BPELProcessInstanceRef instanceRef = ((BPELProcessRefImpl)processRef).wrapProcessInstance(instance);
        mGUI2Ref.put(instance.getId(), instanceRef);
        return instanceRef;
        
    }
    
    protected synchronized BPELProcessInstanceRef removeProcessInstance(BPELProcessInstance instance) {
       BPELProcessRefImpl processRef = (BPELProcessRefImpl)
            mTNS2Ref.get(instance.getBPELProcessManager().getBPELProcess().getTargetNamespace());
       if (processRef == null) {
           initializeProcessMap ();
       }       
       BPELProcessInstanceRef instanceRef = mGUI2Ref.remove(instance.getId());
       processRef.removeProcessInstance(instanceRef);       
       return instanceRef;
    }
    
    protected synchronized BPELProcessRef addProcess(RBPELProcess process) {
        BPELProcessManager manager = mEng.getBPELProcessManager(process.getBPELId());
        BPELProcessRefImpl ref = new BPELProcessRefImpl(manager, process);
        mTNS2Ref.put(process.getTargetNamespace(), ref);
        return ref;
        
    }
    
    protected synchronized BPELProcessRef removeProcess(RBPELProcess process) {
        return (BPELProcessRef) mTNS2Ref.remove(process.getTargetNamespace());
    }

}
