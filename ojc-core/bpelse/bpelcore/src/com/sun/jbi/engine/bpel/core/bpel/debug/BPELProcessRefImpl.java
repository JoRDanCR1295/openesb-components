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
 * @(#)BPELProcessRefImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import com.sun.bpel.model.CorrelationSets;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;

import javax.xml.namespace.QName;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessInstanceRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.XMLElementRef;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RCorrelationSet;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelatingSAInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.WaitingCorrelatedEvent;

/**
 * BPELProcessInstance implementation.
 * @author Sun Microsystems
 * @version 
 */
public class BPELProcessRefImpl implements BPELProcessRef {
    private String mLoc;
    private QName mBPELId;
    private String mTNS;
    private int mSeq = 0;    
    private String mName;
    private String mUri;
    private Map<String, Integer> mGuid2Seq = 
            Collections.synchronizedMap(new WeakHashMap());
    private Map<String, BPELProcessInstanceRef> mInstances = 
            Collections.synchronizedMap(new WeakHashMap()); 
        
    private RBPELProcess mRBPELProcess;
    private BPELProcessManager mManager;
    
    private Map mCorrSetName2Id = new HashMap();
    private Map mCorrSetId2Name = new HashMap();
    
    private Map waitingCorrelatedEvents = new HashMap();
    
    public BPELProcessRefImpl(BPELProcessManager manager, RBPELProcess delegate) {
        mRBPELProcess = delegate;
        mManager = manager;
        mBPELId = delegate.getBPELId();
        
        // TODO this need to be fixed since the bpelid is now qname. for now 
        // setting to toString, need to revisit this.
        mLoc = delegate.getBPELId().toString();
        mTNS = delegate.getTargetNamespace();
        mName = delegate.getName();
        mUri = BPELHelper.getProcessURI(delegate);
        
        // Establish mapping between correlation sets ids and names
        final CorrelationSets sets = mRBPELProcess.getCorrelationSets();
        if (sets != null) {
            for (int i = 0; i < sets.getCorrelationSetSize(); i++) {
                final RCorrelationSet set = 
                        (RCorrelationSet) sets.getCorrelationSet(i);
                        
                final Long id = new Long(set.getUniqueId());
                
                mCorrSetName2Id.put(set.getName(), id);
                mCorrSetId2Name.put(id, set.getName());
            }
        }
    }
    
    public String location() {
        return mLoc;
    }
    
    public String targetNamespace() {
        return mTNS;
    }
    
    public String[] allProcessInstanceIDs() {
        Collection <String> ids = mInstances.keySet();
        String[] idArray = new String[ids.size()];
        int index = 0;
        for (String item : ids) {
            idArray[index] = item;
            index ++;           
        }
        return idArray;        
    }
    
    public BPELProcessInstanceRef getProcessInstance(String guid) {
        return (BPELProcessInstanceRef) mInstances.get(guid);
        
    }
    
    public String name() {
        // TODO Auto-generated method stub
        return mName;
    }

    public String uri() {
        // TODO Auto-generated method stub
        return mUri;
    }

    public XMLElementRef getXMLElement() {
        return new XMLElementRefImpl(mRBPELProcess);
    }
    
    public String[] allCorrelationSetsNames() {
        return (String[]) mCorrSetName2Id.keySet().toArray(
                new String[mCorrSetName2Id.size()]);
    }
    
    public long getCorrelationSetId(final String name) {
        Long id = (Long) mCorrSetName2Id.get(name);
        
        if (id == null) {
            return -1;
        } else {
            return id.longValue();
        }
    }
    
    public long[] getWaitingCorrelatedEventIds() {
        final List ids = new ArrayList();
        final List events = mManager.getCorrelatedWaitingEvents();
        
        for (int i = 0; i < events.size(); i++) {
            final CorrelatingSAInComingEventKeyImpl event = 
                    (CorrelatingSAInComingEventKeyImpl) events.get(i);
            
            ids.add(event.getId());
            
            if (waitingCorrelatedEvents.get(event.getId()) == null) {
                waitingCorrelatedEvents.put(
                        event.getId(), 
                        new WaitingCorrelatedEventImpl(this, event));
            }
        }
        
        final long[] longIds = new long[ids.size()];
        for (int i = 0; i < longIds.length; i++) {
            longIds[i] = ((Long) ids.get(i)).longValue();
        }
        
        return longIds;
    }
    
    public WaitingCorrelatedEvent getWaitingCorrelatedEvent(long eventId) {
        long[] ids = getWaitingCorrelatedEventIds();
        
        for (int i = 0; i < ids.length; i++) {
            if (ids[i] == eventId) {
                return (WaitingCorrelatedEvent) 
                        waitingCorrelatedEvents.get(eventId);
            }
        }
        
        final WaitingCorrelatedEventImpl eventImpl = (WaitingCorrelatedEventImpl) 
                waitingCorrelatedEvents.get(new Long(eventId));
        if (eventImpl != null) {
            waitingCorrelatedEvents.remove(new Long(eventId));
        }
        
        return eventImpl;
    }
    
    public String[] getCorrelationSetNames(long eventId) {
        final List events = mManager.getCorrelatedWaitingEvents();
        
        for (int i = 0; i < events.size(); i++) {
            final CorrelatingSAInComingEventKeyImpl event = 
                    (CorrelatingSAInComingEventKeyImpl) events.get(i);
            
            if (event.getId().longValue() == eventId) {
                final List names = new ArrayList();
                final List values = event.getCorrIds();
                
                for (int j = 0; i < values.size(); i++) {
                    CorrelationVal value = (CorrelationVal) values.get(j);
                    
                    names.add(mCorrSetId2Name.get(new Long(value.getSetID())));
                }
                
                return (String[]) names.toArray(new String[names.size()]);
            }
        }
        
        return null;
    }
    
    public String toString() {
        return mTNS;
    }
    
    public int hashCode () {
        return toString().hashCode();        
    }

    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if ((obj == null)
            || obj.getClass() != this.getClass()) {
            return false;
        }
        BPELProcessRef target = (BPELProcessRef) obj;
        return mTNS.equals(target.targetNamespace());
    }
    
    // Package /////////////////////////////////////////////////////////////////
    Long getCorrelationSetLongId(final String name) {
        return (Long) mCorrSetName2Id.get(name);
    }
    
    String getCorrelationSetName(final long id) {
        return (String) mCorrSetId2Name.get(new Long(id));
    }
    
    // Protected ///////////////////////////////////////////////////////////////
    protected synchronized BPELProcessInstanceRef wrapProcessInstance(BPELProcessInstance instance) {        
        int seq = fetchNextKey(instance.getId());
        BPELProcessInstanceRef ref =  new BPELProcessInstanceRefImpl(this, instance, String.valueOf(seq));  
        mInstances.put(ref.globalID(),ref);
        return ref;
    }

    protected synchronized void removeProcessInstance(BPELProcessInstanceRef instance) { 
        mInstances.remove(instance.globalID());
        mGuid2Seq.remove(instance.globalGUID());
    }
    
    // Private /////////////////////////////////////////////////////////////////
    private int fetchNextKey(String globalGUID) {
        int nextKey = -1;
        Integer key = (Integer) mGuid2Seq.get(globalGUID);
        if (key == null) {
            nextKey = ++mSeq;
            mGuid2Seq.put(globalGUID, nextKey);
        } else {
            nextKey = key.intValue();
        }
        return nextKey;
    }
}
