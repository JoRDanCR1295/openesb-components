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
 * @(#)BPELProcessInstanceRefImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessInstanceRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessRef;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.xml.namespace.QName;

/**
 * BPELProcessInstanceRef implementation.
 * @author Sun Microsystems
 * @version 
 */
public class BPELProcessInstanceRefImpl implements BPELProcessInstanceRef {
    
    private String mID, mSeq, mGUID;
    private BPELProcessRefImpl mTemplate;    
    private BPELProcessInstance mDelegate;
    
    private Map corrSetId2Value = new HashMap();
    
    protected BPELProcessInstanceRefImpl(BPELProcessRefImpl template, BPELProcessInstance delegate, String seqNo) {
        mSeq = seqNo;
        mDelegate = delegate;
        mTemplate = template;
        mID = BPELHelper.getSeq(delegate.getId()).toString();
        mGUID = delegate.getId();
    }
    
    public String globalID() {
        return mID;
    }
    
    public String globalGUID() {
        // TODO Auto-generated method stub
        return mGUID;
    }
    
    public String localID() {
        return mSeq;
    }
    
    public String getCorrelationSetValue(final String name) {
        final Long uid = mTemplate.getCorrelationSetLongId(name);
        
        if (uid != null) {
            final CorrelationVal value = getCorrelationSetValue(uid);
            
            if (value != null) {
                return value.toString();
            }
        }
        
        return null;
    }
    
    public String[] getCorrelationSetPropertyNames(final String name) {
        final Long uid = mTemplate.getCorrelationSetLongId(name);
        
        if (uid != null) {
            final CorrelationVal value = getCorrelationSetValue(uid);
            
            if (value != null) {
                final int length = value.getPropertiesLength();
                final String[] names = new String[length];
                
                for (int i = 0; i < length; i++) {
                    final QName propName = value.getPropertyName(i);
                    names[i] = propName.getNamespaceURI() + "\n" + 
                            propName.getPrefix() + "\n" + 
                            propName.getLocalPart();
                }
                
                return names;
            }
        }
        
        return null;
    }
    
    public String[] getCorrelationSetPropertyTypes(final String name) {
        final Long uid = mTemplate.getCorrelationSetLongId(name);
        
        if (uid != null) {
            final CorrelationVal value = getCorrelationSetValue(uid);
            
            if (value != null) {
                final int length = value.getPropertiesLength();
                final String[] types = new String[length];
                
                for (int i = 0; i < length; i++) {
                    final QName typeName = value.getPropertyType(i);
                    types[i] = typeName.getNamespaceURI() + "\n" + 
                            typeName.getPrefix() + "\n" + 
                            typeName.getLocalPart();
                }
                
                return types;
            }
        }
        
        return null;
    }
    
    public String[] getCorrelationSetPropertyValues(final String name) {
        final Long uid = mTemplate.getCorrelationSetLongId(name);
        
        if (uid != null) {
            final CorrelationVal value = getCorrelationSetValue(uid);
            
            if (value != null) {
                final int length = value.getPropertiesLength();
                final String[] values = new String[length];
                
                for (int i = 0; i < length; i++) {
                    values[i] = value.getPropertyValue(i);
                }
                
                return values;
            }
        }
        
        return null;
    }
    
    public BPELProcessRef template() {
        return mTemplate;
    }
    
    @Override
    public String toString() {
        return "{" + mTemplate.targetNamespace() + "} : instance" +  mID;
    }
    
    @Override
    public int hashCode () {
        return toString().hashCode();        
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if ((obj == null)
            || obj.getClass() != this.getClass()) {
            return false;
        }
        BPELProcessInstanceRef target = (BPELProcessInstanceRef) obj;
        return mID.equals(target.globalID());
    }
    
    // Protected ///////////////////////////////////////////////////////////////
    protected BPELProcessInstance getProcessInstance() {
        return mDelegate;
    }
    
    // Private /////////////////////////////////////////////////////////////////
    private CorrelationVal getCorrelationSetValue(final Long uid) {
        CorrelationVal value = (CorrelationVal) corrSetId2Value.get(uid);
        
        if (value == null) {
            final Collection vals = 
                    mDelegate.getInstanceAssociatedCorrVals();
                    
            final Iterator iterator = vals.iterator();
            while (iterator.hasNext()) {
                final CorrelationVal temp = 
                        (CorrelationVal) iterator.next();
                        
                if (temp.getSetID() == uid.longValue()) {
                    corrSetId2Value.put(uid, temp);
                    value = temp;
                    break;
                }
            }
        }
        
        return value;
    }
}
