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
 * @(#)BPELPartnerLinkImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelatingSAInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import java.util.ArrayList;
import java.util.List;
import javax.xml.namespace.QName;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.WaitingCorrelatedEvent;

/**
 * Implementation of {@link BPELPartnerLink} which allows to query partner link
 * values in the target BPEL engine.
 * 
 * @author Sun Microsystems
 */
public class WaitingCorrelatedEventImpl implements WaitingCorrelatedEvent {
    
    private BPELProcessRefImpl myProcess;
    private CorrelatingSAInComingEventKeyImpl myEvent;
    
    public WaitingCorrelatedEventImpl(
            final BPELProcessRefImpl process,
            final CorrelatingSAInComingEventKeyImpl event) {
        myProcess = process;
        myEvent = event;
    }
    
    public long getId() {
        return myEvent.getId();
    }
    
    public String getPartnerLinkName() {
        return myEvent.getEventModel().getStartElement().getRPartner().getName();
    }
    
    public String[] getCorrelationSetNames() {
        final List corrVals = myEvent.getCorrIds();
        final List names = new ArrayList();
        
        for (int i = 0; i < corrVals.size(); i++) {
            final long setId = ((CorrelationVal) corrVals.get(i)).getSetID();
            final String setName = myProcess.getCorrelationSetName(setId);
            
            if (setName != null) {
                names.add(setName);
            }
        }
        
        if (names.size() > 0) {
            return (String[]) names.toArray(new String[names.size()]);
        } else {
            return null;
        }
    }
    
    public String getCorrelationSetValue(String setName) {
        final Long setId = myProcess.getCorrelationSetLongId(setName);
        
        if (setId != null) {
            final List corrVals = myEvent.getCorrIds();
            
            for (int i = 0; i < corrVals.size(); i++) {
                final CorrelationVal value = (CorrelationVal) corrVals.get(i);
                
                if (setId.longValue() == value.getSetID()) {
                    return value.toString();
                }
            }
        }
        
        return null;
    }
    
    public String[] getCorrelationSetPropertyNames(String setName) {
        final Long setId = myProcess.getCorrelationSetLongId(setName);
        
        if (setId != null) {
            final List corrVals = myEvent.getCorrIds();
            
            for (int i = 0; i < corrVals.size(); i++) {
                final CorrelationVal value = (CorrelationVal) corrVals.get(i);
                
                if (setId.longValue() == value.getSetID()) {
                    final int length = value.getPropertiesLength();
                    final String[] names = new String[length];
                    
                    for (int j = 0; j < length; j++) {
                        final QName propName = value.getPropertyName(j);
                        names[j] = propName.getNamespaceURI() + "\n" + 
                                propName.getPrefix() + "\n" + 
                                propName.getLocalPart();
                    }
                    
                    return names;
                }
            }
        }
        
        return null;
    }

    public String[] getCorrelationSetPropertyTypes(String setName) {
        final Long setId = myProcess.getCorrelationSetLongId(setName);
        
        if (setId != null) {
            final List corrVals = myEvent.getCorrIds();
            
            for (int i = 0; i < corrVals.size(); i++) {
                final CorrelationVal value = (CorrelationVal) corrVals.get(i);
                
                if (setId.longValue() == value.getSetID()) {
                    final int length = value.getPropertiesLength();
                    final String[] types = new String[length];
                    
                    for (int j = 0; j < length; j++) {
                        final QName propName = value.getPropertyType(j);
                        types[j] = propName.getNamespaceURI() + "\n" + 
                                propName.getPrefix() + "\n" + 
                                propName.getLocalPart();
                    }
                    
                    return types;
                }
            }
        }
        
        return null;
    }

    public String[] getCorrelationSetPropertyValues(String setName) {
        final Long setId = myProcess.getCorrelationSetLongId(setName);
        
        if (setId != null) {
            final List corrVals = myEvent.getCorrIds();
            
            for (int i = 0; i < corrVals.size(); i++) {
                final CorrelationVal value = (CorrelationVal) corrVals.get(i);
                
                if (setId.longValue() == value.getSetID()) {
                    final int length = value.getPropertiesLength();
                    final String[] values = new String[length];
                    
                    for (int j = 0; j < length; j++) {
                        values[j] = value.getPropertyValue(j);
                    }
                    
                    return values;
                }
            }
        }
        
        return null;
    }
}
