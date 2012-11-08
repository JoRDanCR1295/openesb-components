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
 * @(#)PickImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;pick&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class PickImpl extends ActivityImpl implements Pick {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 5159514492591609083L;
    
    /** Holds the onMessage sub-elements. */
    private ArrayList onMessages = new ArrayList();
    
    /** Holds the onAlarm sub-elements. */
    private ArrayList onAlarms = new ArrayList();
    
    /** Creates a new instance of PickImpl */
    public PickImpl() {
        super();
        initPick();
    }
    
    /** Creates a new instance of PickImpl.
     * @param   d   Owner document.
     */
    public PickImpl(XMLDocument d) {
        super(d);
        initPick();
    }
    
    /** Initializes this class.
     */
    private void initPick() {
        setLocalName(Pick.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[CREATE_INSTANCE] = new XMLAttributeImpl(Pick.ATTR.CREATE_INSTANCE, String.class, true, 
        		XMLAttribute.BOOLEAN_ENUM_VALS);

        childrenTags = new String[] {
            OnMessage.TAG,
            OnAlarm.TAG
        };
    }
    
    /** @see Pick#getCreateInstance
     */
    public String getCreateInstance() {
        return xmlAttrs[CREATE_INSTANCE].getValue();
    }
     
    /** @see Pick#setCreateInstance
     */
    public void setCreateInstance(String c) {
        setAttribute(CREATE_INSTANCE, c);
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof OnMessage) {
            addOnMessage((OnMessage) c);
        } else if (c instanceof OnAlarm) {
            addOnAlarm((OnAlarm) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof OnMessage) {
            removeOnMessage((OnMessage) c);
        } else if (c instanceof OnAlarm) {
            removeOnAlarm((OnAlarm) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see Pick#getOnMessage
     */
    public OnMessage getOnMessage(int i) {
        return (OnMessage) onMessages.get(i);
    }
    
    /** @see Pick#setOnMessage
     */
    public synchronized void setOnMessage(int i, OnMessage o) {
        if (onMessages.size() == i) {
            addOnMessage(o);
        } else {
            replaceChild(3, (OnMessage) onMessages.get(i), o);
            onMessages.set(i, o);
        }
    }
    
    /** @see Pick#addOnMessage(OnMessage)
     */
    public synchronized void addOnMessage(OnMessage o) {
        super.addChild(3, o);
        onMessages.add(o);
    }
    
    /** @see Pick#addOnMessage(int, OnMessage)
     */
    public synchronized void addOnMessage(int index, OnMessage o) {
        if ((index < 0) || (index > onMessages.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + onMessages.size());
        } else if (index == onMessages.size()) {
            addOnMessage(o);
        } else {
            super.addChild(3, getOnMessage(index), o);
            onMessages.add(index, o);
        }
    }
    
    /** @see Pick#clearOnMessages
     */
    public synchronized void clearOnMessages() {
        while (onMessages.size() > 0) {
            removeOnMessage(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see Pick#removeOnMessage(int)
     */
    public synchronized void removeOnMessage(int i) {
        removeOnMessage(getOnMessage(i));
    }
    
    /** @see Pick#removeOnMessage(OnMessage)
     */
    public synchronized boolean removeOnMessage(OnMessage o) {
        super.removeChild(o);
        return onMessages.remove(o);
    }
    
    /** @see Pick#getOnMessageSize
     */
    public int getOnMessageSize() {
        return onMessages.size();
    }
    
    /** @see Pick#indexOfOnMessage
     */
    public int indexOfOnMessage(XMLNode onMsg) {
        return onMessages.indexOf(onMsg);
    }
    
    /** @see Pick#getOnMessages
     */
    public synchronized Collection getOnMessages() {
        return Collections.unmodifiableCollection((ArrayList) onMessages.clone());
    }
    
    /** @see Pick#getOnAlarm
     */
    public OnAlarm getOnAlarm(int i) {
        return (OnAlarm) onAlarms.get(i);
    }
    
    /** @see Pick#setOnAlarm
     */
    public synchronized void setOnAlarm(int i, OnAlarm o) {
        if (onAlarms.size() == i) {
            addOnAlarm(o);
        } else {
            replaceChild(4, (OnAlarm) onAlarms.get(i), o);
            onAlarms.set(i, o);
        }
    }
    
    /** @see Pick#addOnAlarm(OnAlarm)
     */
    public synchronized void addOnAlarm(OnAlarm o) {
        super.addChild(4, o);
        onAlarms.add(o);
    }
    
    /** @see Pick#addOnAlarm(int, OnAlarm)
     */
    public synchronized void addOnAlarm(int index, OnAlarm o) {
        if ((index < 0) || (index > onAlarms.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + onAlarms.size());
        } else if (index == onAlarms.size()) {
            addOnAlarm(o);
        } else {
            super.addChild(4, getOnAlarm(index), o);
            onAlarms.add(index, o);
        }
    }
    
    /** @see Pick#clearOnAlarms
     */
    public synchronized void clearOnAlarms() {
        while (onAlarms.size() > 0) {
            removeOnAlarm(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see Pick#removeOnAlarm(int)
     */
    public synchronized void removeOnAlarm(int i) {
        removeOnAlarm(getOnAlarm(i));
    }
    
    /** @see Pick#removeOnAlarm(OnAlarm)
     */
    public synchronized boolean removeOnAlarm(OnAlarm o) {
        super.removeChild(o);
        return onAlarms.remove(o);
    }
    
    /** @see Pick#getOnAlarmSize
     */
    public int getOnAlarmSize() {
        return onAlarms.size();
    }
    
    /** @see Pick#indexOfOnAlarm
     */
    public int indexOfOnAlarm(XMLNode onAlarm) {
        return onAlarms.indexOf(onAlarm);
    }
    
    /** @see Pick#getOnAlarms
     */
    public synchronized Collection getOnAlarms() {
        return Collections.unmodifiableCollection((ArrayList) onAlarms.clone());
    }
    
    /** @see XMLNode#accept
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }

        if (!super.accept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }
}
