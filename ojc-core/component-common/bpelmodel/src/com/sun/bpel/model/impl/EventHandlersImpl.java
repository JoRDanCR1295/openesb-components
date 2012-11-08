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
 * @(#)EventHandlersImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;

import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 *
 * @author Sun Microsystems
 */
public class EventHandlersImpl extends ActivityImpl implements  EventHandlers {
    
    private ArrayList mEventHandlersOnEventArray = new ArrayList ();
    private ArrayList mEventHandlersOnAlarmArray = new ArrayList ();
    private EventHandlersOnEvent [] mEventHandlersOnEvents;
    private EventHandlersOnAlarm [] mEventHandlersOnAlarms;
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -117867370401509833L;
    

    /** Creates a new instance of EventHandlersImpl */
    public EventHandlersImpl() {
        super();
        initEventHandlers();
    }
    
    /** Creates a new instance of EventHandlersOnAlarmImpl.
     * @param   d   Owner document.
     */  
    public EventHandlersImpl(XMLDocument d) {
        super(d);
        // TODO Auto-generated constructor stub
        initEventHandlers();
    }
    
    /** Initializes this class. 
     */
    private void initEventHandlers() {
        setLocalName(EventHandlers.TAG);
        childrenTags = new String[] {
            EventHandlersOnEvent.TAG,
            EventHandlersOnAlarm.TAG,
        };
    }
    
    
    public EventHandlersOnEvent[] getOnEvents() {
        // TODO Auto-generated method stub
        if (mEventHandlersOnEvents == null) {
            mEventHandlersOnEvents = new EventHandlersOnEvent [mEventHandlersOnEventArray.size()];
            mEventHandlersOnEvents = (EventHandlersOnEvent []) mEventHandlersOnEventArray.toArray(mEventHandlersOnEvents);
        }
        return mEventHandlersOnEvents;
    }

    public EventHandlersOnEvent getOnEvent(int i) {
        // TODO Auto-generated method stub
        if (mEventHandlersOnEvents == null) {
            mEventHandlersOnEvents = new EventHandlersOnEvent [mEventHandlersOnEventArray.size()];
            mEventHandlersOnEvents = (EventHandlersOnEvent []) mEventHandlersOnEventArray.toArray(mEventHandlersOnEvents);
        }
        if (i > mEventHandlersOnEvents.length - 1) {
            return null;
        }
        return mEventHandlersOnEvents[i];

    }

    public void removeOnEvent(int i) {
        // TODO Auto-generated method stub
        if (i > mEventHandlersOnAlarmArray.size()-1 || i < 0) {
            return;
        }
        EventHandlersOnEvent onEvent = (EventHandlersOnEvent) mEventHandlersOnEventArray.remove(i);
        super.removeChild(onEvent);
        mEventHandlersOnEvents = null;
    }

    public void setOnEvents(EventHandlersOnEvent[] onEvent) {
        // TODO Auto-generated method stub
        if (onEvent != null) {
            mEventHandlersOnEvents = onEvent;
            mEventHandlersOnEventArray = new ArrayList (onEvent.length);
            for (int i=0 ; i<onEvent.length; i++) {
                mEventHandlersOnEventArray.add(i,onEvent[i]);
                super.addChild(onEvent[i]);
            }
        } else {
            mEventHandlersOnEventArray = new ArrayList ();
            mEventHandlersOnEvents = null;
            
        }
        
    }

    public void setOnEvent(EventHandlersOnEvent onEvent, int i) {
        // TODO Auto-generated method stub
        if (i > 0) {
            if (i < mEventHandlersOnEventArray.size() - 1) {
                EventHandlersOnEvent onEv = (EventHandlersOnEvent) mEventHandlersOnEventArray.get(i);
                super.removeChild(onEv);
                mEventHandlersOnEventArray.set(i, onEvent);
                mEventHandlersOnEvents[i] = onEvent;
                super.addChild(onEvent);
                
            }else {
                addOnEvent(onEvent);
            }
        }
        
    }

    public void addOnEvent(EventHandlersOnEvent onEvent) {
        // TODO Auto-generated method stub
        mEventHandlersOnEventArray.add(onEvent);
        super.addChild(onEvent);
        mEventHandlersOnEvents = null;
        
    }

    public void insertOnEvent(EventHandlersOnEvent onEvent, int i) {
        // TODO Auto-generated method stub
        if (i > 0) {
            if (i < mEventHandlersOnEventArray.size() - 1) {
                mEventHandlersOnEventArray.add(i, onEvent);
                super.addChild(onEvent);
                mEventHandlersOnEvents = null;
            }else {
                mEventHandlersOnEventArray.add(onEvent);
                super.addChild(onEvent);
                mEventHandlersOnEvents = null;
            }
        }  
    }

    public EventHandlersOnAlarm[] getOnAlarms() {
        // TODO Auto-generated method stub
        if (mEventHandlersOnAlarms == null) {
            mEventHandlersOnAlarms = new EventHandlersOnAlarm [mEventHandlersOnAlarmArray.size()];
            mEventHandlersOnAlarms = (EventHandlersOnAlarm []) mEventHandlersOnAlarmArray.toArray(mEventHandlersOnAlarms);
        }
        return mEventHandlersOnAlarms;
    }

    public EventHandlersOnAlarm getOnAlarm(int i) {
        // TODO Auto-generated method stub
        if (mEventHandlersOnAlarms == null) {
            mEventHandlersOnAlarms = new EventHandlersOnAlarm [mEventHandlersOnAlarmArray.size()];
            mEventHandlersOnAlarms = (EventHandlersOnAlarm []) mEventHandlersOnAlarmArray.toArray(mEventHandlersOnAlarms);
        }
        if (i > mEventHandlersOnAlarms.length - 1) {
            return null;
        }
        return mEventHandlersOnAlarms[i];

    }

    public void removeOnAlarm(int i) {
        // TODO Auto-generated method stub
        if (i > mEventHandlersOnAlarmArray.size()-1 || i < 0) {
            return;
        }
        EventHandlersOnAlarm onAlarm =  (EventHandlersOnAlarm) mEventHandlersOnAlarmArray.remove(i);
        super.removeChild(onAlarm);
        mEventHandlersOnAlarms = null;
    }

    public void setOnAlarms(EventHandlersOnAlarm[] onAlarm) {
        // TODO Auto-generated method stub
        if (onAlarm != null) {
            mEventHandlersOnAlarms = onAlarm;
            mEventHandlersOnAlarmArray = new ArrayList (onAlarm.length);
            for (int i=0 ; i<onAlarm.length; i++) {
                mEventHandlersOnAlarmArray.add(i,onAlarm[i]);
                super.addChild(onAlarm[i]);
            }
        } else {
            mEventHandlersOnAlarmArray = new ArrayList ();
            mEventHandlersOnAlarms = null;
            
        }
        
    }

    public void setOnAlarm(EventHandlersOnAlarm onEvent, int i) {
        // TODO Auto-generated method stub
        if (i > 0) {
            if (i < mEventHandlersOnAlarmArray.size() - 1) {
                EventHandlersOnAlarm onAlarm = (EventHandlersOnAlarm) mEventHandlersOnAlarmArray.get(i);
                super.removeChild(onAlarm);
                mEventHandlersOnAlarmArray.set(i, onEvent);
                mEventHandlersOnAlarms[i] = onEvent;
                super.addChild(onAlarm);
            }else {
                addOnAlarm(onEvent);
            }
        }
        
    }

    public void addOnAlarm(EventHandlersOnAlarm onEvent) {
        // TODO Auto-generated method stub
        mEventHandlersOnAlarmArray.add(onEvent);
        super.addChild(onEvent);
        mEventHandlersOnAlarms = null;

        
    }

    public void insertOnAlarm(EventHandlersOnAlarm onEvent, int i) {
        // TODO Auto-generated method stub
        if (i > 0) {
            if (i < mEventHandlersOnAlarmArray.size() - 1) {
                mEventHandlersOnAlarmArray.add(i, onEvent);
                mEventHandlersOnAlarms = null;
                super.addChild(onEvent);
            }else {
                mEventHandlersOnAlarmArray.add(onEvent);
                mEventHandlersOnAlarms = null;
            }
        }  
    }

    public int sizeOfOnEvent() {
        // TODO Auto-generated method stub
        return 0;
    }

    public int sizeOfOnAlarm() {
        // TODO Auto-generated method stub
        return 0;
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
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
