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
 * @(#)RBPELProcessImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.If;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.Otherwise;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.impl.BPELProcessImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RInvoke;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;


/**
 * Runtime BPEL Process implementation
 *
 * @author Sun Microsystems
 */
public class RBPELProcessImpl extends BPELProcessImpl implements RBPELProcess, ScopingElement {
    /**
     *
     */
    private static final long serialVersionUID = 3256727264572813369L;

    private Map mStartElementsMap = new HashMap();
    
    private Set mAllStartElems = null;
    
    private Set<Invoke> mAllInvokeElems = null;

    private boolean mIsEventHandlersDefined;
    /**
     * Creates a new RBPELProcessImpl object.
     *
     * @param bpeldoc runtime BPEL document
     */
    public RBPELProcessImpl(RBPELDocumentImpl bpeldoc) {
        super(bpeldoc);
    }

    /**
     * @see com.sun.bpel.model.meta.RBPELProcess#getBPELProcessManager()
     */
    /*public BPELProcessManager getBPELProcessManager() {
        return mBPELProcessManager;
    }*/

    /**
     * @see RBPELProcess#setBPELProcessManager(com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager)
     */
    /*public void setBPELProcessManager(BPELProcessManager bpelProcMgr) {
        mBPELProcessManager = bpelProcMgr;
    }*/

    /**
     * @see com.sun.bpel.model.meta.RBPELProcess#getStartElements()
     */
    public Set getStartElements() {
        if (mAllStartElems == null) {
            mAllStartElems = new HashSet();
            Set s1 = getStartElements(true);
            Set s2 = getStartElements(false);
            mAllStartElems.addAll(s1);
            mAllStartElems.addAll(s2);
        }
        
        return Collections.unmodifiableSet(mAllStartElems);
    }
    
    public Set<Invoke> getInvokeElements() {
    	if (mAllInvokeElems == null) {
    		mAllInvokeElems = new HashSet<Invoke>();
    		mAllInvokeElems.addAll(getInvokes());
    	}
    	return Collections.unmodifiableSet(mAllInvokeElems);
    }
    
    public RStartElement getStartElement(String partnerLink, String oper, 
    			String msgExchange) {
    	
    	Iterator iter = getStartElements().iterator();
    	while (iter.hasNext()) {
    		RStartElement elem = (RStartElement) iter.next();
    		String ePartnerLink = elem.getRPartner().getName();
    		String eOper = elem.getWSDLOperation().getName();
    		// TODO: in the future may have to check for the bpel message excahnage too???
    		if (partnerLink.equals(ePartnerLink) && oper.equals(eOper)) {
    			return elem;
    		}
    	}
    	return null;
    }
    
    public RVariable getVariable(long variableUniqueId) {
    	Variables vars = getVariables();
    	Iterator iter = vars.getVariables().iterator();
    	while (iter.hasNext()) {
    		RVariable var = (RVariable) iter.next();
    		if (variableUniqueId == var.getUniqueId()) {
    			return var;
    		}
    	}
    	return null;
    }


    /**
     * @see com.sun.bpel.model.meta.RBPELProcess#getStartElements(boolean)
     */
    public Set getStartElements(boolean createInstance) {
        Boolean bool = Boolean.valueOf(createInstance);
        Set result = (Set) mStartElementsMap.get(bool);

        if (result == null) {
            result = new HashSet();
            
            
            if (!createInstance) {
            	
                if (getEventHandlers() != null) {
                    processEventHandlers (getEventHandlers(), result);
                }
                
                if(getFaultHandlers() != null) {
                    processFaultHandlers(getFaultHandlers(), result);
                }
            }
            
            RActivity act = (RActivity) this.getActivity();
            getStartElements(act, result, createInstance);
            mStartElementsMap.put(bool, result);
        }
        
        return Collections.unmodifiableSet(result);
    }

    private void getStartElements(RActivity act, Set result, boolean createInstance) {
        if (act == null) {
            return;
        }
        
        if (act instanceof RStartElement) {
            // Receive will fall into this category
            RStartElement startElement = (RStartElement) act;

            if (checkCondition(startElement, createInstance)) {
                result.add(act);
            }
        } else if (act instanceof Pick) {
            Pick pick = (Pick) act;

            for (int i = 0, size = pick.getOnMessageSize(); i < size; i++) {
                RStartElement startElement = (RStartElement) pick.getOnMessage(i);

                if (checkCondition(startElement, createInstance)) {
                    result.add(startElement);
                }

                RActivity child = null;

                if (startElement instanceof RActivityHolder) {
                    child = ((RActivityHolder) startElement).getChildActivity();
                    getStartElements(child, result, createInstance);
                }
            }
        } else if (act instanceof If) {
            If ifBlock = (If) act;
            RActivity target = null;

            target = (RActivity) ifBlock.getActivity();            
            getStartElements(target, result, createInstance);
            
            Collection elseIfs = ifBlock.getElseIfs();
            

            if (elseIfs != null) {
                Iterator iter = elseIfs.iterator();

                while (iter.hasNext()) {
                    ElseIf elseIfClause = (ElseIf) iter.next();
                    target = (RActivity) elseIfClause.getActivity();
                    getStartElements(target, result, createInstance);
                }
            }

            Else elsee = (Else)ifBlock.getElse();

            if (elsee != null) {
                target = (RActivity) elsee.getActivity();
                getStartElements(target, result, createInstance);
            }            
        } else if (act instanceof Scope) {
        	
            if (!createInstance) {
                if (((Scope) act).getEventHandlers() != null) {
                    processEventHandlers(((Scope) act).getEventHandlers(), result);
                }
                if(((Scope)act).getFaultHandlers() != null) {
                    processFaultHandlers(((Scope)act).getFaultHandlers(), result);
                }
            	if(((Scope)act).getTerminationHandler() != null) {
            		getStartElements((RActivity)((Scope)act).getTerminationHandler().getActivity(), result, false);
            	}
            	if(((Scope)act).getCompensationHandler() != null) {
            		getStartElements((RActivity)((Scope)act).getCompensationHandler().getActivity(), result, false);
            	}
            }
        }

        RActivity child = null;

        if (act instanceof RActivityHolder) {
            child = ((RActivityHolder) act).getChildActivity();
            getStartElements(child, result, createInstance);
        }

        RActivity next = act.getNextActivity();
        getStartElements(next, result, createInstance);
    }
    
    private boolean checkCondition(RStartElement startElement, boolean initiateFlag) {
        return ((initiateFlag && startElement.getRCreateInstance()) ||
        (!(initiateFlag) && !(startElement.getRCreateInstance())));
    }
    
    private void processEventHandlers (EventHandlers eventHandlers, Set result) {
        EventHandlersOnEvent[] allOnEvents = eventHandlers.getOnEvents();
        if (allOnEvents != null && allOnEvents.length > 0) {
            for (int i=0; i<allOnEvents.length; i++) {
                getStartElements(((RActivity)allOnEvents[i]), result, false);
            }
        }  
        EventHandlersOnAlarm[] allOnAlarms = eventHandlers.getOnAlarms();
        if (allOnAlarms != null && allOnAlarms.length > 0) {
            for (int i=0; i<allOnAlarms.length; i++) {
                getStartElements(((RActivity) allOnAlarms[i]), result, false);
            }
        }
    }

    private void processFaultHandlers (FaultHandlers faultHandlers, Set result) {
    	
    	if (faultHandlers.getCatches() != null){
    		 Iterator iter = faultHandlers.getCatches().iterator();
    		 while (iter.hasNext()){
    			 Catch catchModel = (Catch) iter.next();
    			 getStartElements(((RActivity)catchModel.getActivity()), result, false);
    		 }
    	}
    	
    	if(faultHandlers.getCatchAll() != null) {
    		getStartElements(((RActivity)faultHandlers.getCatchAll().getActivity()), result, false);
    	}
    }
    
    /**
     * @see com.sun.bpel.model.meta.ScopingElement#getScopeId()
     */
    public long getScopeId() {
        return DEFAULT_PROCESS_SCOPE_ID.longValue();
    }

    /**
     * @see com.sun.bpel.model.meta.RActivityHolder#getChildActivity()
     */
    public RActivity getChildActivity() {
        return (RActivity) getActivity();
    }

    /**
     * @see RActivityHolder#setChildActivity(com.sun.bpel.model.meta.RActivity)
     */
    public void setChildActivity(RActivity act) {
        throw new UnsupportedOperationException();
    }

    /**
     * gets BPEL ID
     *
     * @return QName BPEL ID
     */
    public QName getBPELId() {
        String getTargetNameSpace =  ((BPELDocument) super.getOwnerDocument()).getTargetNamespace();
        return new QName(getTargetNameSpace, getName());
    }
    
    public void setEventHandlersDefined() {
    	this.mIsEventHandlersDefined = true;
    }
    
    public boolean isEventHandlersDefined() {
    	return mIsEventHandlersDefined;
    }
}
