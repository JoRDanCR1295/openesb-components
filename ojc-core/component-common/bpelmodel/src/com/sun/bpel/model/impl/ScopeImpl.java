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
 * @(#)ScopeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.EventHandlers;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.CompensationHandler;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.TerminationHandler;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.CorrelationSets;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.PartnerLinks;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements a &lt;scope&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ScopeImpl extends ActivityImpl implements Scope {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -7770872409875556197L;
    
    /** Holds value of property partners. */
    private PartnerLinks partners;
    
    /** Holds value of property containers */
    private Variables containers;
    
    /** Holds value of property correlationSets. */
    private CorrelationSets correlationSets;
    
    /** Holds faultHandlers sub-element. */
    private FaultHandlers faultHandlers;
    
    /** Holds compensationHandler sub-element. */
    private CompensationHandler compensationHandler;
    
    /** Holds terminationHandler sub-element. */
    private TerminationHandler terminationHandler;
    
    /** Holds value of property eventHandlers. */
    private EventHandlers eventHandlers;
    
    /** Holds activity sub-element. */
    private Activity activity;
    
    /** Creates a new instance of ScopeImpl */
    public ScopeImpl() {
        super();
        initScope();
    }
    
    /** Creates a new instance of ScopeImpl.
     * @param   d   Owner document.
     */
    public ScopeImpl(XMLDocument d) {
        super(d);
        initScope();
    }
    
    /** Initializes this class.
     */
    private void initScope() {
        setLocalName(Scope.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[ISOLATED] = new XMLAttributeImpl(Scope.ATTR.ISOLATED, String.class, false, 
        		XMLAttribute.BOOLEAN_ENUM_VALS); 

        childrenTags = new String[] {
    		PartnerLinks.TAG,
            Variables.TAG,
            CorrelationSets.TAG,
            FaultHandlers.TAG,
            CompensationHandler.TAG,
            TerminationHandler.TAG,            
            Activity.TAG
        };
    }
    
    /** Getter for isolated attribute.
     * @return  Value of isolated attribute.
     */
    public String getIsolated() {
        return xmlAttrs[ISOLATED].getValue();
    }
    
    /** Setter for isolated attribute.
     * @param   c   Value of isolated attribute.
     */
    public void setIsolated(String c) {
        setAttribute(ISOLATED, c);
    }
    
    /** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
    	if (c instanceof PartnerLinks) {
            setPartnerLinks((PartnerLinks) c);
        } else if (c instanceof Variables) {
            setVariables((Variables) c);
        } else if (c instanceof CorrelationSets) {
            setCorrelationSets((CorrelationSets) c);
        } else if (c instanceof FaultHandlers) {
            setFaultHandlers((FaultHandlers) c);
        } else if (c instanceof CompensationHandler) {
            setCompensationHandler((CompensationHandler) c);
        } else if (c instanceof TerminationHandler) {
            setTerminationHandler((TerminationHandler) c);
        } else if (c instanceof Activity) {
            setActivity((Activity) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
    	if (c instanceof PartnerLinks) {
            setPartnerLinks(null);
        } else if (c instanceof Variables) {
            setVariables(null);
        } else if (c instanceof CorrelationSets) {
            setCorrelationSets(null);
        } else if (c instanceof FaultHandlers) {
            setFaultHandlers(null);
        } else if (c instanceof CompensationHandler) {
            setCompensationHandler(null);
        } else if (c instanceof TerminationHandler) {
            setTerminationHandler(null);
        } else if (c instanceof Activity) {
            setActivity(null);
        } else {
            super.removeChild(c);
        }
    }
    
    
    /** Getter for property partners.
     * @return Value of property partners.
     *
     */
    public PartnerLinks getPartnerLinks() {
        return partners;
    }
    
    /** Setter for property partners.
     * @param partners New value of property partners.
     *
     */
    public void setPartnerLinks(PartnerLinks partners) {
    	PartnerLinks oldPartners = this.partners;
    	this.partners = partners;
        super.replaceChild(3, oldPartners, partners);
    }
    
    /** Getter for property containers.
     * @return  Value of property containers.
     */
    public Variables getVariables() {
        return containers;
    }
    
    /** Setter for property containers.
     * @param   containers  New value of property containers.
     */
    public void setVariables(Variables containers) {
    	Variables oldContainers = this.containers;
    	this.containers = containers;
        super.replaceChild(4, oldContainers, containers);
        
    }
    
    /** Getter for property correlationSets.
     * @return Value of property correlationSets.
     *
     */
    public CorrelationSets getCorrelationSets() {
        return correlationSets;
    }
    
    /** Setter for property correlationSets.
     * @param correlationSets New value of property correlationSets.
     *
     */
    public void setCorrelationSets(CorrelationSets correlationSets) {
    	CorrelationSets oldCorrelationSets = this.correlationSets;
    	this.correlationSets = correlationSets;
        super.replaceChild(5, oldCorrelationSets, correlationSets);
    }

	/** Getter for faultHandlers sub-element.
     * @return  faultHandlers sub-element.
     */
    public FaultHandlers getFaultHandlers() {
        return faultHandlers;
    }
    
    /** Setter for faultHandlers sub-element.
     * @param   f   faultHandlers sub-element.
     */
    public void setFaultHandlers(FaultHandlers f) {
        super.replaceChild(6, faultHandlers, f);
        faultHandlers = f;
    }
    
    /** Getter for compensationHandler sub-element.
     * @return  compensationHandler sub-element.
     */
    public CompensationHandler getCompensationHandler() {
        return compensationHandler;
    }
    
    /** Setter for compensationHandler sub-element.
     * @param   c   compensationHandler sub-element.
     */
    public void setCompensationHandler(CompensationHandler c) {
        super.replaceChild(7, compensationHandler, c);
        compensationHandler = c;
    }
    
    /** Getter for terminationHandler sub-element.
     * @return  terminationHandler sub-element.
     */
    public TerminationHandler getTerminationHandler() {
        return terminationHandler;
    }
    
    /** Setter for terminationHandler sub-element.
     * @param   t   terminationHandler sub-element.
     */
    public void setTerminationHandler(TerminationHandler t) {
        super.replaceChild(8, terminationHandler, t);
        terminationHandler = t;
    }
    
    /** Getter for property eventHandlers.
     * @return Value of property eventHandlers.
     *
     */
    public EventHandlers getEventHandlers() {
        return eventHandlers;
    }
    
    /** Setter for property eventHandlers.
     * @param eventHandlers New value of property eventHandlers.
     *
     */
    public void setEventHandlers(EventHandlers eventHandlers) {
        EventHandlers oldEventHandlers = this.eventHandlers;
    	this.eventHandlers = eventHandlers;
        super.replaceChild(9, oldEventHandlers, eventHandlers);
    }
    
    /** @see SingleActivityHolder#getActivity
     */
    public Activity getActivity() {
        return activity;
    }
    
    /** @see SingleActivityHolder#setActivity
     */
    public void setActivity(Activity a) {
        super.replaceChild(10, activity, a);
        activity = a;
    }
    
    
	/**
     * Get PartnerLink Given its name.
     * @param pName the name of the partnerLink
     * @return the partnerLink or null if not found
     */
    public PartnerLink getBPELPartnerLink(String pName) {
    	if(pName == null) {
    		return null;
    	}
    	
    	PartnerLink partnerLink = BPELHelper.getPartner(pName, this.partners);
    	
    	return partnerLink;
    }
     
    
    /**
     * Get Variable Given its name.
     * @param vName the name of the variable
     * @return the variable or null if not found
     */
    public Variable getBPELVariable(String vName) {
    	if(vName == null) {
    		return null;
    	}
    	
    	Variable variable = BPELHelper.getVariable(vName, this.containers);
    	
    	return variable;

    }
		

	
	public Collection getAllBPELOnMessage() {
		return BPELHelper.getAllBPELOnMessage(this);
	}

	public Collection getAllBPELReceive() {
		return BPELHelper.getAllBPELReceive(this);
	}
    
	
    /**
     * Get a collection of all bpel receive activities
     * used in this process which have given partnerLink. 
     * This will find out all the
     * nested receives as well.
     * @return Collection of bpel receive activities.
     */
    public Collection getAllBPELReceive(String partnerLinkName) {
    	return BPELHelper.getAllMatchingBPELReceive(partnerLinkName, this);
    }
    
    /**
     * Get a collection of all bpel onMessage activities
     * used in this process which have given partnerLinkName. 
     * This will find out all the
     * nested onMessage as well.
     * @return Collection of bpel onMessage activities.
     */
    public Collection getAllBPELOnMessage(String partnerLinkName) {
    	return BPELHelper.getAllMatchingBPELOnMessage(partnerLinkName, this);
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
    
    /** @see com.sun.bpel.model.BPELProcessOrScope#getCorrelationSet(java.lang.String)
     */
    public CorrelationSet getCorrelationSet(String name) {
        CorrelationSets sets = getCorrelationSets();
        CorrelationSet retVal = null;
        if (sets != null) {
            int setSize = sets.getCorrelationSetSize();

            CorrelationSet set = null;
            for (int i = 0; i < setSize; i++) {
                set = sets.getCorrelationSet(i);
                if (set.getName().equals(name)) {
                    retVal = set;
                    break;
                }
            }
        }
        return retVal;
    }  
}
