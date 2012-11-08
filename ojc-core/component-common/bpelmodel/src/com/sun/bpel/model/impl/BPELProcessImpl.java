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
 * @(#)BPELProcessImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;


import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;


import javax.wsdl.Definition;
import javax.wsdl.Part;
import javax.wsdl.Message;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;
import org.xml.sax.SAXException;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.CompensationHandler;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.CorrelationSets;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.ForEach;
import com.sun.bpel.model.If;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Otherwise;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.PartnerLinks;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.SingleActivityHolder;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.TerminationHandler;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.While;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.model.SystemFault;

import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.wsdl4j.ext.WSDL4JExt;
import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;

/**
 * Implements the &lt;process&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class BPELProcessImpl extends BPELElementImpl implements BPELProcess {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -5406240685156785276L;
        
    /** Holds value of property partners. */
    private PartnerLinks partners;
    
    /** Holds value of property containers */
    private Variables containers;
    
    /** Holds value of property correlationSets. */
    private CorrelationSets correlationSets;
    
    /** Holds value of property faultHandlers. */
    private FaultHandlers faultHandlers;
    
    /** Holds value of property compensationHandler. */
    private CompensationHandler compensationHandler;
    
    /** Holds value of property eventHandlers. */
    private EventHandlers eventHandlers;
    
    /** Holds value of property activity. */
    private Activity activity;
    
    private List imports;
    
    // a schema type loader that is responsible to look up schema components
    // reachable from the BPEL process.
    private SchemaTypeLoader schemaTypeLoader;
    
    private static Message SYSTEM_FAULT_MESSAGE;
    
    static {
    	SYSTEM_FAULT_MESSAGE = getSystemFaultMessage();
    }
    
    /** Creates a new instance of ProcessImpl */
    public BPELProcessImpl() {
        super();
        initBPELProcess();
    }
    
    /** Creates a new instance of BPELProcessImpl.
     * @param   d   Owner document.
     */
    public BPELProcessImpl(XMLDocument d) {
        super(d);
        initBPELProcess();
    }
    
    /** Initializes this class.
     */
    private void initBPELProcess() {
        setLocalName(BPELProcess.TAG);
        setDefaultNamespace(BPELDocument.BPEL_NAMESPACE);
        setNamespace(BPELDocument.BPEL_PREFIX, BPELDocument.BPEL_NAMESPACE);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.TARGET_NAMESPACE, String.class, false,
                                 null),
            new XMLAttributeImpl(ATTR.QUERY_LANGUAGE, String.class, true, null),
            new XMLAttributeImpl(ATTR.EXPRESSION_LANGUAGE, String.class, true,
                                 null),
            new XMLAttributeImpl(ATTR.SUPPRESS_JOIN_FAILURE, String.class, true,
                                 XMLAttribute.BOOLEAN_ENUM_VALS),
            new XMLAttributeImpl(ATTR.ENABLE_INSTANCE_COMPENSATION,
                                 String.class, true,
                                 XMLAttribute.BOOLEAN_ENUM_VALS),
            new XMLAttributeImpl(ATTR.ABSTRACT_PROCESS, String.class, true,
                                 XMLAttribute.BOOLEAN_ENUM_VALS),
            new XMLAttributeImpl(ATTR.ATOMIC, String.class, true,
                    			 XMLAttribute.BOOLEAN_ENUM_VALS),
            new XMLAttributeImpl(ATTR.IGNORE_MISSING_FROM_DATA, String.class, true,
       			 				 XMLAttribute.BOOLEAN_ENUM_VALS),
       		new XMLAttributeImpl(ATTR.WAITING_REQUEST_LIFE_SPAN, String.class, true,
       							 null),
            new XMLAttributeImpl(ATTR.PERSISTENCE_OPT_OUT, String.class, true,
            					 XMLAttribute.BOOLEAN_ENUM_VALS),
            new XMLAttributeImpl(ATTR.GENERATE_EVENTS, String.class, true,
                    			 XMLAttribute.BOOLEAN_ENUM_VALS),
            new XMLAttributeImpl(ATTR.ATOMIC_TX_TYPE, String.class, true,
            					 ATOMIC_TX_TYPES),
            new XMLAttributeImpl(ATTR.ENABLE_LOGGING, String.class, true,
            					 XMLAttribute.BOOLEAN_ENUM_VALS),
            new XMLAttributeImpl(ATTR.EXTRA_NDC, String.class, true, null)
        };
        childrenTags = new String[] {
            PartnerLinks.TAG,
            Variables.TAG,
            CorrelationSets.TAG,
            FaultHandlers.TAG,
            CompensationHandler.TAG,
            Activity.TAG
        };
        
        imports = new ArrayList();
        
    }
    
    /** Getter for property name.
     * @return Value of property name.
     *
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /** Setter for property name.
     * @param name  New value of property name.
     *
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    /** Setter for property name.
     * @param qName New qName of property name.
     * @param name  New value of property name.
     *
     */
    public void setName(String qName, String name) {
        setAttribute(NAME, qName, name);
    }
    
    /** Getter for property targetNamespace.
     * @return Value of property targetNamespace.
     *
     */
    public String getTargetNamespace() {
        return xmlAttrs[TARGET_NAMESPACE].getValue();
    }
    
    /** Setter for property targetNamespace.
     * @param targetNamespace   New value of property targetNamespace.
     *
     */
    public void setTargetNamespace(String targetNamespace) {
        setAttribute(TARGET_NAMESPACE, targetNamespace);
    }
    
    /** Setter for property targetNamespace.
     * @param qName             New qName of property targetNamespace.
     * @param targetNamespace   New value of property targetNamespace.
     *
     */
    public void setTargetNamespace(String qName, String targetNamespace) {
        setAttribute(TARGET_NAMESPACE, qName, targetNamespace);
    }
    
    /** Getter for property queryLanguage.
     * @return Value of property queryLanguage.
     *
     */
    public String getQueryLanguage() {
        return xmlAttrs[QUERY_LANGUAGE].getValue();
    }
    
    /** Setter for property name.
     * @param queryLanguage  New value of property queryLanguage.
     *
     */
    public void setQueryLanguage(String queryLanguage) {
        setAttribute(QUERY_LANGUAGE, queryLanguage);
    }
    
    /** Setter for property queryLanguage.
     * @param qName         New qName of property queryLanguage.
     * @param queryLanguage New value of property queryLanguage.
     *
     */
    public void setQueryLanguage(String qName, String queryLanguage) {
        setAttribute(QUERY_LANGUAGE, qName, queryLanguage);
    }
    
    /** Getter for property expressionLanguage.
     * @return Value of property expressionLanguage.
     *
     */
    public String getExpressionLanguage() {
        return xmlAttrs[EXPRESSION_LANGUAGE].getValue();
    }
    
    /** Setter for property expressionLanguage.
     * @param expressionLanguage    New value of property expressionLanguage.
     *
     */
    public void setExpressionLanguage(String expressionLanguage) {
        setAttribute(EXPRESSION_LANGUAGE, expressionLanguage);
    }
    
    /** Setter for property expressionLanguage.
     * @param qName                 New qName of property expressionLanguage.
     * @param expressionLanguage    New value of property expressionLanguage.
     *
     */
    public void setExpressionLanguage(String qName, String expressionLanguage) {
        setAttribute(EXPRESSION_LANGUAGE, qName, expressionLanguage);
    }
    
    /** Getter for property suppressJoinFailure.
     * @return Value of property suppressJoinFailure.
     *
     */
    public String getSuppressJoinFailure() {
        return xmlAttrs[SUPPRESS_JOIN_FAILURE].getValue();
    }
    
    /** Setter for property suppressJoinFailure.
     * @param suppressJoinFailure  New value of property suppressJoinFailure.
     *
     */
    public void setSuppressJoinFailure(String suppressJoinFailure) {
        setAttribute(SUPPRESS_JOIN_FAILURE, suppressJoinFailure);
    }
    
    /** Setter for property suppressJoinFailure.
     * @param qName                 New qName of property suppressJoinFailure.
     * @param suppressJoinFailure   New value of property suppressJoinFailure.
     *
     */
    public void setSuppressJoinFailure(String qName,
                                       String suppressJoinFailure) {
        setAttribute(SUPPRESS_JOIN_FAILURE, qName, suppressJoinFailure);
    }
    
    /** Getter for property enableInstanceCompensation.
     * @return Value of property enableInstanceCompensation.
     *
     */
    public String getEnableInstanceCompensation() {
        return xmlAttrs[ENABLE_INSTANCE_COMPENSATION].getValue();
    }
    
    /** Setter for property enableInstanceCompensation.
     * @param enableInstanceCompensation    New value of property
                                              enableInstanceCompensation.
     *
     */
    public void setEnableInstanceCompensation(
            String enableInstanceCompensation) {
        setAttribute(ENABLE_INSTANCE_COMPENSATION, enableInstanceCompensation);
    }
    
    /** Setter for property enableInstanceCompensation.
     * @param qName                         New qName of property
     *                                        enableInstanceCompensation.
     * @param enableInstanceCompensation    New value of property
     *                                        enableInstanceCompensation.
     *
     */
    public void setEnableInstanceCompensation(String qName,
            String enableInstanceCompensation) {
        setAttribute(ENABLE_INSTANCE_COMPENSATION, qName,
                     enableInstanceCompensation);
    }
    
    /** Getter for property abstractProcess.
     * @return Value of property abstractProcess.
     *
     */
    public String getAbstractProcess() {
        return xmlAttrs[ABSTRACT_PROCESS].getValue();
    }

    /** Setter for property abstractProcess.
     * @param abstractProcess New value of property abstractProcess.
     *
     */
    public void setAbstractProcess(String abstractProcess) {
        setAttribute(ABSTRACT_PROCESS, abstractProcess);
    }
    
    /** Setter for property abstractProcess.
     * @param qName             New qName of property abstractProcess.
     * @param abstractProcess   New value of property abstractProcess.
     *
     */
    public void setAbstractProcess(String qName, String abstractProcess) {
        setAttribute(ABSTRACT_PROCESS, qName, abstractProcess);
    }
    
    /** Getter for property ignoreMissingFromData
     * @return Value of property ignoreMissingFromData
     */
    public String getIgnoreMissingFromData() {
    	return xmlAttrs[IGNORE_MISSING_FROM_DATA].getValue();
    }
    
    /** Setter for property ignoreMissingFromData
     * @param ignoreMissingFromData New value of property ignoreMissingFromData
     */
    public void setIgnoreMissingFromData(String ignoreMissingFromData) {
    	setAttribute(IGNORE_MISSING_FROM_DATA, ignoreMissingFromData);
    }
    
    /** Setter for property ignoreMissingFromData
     * @param qName		New qName of property ignoreMissingFromData
     * @param ignoreMissingFromData	New value of property ignoreMissingFromData
     */
    public void setIgnoreMissingFromData(String qName, String ignoreMissingFromData) {
    	setAttribute(IGNORE_MISSING_FROM_DATA, qName, ignoreMissingFromData);
    }
    
    /** Getter for property peristenceOptOut
     * @return Value of property peristenceOptOut
     */
	public String getPersistenceOptOut() {
		return xmlAttrs[PERSISTENCE_OPT_OUT].getValue();
	}

	/** Setter for property peristenceOptOut
     * @param peristenceOptOut New value of property peristenceOptOut
     */
	public void setPersistenceOptOut(String persistenceOptOut) {
		setAttribute(PERSISTENCE_OPT_OUT, persistenceOptOut);
	}

	/** Setter for property peristenceOptOut
     * @param qName		New qName of property peristenceOptOut
     * @param peristenceOptOut	New value of property peristenceOptOut
     */
	public void setPersistenceOptOut(String qName, String persistenceOptOut) {
		setAttribute(PERSISTENCE_OPT_OUT, qName, persistenceOptOut);
	}
	
    /** Getter for property generateEvents
     * @return Value of property generateEvents
     */
	public String getGenerateEvents() {
		return xmlAttrs[GENERATE_EVENTS].getValue();
	}

	/** Setter for property generateEvents
     * @param generateEvents New value of property generateEvents
     */
	public void setGenerateEvents(String generateEvents) {
		setAttribute(GENERATE_EVENTS, generateEvents);
	}

	/** Setter for property generateEvents
     * @param qName		New qName of property generateEvents
     * @param generateEvents	New value of property generateEvents
     */
	public void setGenerateEvents(String qName, String generateEvents) {
		setAttribute(GENERATE_EVENTS, qName, generateEvents);
	}

	/** Getter for property waitingRequestLifeSpan
     * @return Value of property waitingRequestLifeSpan
     */
    public String getWaitingRequestLifeSpan() {
    	return xmlAttrs[WAITING_REQUEST_LIFE_SPAN].getValue();
    }
    
    /** Setter for property waitingRequestLifeSpan
     * @param waitingRequestLifeSpan New value of property waitingRequestLifeSpan
     */
    public void setWaitingRequestLifeSpan(String waitingRequestLifeSpan) {
    	setAttribute(WAITING_REQUEST_LIFE_SPAN, waitingRequestLifeSpan);
    }
    
    /** Setter for property waitingRequestLifeSpan
     * @param qName		New qName of property waitingRequestLifeSpan
     * @param ignoreMissingFromData	New value of property waitingRequestLifeSpan
     */
    public void setWaitingRequestLifeSpan(String qName, String waitingRequestLifeSpan) {
    	setAttribute(WAITING_REQUEST_LIFE_SPAN, qName, waitingRequestLifeSpan);
    }
    
    /** Getter for property atomic
     * @return Value of property atomic
     */
    public String getAtomic() {
    	return xmlAttrs[ATOMIC].getValue();
    }
    
    /** Setter for property atomic
     * @param atomic New value of property atomic
     */
    public void setAtomic(String atomic) {
    	setAttribute(ATOMIC, atomic);
    }
    
    /** Setter for property atomic
     * @param qName		New qName of property atomic
     * @param atomic	New value of property atomic
     */
    public void setAtomic(String qName, String atomic) {
    	setAttribute(ATOMIC, qName, atomic);
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
        } else if (c instanceof Activity) {
            setActivity((Activity) c);
        } else if (c instanceof Import) {
        	addImport((Import) c);
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
        } else if (c instanceof Activity) {
            setActivity(null);
        } else if (c instanceof Import) {
        	removeImport((Import) c);
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
        super.replaceChild(2, oldPartners, partners);
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
        super.replaceChild(3, oldContainers, containers);
        
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
        super.replaceChild(4, oldCorrelationSets, correlationSets);
    }
    
    /** Getter for property faultHandlers.
     * @return Value of property faultHandlers.
     *
     */
    public FaultHandlers getFaultHandlers() {
        return faultHandlers;
    }
    
    /** Setter for property faultHandlers.
     * @param faultHandlers New value of property faultHandlers.
     *
     */
    public void setFaultHandlers(FaultHandlers faultHandlers) {
    	FaultHandlers oldFaultHandlers = this.faultHandlers;
    	this.faultHandlers = faultHandlers;
        super.replaceChild(5, oldFaultHandlers, faultHandlers);
    }
    
    /** Getter for property compensationHandlers.
     * @return Value of property compensationHandlers.
     *
     */
    public CompensationHandler getCompensationHandler() {
        return compensationHandler;
    }
    
    /** Setter for property compensationHandlers.
     * @param compensationHandler New value of property compensationHandlers.
     *
     */
    public void setCompensationHandler(
            CompensationHandler compensationHandler) {
    	CompensationHandler oldCompensationHandler = this.compensationHandler;
    	this.compensationHandler = compensationHandler;
    	super.replaceChild(6, oldCompensationHandler, compensationHandler);
        
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
        super.replaceChild(7, oldEventHandlers, eventHandlers);
    }
        
    /** @see SingleActivityHolder#getActivity
     */
    public Activity getActivity() {
        return activity;
    }
    
    /** @see SingleActivityHolder#setActivity
     */
    public void setActivity(Activity activity) {
    	Activity oldActivity = this.activity;
    	this.activity = activity;
    	super.replaceChild(8, oldActivity, activity);
        
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

    /** 
     * Get all the source partners - partners associated with receives and 
     * onMessages
     * @return List An arrayList of partner objects
     */
    public List getSourcePartners() {

        List sourcePartners = new ArrayList();

        List receiveActivities = getActivities("com.sun.bpel.model.Receive");
        for (int i = 0; i < receiveActivities.size(); i++) {
            XMLNode node = (XMLNode) receiveActivities.get(i);
            if (node instanceof Receive) {
                String partnerName = ((Receive) node).getPartnerLink();
                sourcePartners.add(BPELHelper.getPartner(partnerName, this.partners));
            }
        }

        List onMessages = getActivities("com.sun.bpel.model.OnMessage"); 
        for (int i = 0; i < onMessages.size(); i++) {
            XMLNode node = (XMLNode) onMessages.get(i);
            if (node instanceof OnMessage) {
                String partnerName = ((OnMessage) node).getPartnerLink();
                sourcePartners.add(BPELHelper.getPartner(partnerName, this.partners));
            }
        }

        return sourcePartners;
    }

    /** 
     * Get all the destination partners - partners associated with invokes
     * @return List An arrayList of partner objects
     */
    public List getDestinationPartners() {
        List destPartners = new ArrayList();

        List invokeActivities = getActivities("com.sun.bpel.model.Invoke");

        for (int i = 0; i < invokeActivities.size(); i++) {
            XMLNode node = (XMLNode) invokeActivities.get(i);
            if (node instanceof Invoke) {
                String partnerName = ((Invoke) node).getPartnerLink();
                destPartners.add(BPELHelper.getPartner(partnerName, this.partners));
            }
        }

        return destPartners;
    }

    /**
     * Get all the invokes in the business process
     * @return List An arrayList of activities
     */
    public List getInvokes() {
        List invokeActivities = getActivities("com.sun.bpel.model.Invoke");
        return invokeActivities;
    }

    /**
     * Get all the activies of a particular type in the business process
     * @param type - Type of activities that needs to be obtained
     * @return List An arrayList of activities
     */
    public List getActivities(String type) {
        List activities = new ArrayList();
        getActivities(type, activity, activities);
        
        getEventHandlerActivities(getEventHandlers(), type, activities);
        getFaultHandlerActivities(type, activities);
        getCompensationHandlerActivities(type, activities);
        return activities;
    }

    /**
     * Get all the activies of a particular type in the given activity
     * @param type - Type of activities that needs to be obtained
     * @param currentActivity - the activity that is the root of the activity tree
     * @return List An arrayList of activities
     */
    public List getActivities(String type, Activity currentActivity) {
        List activities = new ArrayList();
        getActivities(type, currentActivity, activities);
        return activities;
    }

    /**
     * Get the activities in the tree of activities of a particular type.
     * @param type - Type of activities that needs to be obtained
     * @param currentActivity - the activity that is the root of the activity tree
     * @param result - the list of activities to be returned
     */
    private void getActivities(String type, Activity currentActivity,
                               List result) {
        if (currentActivity == null || type == null || "".equals(type.trim())) {
            return;
        }

        if (currentActivity instanceof Receive && 
            "com.sun.bpel.model.Receive".equals(type)) {
            Receive receive = (Receive) currentActivity;
            result.add(receive);
        } else if (currentActivity instanceof Pick) {
            Pick pick = (Pick) currentActivity;
            for (int i = 0; i < pick.getOnMessageSize(); i++) {
                OnMessage onMessage = pick.getOnMessage(i);
                if (onMessage != null) {
                    if ("com.sun.bpel.model.OnMessage".equals(type)) {
                        result.add(onMessage);
                    }
                    
                    getActivities(type, onMessage.getActivity(), result);
                }
            }
            
            for (int i = 0; i < pick.getOnAlarmSize(); i++) {
                OnAlarm onAlarm = pick.getOnAlarm(i);
                if (onAlarm != null) {
                    getActivities(type, onAlarm.getActivity(), result);
                }
            }
        } else if (currentActivity instanceof Reply &&
                   "com.sun.bpel.model.Reply".equals(type)) {
            Reply replyAct = (Reply) currentActivity;
            result.add(replyAct);
        } else if (currentActivity instanceof Throw 
                   && "com.sun.bpel.model.Throw".equals(type)) {
            Throw throwAct = (Throw) currentActivity;
            result.add(throwAct);
        } else if (currentActivity instanceof Sequence) {
            Sequence sequence = (Sequence) currentActivity;
            for (int i = 0; i < sequence.getActivitySize(); i++) {
                getActivities(type, sequence.getActivity(i), result);
            }
        } else if (currentActivity instanceof Flow) {
            Flow flow = (Flow) currentActivity;
            for (int i = 0; i < flow.getActivitySize(); i++) {
                getActivities(type, flow.getActivity(i), result);
            }
        } else if (currentActivity instanceof Switch) {
            Switch switchAct = (Switch) currentActivity;
            for (int i = 0; i < switchAct.getCaseSize(); i++) {
                Activity act = ((Case) switchAct.getCase(i)).getActivity();
                getActivities(type, act, result);
            }
            
            Otherwise otherWise = switchAct.getOtherwise();
            if (otherWise != null) {
                getActivities(type, otherWise.getActivity(), result);
            }
        } else if ((currentActivity instanceof While) 
                || (currentActivity instanceof RepeatUntil)
                || (currentActivity instanceof ForEach)) {
            getActivities(type, ((SingleActivityHolder) currentActivity).getActivity(), result);

        } else if (currentActivity instanceof If) {
            If ifBlock = (If) currentActivity;
            Activity target = null;

            target = ifBlock.getActivity();            
            getActivities(type, target, result);
            
            Collection elseIfs = ifBlock.getElseIfs();
            

            if (elseIfs != null) {
                Iterator iter = elseIfs.iterator();

                while (iter.hasNext()) {
                    ElseIf elseIfClause = (ElseIf) iter.next();
                    target = elseIfClause.getActivity();
                    getActivities(type, target, result);
                }
            }

            Else elsee = (Else)ifBlock.getElse();

            if (elsee != null) {
                target = elsee.getActivity();
                getActivities(type, target, result);
            }
            
        } else if (currentActivity instanceof Invoke) {
            // handle catch
            for (int catchIdx = 0; 
                 catchIdx < ((Invoke) currentActivity).getCatchSize(); catchIdx++) {

                com.sun.bpel.model.Catch catchFault = 
                    ((Invoke) currentActivity).getCatch(catchIdx);
                Activity tempActivity = catchFault.getActivity();
                if (tempActivity != null) {
                    // now only looking for receives
                    getActivities(type, tempActivity, result);
                }
            }

            // handle catchAll
            CatchAll catchAll = ((Invoke) currentActivity).getCatchAll();
            if (catchAll != null) {
                Activity tempActivity = catchAll.getActivity();
                if (tempActivity != null) {
                    // now only looking for receives
                    getActivities(type, tempActivity, result);
                }
            }

            // handle compensationHandler
            CompensationHandler compHandler = 
                ((Invoke) currentActivity).getCompensationHandler();
            if (compHandler != null) {
                Activity compensationActivity = 
                    compHandler.getActivity();
                if (compensationActivity != null) {
                    getActivities(type, compensationActivity, result);
                }
            }

            if ("com.sun.bpel.model.Invoke".equals(type)) {
                result.add((Invoke) currentActivity);
            }
        } else if (currentActivity instanceof Scope) {

            getActivities(type, ((Scope) currentActivity).getActivity(), result);

            EventHandlers eventHandler = ((Scope) currentActivity).getEventHandlers();
            if (eventHandler != null) {
            	getEventHandlerActivities(eventHandler, type, result);
            }
            
            FaultHandlers faultHandler = ((Scope) currentActivity).getFaultHandlers();

            if (faultHandler != null) {
                // handle catch
                for (int catchIdx = 0; 
                     catchIdx < faultHandler.getCatchSize(); catchIdx++) {
                    
                    com.sun.bpel.model.Catch catchFault = 
                        faultHandler.getCatch(catchIdx);
                    Activity tempActivity = catchFault.getActivity();
                    if (tempActivity != null) {
                        // now only looking for receives
                        getActivities(type, tempActivity, result);
                    }
                }
                
                // handle catchAll
                CatchAll catchAll = faultHandler.getCatchAll();
                if (catchAll != null) {
                    Activity tempActivity = catchAll.getActivity();
                    if (tempActivity != null) {
                        // now only looking for receives
                        getActivities(type, tempActivity, result);
                    }
                }
            }

            // handle compensationHandler
            CompensationHandler compHandler = 
                ((Scope) currentActivity).getCompensationHandler();
            if (compHandler != null) {
                Activity compensationActivity = 
                    compHandler.getActivity();
                if (compensationActivity != null) {
                    getActivities(type, compensationActivity, result);
                }
            }
            
            // handle terminationHandler
            TerminationHandler terminationHandler = 
                ((Scope) currentActivity).getTerminationHandler();
            if (terminationHandler != null) {
                Activity terminationActivity = 
                	terminationHandler.getActivity();
                if (terminationActivity != null) {
                    getActivities(type, terminationActivity, result);
                }
            }            
        }
    }
    
    private void getEventHandlerActivities(EventHandlers eventHandler, 
    		String type, List result) {

    	if (eventHandlers != null) {
        	// handle eventHandler defined on scope.
        	EventHandlersOnEvent[] allOnEvents = eventHandler.getOnEvents();
            if (allOnEvents != null && allOnEvents.length > 0) {
                for (int i=0; i<allOnEvents.length; i++) {
                	Activity tempActivity = ((EventHandlersOnEvent) allOnEvents[i]).getActivity();
                	if (tempActivity != null) {
                		getActivities(type, tempActivity, result);
                	}
                }
            }  

            EventHandlersOnAlarm[] allOnAlarms = eventHandler.getOnAlarms();
            if (allOnAlarms != null && allOnAlarms.length > 0) {
                for (int i=0; i<allOnAlarms.length; i++) {
                	Activity tempActivity = ((EventHandlersOnAlarm) allOnAlarms[i]).getActivity();
                	if (tempActivity != null) {
                		getActivities(type, tempActivity, result);
                	}
                }
            }
    		
    	}
    }

    /**
     * Get the activities in the tree of activities of a particular type.
     * The type is either start or end
     * @param type - Type of activities that needs to be obtained - 
     * start or end
     * @return the list of partners or activities
     */
    private void getFaultHandlerActivities(String type, List result) {

        FaultHandlers faultHandler = getFaultHandlers();

        if (faultHandler != null) {
            // handle catch
            for (int catchIdx = 0; 
                 catchIdx < faultHandler.getCatchSize(); catchIdx++) {
                    
                com.sun.bpel.model.Catch catchFault = 
                    faultHandler.getCatch(catchIdx);
                Activity tempActivity = catchFault.getActivity();
                if (tempActivity != null) {
                    // now only looking for receives
                    getActivities(type, tempActivity, result);
                }
            }
                
            // handle catchAll
            CatchAll catchAll = faultHandler.getCatchAll();
            if (catchAll != null) {
                Activity tempActivity = catchAll.getActivity();
                if (tempActivity != null) {
                    // now only looking for receives
                    getActivities(type, tempActivity, result);
                }
            }
        }
    }

    /**
     * Get the activities in the tree of activities of a particular type.
     * The type is either start or end
     * @param type - Type of activities that needs to be obtained - 
     * start or end
     * @return the list of partners or activities
     */
    private void getCompensationHandlerActivities(String type, List result) {
        // handle compensationHandler
        CompensationHandler compHandler = getCompensationHandler(); 

        if (compHandler != null) {
            Activity compensationActivity = 
                compHandler.getActivity();
            if (compensationActivity != null) {
                getActivities(type, compensationActivity, result);
            }
        }
    }
    
    /**
     * add a new import element.
     * The <import> element is used within a BPEL4WS process to explicitly indicate a dependency on
     *  external XML Schema or WSDL definitions. Any number of <import> elements may appear as
     *  initial children of the <process> element, before any other child element.
     *  
     * @param newImport new Import element.
     */
    public void addImport(Import newImport) {
    	imports.add(newImport);
    	super.addChild(1, newImport);
    }
    
    /**
     * remove an old import element.
     * The <import> element is used within a BPEL4WS process to explicitly indicate a dependency on
     *  external XML Schema or WSDL definitions. Any number of <import> elements may appear as
     *  initial children of the <process> element, before any other child element.
     *  
     * @param oldImport new Import element.
     */
    public void removeImport(Import oldImport) {
    	imports.remove(oldImport);
    	super.removeChild(oldImport);
    	//null out imported document object reference
    	oldImport.setImportedObject(null);
    }
    
    /**
     * get the list of all import elements.
     * The <import> element is used within a BPEL4WS process to explicitly indicate a dependency on
     *  external XML Schema or WSDL definitions. Any number of <import> elements may appear as
     *  initial children of the <process> element, before any other child element.
     *  
     * @return List list of Import objects
     */
    public List getImports() {
    	return imports;
    }
    
    
    
    public List getImports(String namespace) {
    	if(namespace == null) {
    		return Collections.EMPTY_LIST;
    	}
    	
		ArrayList matchingImports = new ArrayList();
		List allImports = getImports();
		Iterator it = allImports.iterator();
		while(it.hasNext()) {
			Import imp = (Import) it.next();
			if(namespace.equals(imp.getNamespace())) {
				matchingImports.add(imp);
			}
		}
		return matchingImports;
	}
    

	public Collection getAllImportedWSDLDefinitions() {
    	ArrayList wsdlDefinitions = new ArrayList();
    	
    	List imports = this.getImports();
    	if(imports != null) {
    		Iterator it = imports.iterator();
    		while(it.hasNext()) {
    			Import  imp = (Import) it.next();
    			Object impDocument = imp.getImportedObject();
    			if(impDocument instanceof WSDLDocument) {
    				WSDLDocument doc = (WSDLDocument) impDocument;
    				Definition def = doc.getDefinition();
    				if(def != null) {
    					wsdlDefinitions.add(def);
    				}
    			}
    		}
    	}
    	
    	return wsdlDefinitions;
	}

    
	public Collection getImportedWSDLDefinitions(String targetNamespace) {
		if(targetNamespace == null) {
			return Collections.EMPTY_LIST;
		}
		Collection matchingWsdlDefinitions = new ArrayList();
		Collection wsdlDefinitions = getAllImportedWSDLDefinitions();
    	Iterator it = wsdlDefinitions.iterator();
		while(it.hasNext()) {
			Definition wsdlDef = (Definition) it.next();
			if(targetNamespace.equals(wsdlDef.getTargetNamespace())) {
				matchingWsdlDefinitions.add(wsdlDef);
			}
		}
		return matchingWsdlDefinitions;
	}

	
	public Collection getAllImportedXMLSchemas() {
		ArrayList schemas = new ArrayList();
    	
    	List imports = this.getImports();
    	if(imports != null) {
    		Iterator it = imports.iterator();
    		while(it.hasNext()) {
    			Import  imp = (Import) it.next();
    			Object impDocument = imp.getImportedObject();
    			if(impDocument instanceof XMLSchema) {
    				schemas.add(impDocument);
    			}
    		}
    	}
    	
    	return schemas;
	}

	public Collection getImportedXMLSchemas(String targetNamespace) {
		if(targetNamespace == null) {
			return Collections.EMPTY_LIST;
		}
		Collection matchingXMLSchemas = new ArrayList();
		Collection xmlSchemas = getAllImportedXMLSchemas();
    	Iterator it = xmlSchemas.iterator();
		while(it.hasNext()) {
			XMLSchema xmlSchema = (XMLSchema) it.next();
			Schema schema = (Schema) xmlSchema.getSchema();
			if(targetNamespace.equals(schema.getTargetNamespace())) {
				matchingXMLSchemas.add(xmlSchema);
			}
		}
		return matchingXMLSchemas;
	}

	/**
	 * Get WSDLMessage Given its QName. Look into all imported WSDL for given
	 * message
	 * 
	 * @param qName
	 *            QName of the message
	 * @return WSDLMessage
	 */
	public Message getWSDLMessage(QName qName) {
		if (qName == null) {
			return null;
		}
		Collection wsdlDefinitions = getAllImportedWSDLDefinitions();

		/**
		 * processing of the WSDL defintions importing from BPELDocument.
		 */
		Iterator it = wsdlDefinitions.iterator();
		while (it.hasNext()) {
			Definition wsdlDefination = (Definition) it.next();
			Message msg = wsdlDefination.getMessage(qName);
			if (msg != null) {
				return msg;
			}
		}
		/**
		 * CR 6809609: Check for System Fault Message. The check is required to
		 * populate the WSDL message for the system fault in the model. When the
		 * check returns true, the Default system fault WSDLDef is created and
		 * WSDL message is populated in the variable definition.
		 */
		if (isSystemFault(qName)) {
			return SYSTEM_FAULT_MESSAGE;
		}
		return null;
	}
    
    
    
    /**
     * isSystemFault: checks if the Qname is of System fault message type.
     * @param name - QName 
     * @return true - System fault type, false otherwise.
     */
    private boolean isSystemFault(QName name){
    	boolean sysFault = false;
    	if( SystemFault.MESSAGE_QNAME.equals( name )){
    		sysFault = true;
    	}
    	return sysFault;
    }
    
    /**
     * If systemfault type, construct a System Fault WSDL definition which is 
     * internal fault structure used at runtime to populate the faultVar of the system
     * fault. 
     * 
     * @param wsdlDefinitions
     * @return System Fault message model 
     */
    private static Message getSystemFaultMessage(){

    	// Creates a WSDL definition with the QName and URI of the SystemFault.
    	Definition wsdlDef = WSDL4JExt.newDefinition();
    	QName msgType = SystemFault.MESSAGE_QNAME;
    	wsdlDef.setTargetNamespace(msgType.getNamespaceURI());
    	wsdlDef.setQName(msgType);

    	// Create a System fault message and add to the WSDL Definition.
    	Message msgDef = wsdlDef.createMessage();
    	wsdlDef.addMessage(msgDef);
    	msgDef.setQName(msgType);
    	Part part = wsdlDef.createPart();
    	part.setName(SystemFault.MESSAGE_PARTNAME);
    	part.setTypeName(new QName(SystemFault.XML_NAMESPACE_URL, 
    			SystemFault.XMS_ANY_TYPE,
    			SystemFault.XML_TYPE ));
    	msgDef.addPart(part);

    	return msgDef;
    }
    
    
    
    /**
     * Get PortType Given its QName.
     * @param qName the name of the port type
     * @return the port type or null if not found
     */
    public PortType getPortType(QName qName) {
    	QName nsQName = qName;
    	if(nsQName == null) {
    		return null;
    	}
    	
    	PortType portType = null;
    	Collection wsdlDefinitions = getAllImportedWSDLDefinitions();
    	Iterator it = wsdlDefinitions.iterator();
		while(it.hasNext()) {
			Definition wsdlDefination = (Definition) it.next();
			PortType pt = wsdlDefination.getPortType(nsQName);
			if(pt != null) {
				portType = pt;
				break;
			}
		}
		
    	return portType;
    }
   
    
    
    public Collection getAllPortTypes() {
		ArrayList allPortTypes = new ArrayList();
		
	    Collection wsdlDefinitions = getAllImportedWSDLDefinitions();
        Iterator iter = wsdlDefinitions.iterator();
        while (iter.hasNext()) {
            Definition wsdlDefinition = (Definition) iter.next();
            allPortTypes.addAll(wsdlDefinition.getPortTypes().values());
        }
        
        return allPortTypes;
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

	public Collection getBPELVariables(QName messageQName) {
		if(messageQName == null) {
			return Collections.EMPTY_LIST;
		}
		
		String namespace = messageQName.getNamespaceURI();
		if(namespace == null) {
			namespace = getNamespace(messageQName.getPrefix());
		}
		
		if(namespace == null) {
			return Collections.EMPTY_LIST;
		}
		
		QName namespaceMessageQName = 
			com.sun.bpel.xml.NamespaceUtility.getQName(
						namespace, messageQName.getLocalPart(),
						messageQName.getPrefix());
		
		ArrayList matchedVariables = new ArrayList();
	
		Variables containers = getVariables();
        int numContainers = containers.getVariableSize();

        for (int i = 0; i < numContainers; i++) {
            Variable container = containers.getVariable(i);

            QName messageType = container.getMessageType();
            if(messageType != null) {
	            String messageTypeLocalName = messageType.getLocalPart();
	            String wsdlNamespace = messageType.getNamespaceURI();
	            if(wsdlNamespace == null) {
	            	 String messageTypePrefix = messageType.getPrefix();
	            	 wsdlNamespace = container.getNamespace(messageTypePrefix);
	            }
	            
	            QName containerMessageQName = 
	            		com.sun.bpel.xml.NamespaceUtility.getQName(
	            				wsdlNamespace, messageTypeLocalName, 
	            				messageType.getPrefix());
	            
	            // To CHECK: have to make sure that the equals works as expected.
	            if (namespaceMessageQName.equals(containerMessageQName)) {
	            	matchedVariables.add(container);
	            }
            }
        }

		return matchedVariables;
	}

	public Collection getAllBPELVariables(BPELElement element) {
		return BPELHelper.getAllUniqueVariables(element);
	}

	public MessageProperty getBPELProperty(QName propertyQName) {
	    QName nsQName = propertyQName;
	    if(nsQName == null) {
	        return null;
	    }

	    MessageProperty property = null;
	    Collection wsdlDefinitions = getAllImportedWSDLDefinitions();
	    Iterator it = wsdlDefinitions.iterator();
	    while(it.hasNext()) {
	        Definition wsdlDefination = (Definition) it.next();
	        MessageProperty prop = WSDL4JExt.getMessageProperty(wsdlDefination, nsQName);
	        if(prop != null) {
	            property = prop;
	            break;
	        }
	    }

	    return property;
	}

	public SchemaGlobalElement getXSDElement(QName elementQName) {
		return schemaTypeLoader.findElement(elementQName);
	}

	public SchemaType getXSDType(QName typeQName) {
	    return schemaTypeLoader.findType(typeQName);
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

    /** @see com.sun.bpel.model.BPELProcessOrScope#getCorrelationSet(java.lang.String)
     */
    public CorrelationSet getCorrelationSet(String name) {
        CorrelationSets sets = getCorrelationSets();
        int setSize = sets.getCorrelationSetSize();
        CorrelationSet retVal = null;
        CorrelationSet set = null;
        for (int i = 0; i < setSize; i++) {
            set = sets.getCorrelationSet(i);
            if (set.getName().equals(name)) {
                retVal = set;
                break;
            }
        }
        return retVal;
    }

    /** @see com.sun.bpel.model.BPELProcess#getBPELPropertyAlias(javax.xml.namespace.QName)
     */
    public Collection<MessagePropertyAlias> getBPELPropertyAlias(QName propName) {
    	if (propName == null) {
    		return null;
    	}
        Collection retVal = new ArrayList();
        MessagePropertyAlias propertyAlias = null;
        Collection wsdlDefinitions = getAllImportedWSDLDefinitions();
        Iterator it = wsdlDefinitions.iterator();
        Iterator aliasItr = null;
        QName propQName = null;
        while(it.hasNext()) {
            Definition wsdlDefination = (Definition) it.next();
            Collection propAliases =
                WSDL4JExt.getMessagePropertyAliases(wsdlDefination, propName);
            retVal.addAll(propAliases);
        }
        
        return retVal;
    }

    public SchemaTypeLoader getSchemaTypeLoader() {
        return schemaTypeLoader;
    }

    public void setSchemaTypeLoader(SchemaTypeLoader loader) {
        schemaTypeLoader = loader; 
    }

    /** @see com.sun.bpel.model.BPELProcess#getNMPropToPropAliasMap()
     */
    public Map<QName, Collection<MessagePropertyAlias>> getNMPropToPropAliasMap() {
        return mNMPropToPropAliasMap;
    }

    /** @see com.sun.bpel.model.BPELProcess#setNMPropToPropAliasMap()
     */
    public void setNMPropToPropAliasMap() {
        mNMPropToPropAliasMap = new HashMap<QName, Collection<MessagePropertyAlias>>();
        Collection wsdlDefinitions = getAllImportedWSDLDefinitions();
        Iterator it = wsdlDefinitions.iterator();
        while(it.hasNext()) {
            Definition wsdlDefination = (Definition) it.next();
            Collection<MessageProperty> props = WSDL4JExt.getMessageProperties(wsdlDefination);
            for (MessageProperty prop : props) {
                Collection<MessagePropertyAlias> propAliases = getBPELPropertyAlias(prop.getName());
                for (MessagePropertyAlias propAlias : propAliases) {
                    if (!Utility.isEmpty(propAlias.getNMProperty())) {
                        Collection msgPropAliases = mNMPropToPropAliasMap.get(prop);
                        if (msgPropAliases == null) {
                            msgPropAliases = new ArrayList();
                            mNMPropToPropAliasMap.put(prop.getName(), msgPropAliases);
                        }
                        msgPropAliases.add(propAlias);
                    }
                }
            }
        }

    }
    
    Map<QName, Collection<MessagePropertyAlias>> mNMPropToPropAliasMap;

    public String getAtomicTxType() {
        return xmlAttrs[ATOMIC_TX_TYPE].getValue();
    }

    public void setAtomicTxType(String atomicTxType) {
        setAttribute(ATOMIC_TX_TYPE, atomicTxType);

    }

    public void setAtomicTxType(String qName, String atomicTxType) {
        setAttribute(ATOMIC_TX_TYPE, qName, atomicTxType);
    }

    /**
     * Method added for monitoring API to provide serialized BPEL document
     * 
     * @return
     * @throws SAXException
     */
    public String getSerializedBPELDocument() {
        String retVal = null;
        try {
            retVal = super.getSerializedBPELDocument();
        } catch (SAXException se) {
            throw new RuntimeException(se);
        }
        return retVal;
    }

    public String getEnableLogging() {
    	return xmlAttrs[ENABLE_LOGGING].getValue();
    }

    public void setEnableLogging(String enableLogging) {
    	setAttribute(ENABLE_LOGGING, enableLogging);
    }

    public void setEnableLogging(String qName, String enableLogging) {
    	setAttribute(ENABLE_LOGGING, qName, enableLogging);
    }

    public String getExtraNDC() {
    	return xmlAttrs[EXTRA_NDC].getValue();
    }

    public void setExtraNDC(String extraNDC) {
    	setAttribute(ENABLE_LOGGING, extraNDC);
    }

    public void setExtraNDC(String qName, String extraNDC) {
    	setAttribute(ENABLE_LOGGING, qName, extraNDC);
    }
    
}
