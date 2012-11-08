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
 * @(#)DummyRBPELProcess.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.test.common;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.event.ChangeListener;
import javax.wsdl.Message;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.xml.sax.Locator;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELDocumentation;
import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.CompensationHandler;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.CorrelationSets;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.PartnerLinks;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.xml.common.model.Documentation;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLAttributeEvent;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElementListener;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLNodeEvent;
import com.sun.bpel.xml.common.model.XMLNodeListener;
import com.sun.bpel.xml.common.visitor.Visitor;
import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;

/**
 *
 *
 * @author Sun Microsystems
 */
public class DummyRBPELProcess implements RBPELProcess {

	public Set<Invoke> getInvokeElements() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getAtomic() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getIgnoreMissingFromData()
	 */
	public String getIgnoreMissingFromData() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setIgnoreMissingFromData(java.lang.String, java.lang.String)
	 */
	public void setIgnoreMissingFromData(String name,
			String ignoreMissingFromData) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getPersistenceOptOut()
	 */
	public String getPersistenceOptOut() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setPersistenceOptOut(java.lang.String, java.lang.String)
	 */
	public void setPersistenceOptOut(String name, String persistenceOptOut) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setPersistenceOptOut(java.lang.String)
	 */
	public void setPersistenceOptOut(String persistenceOptOut) {
		// TODO Auto-generated method stub
		
	}
	
	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getGenerateEvents()
	 */
	public String getGenerateEvents() {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setGenerateEvents(java.lang.String, java.lang.String)
	 */
	public void setGenerateEvents(String name, String generateEvents) {
		// TODO Auto-generated method stub
		
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setGenerateEvents(java.lang.String)
	 */
	public void setGenerateEvents(String generateEvents) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setIgnoreMissingFromData(java.lang.String)
	 */
	public void setIgnoreMissingFromData(String ignoreMissingFromData) {
		// TODO Auto-generated method stub
		
	}

	public void setAtomic(String name, String atomic) {
		// TODO Auto-generated method stub
		
	}

	public void setAtomic(String atomic) {
		// TODO Auto-generated method stub
		
	}
	
	private QName mbpelId;
	
	/**
	 * 
	 */
	public DummyRBPELProcess(QName bpelId) {
		mbpelId = bpelId;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.meta.RBPELProcess#getBPELId()
	 */
	public QName getBPELId() {
		return mbpelId;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.meta.RBPELProcess#getStartElement(java.lang.String, java.lang.String, java.lang.String)
	 */
	public RStartElement getStartElement(String partnerLink, String oper,
			String msgExchange) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.meta.RBPELProcess#getStartElements()
	 */
	public Set getStartElements() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.meta.RBPELProcess#getStartElements(boolean)
	 */
	public Set getStartElements(boolean isInitiateTrue) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.meta.RBPELProcess#getVariable(long)
	 */
	public RVariable getVariable(long variableUniqueId) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#addImport(com.sun.bpel.model.Import)
	 */
	public void addImport(Import newImport) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getAbstractProcess()
	 */
	public String getAbstractProcess() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getActivities(java.lang.String)
	 */
	public List getActivities(String type) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getActivities(java.lang.String, com.sun.bpel.model.Activity)
	 */
	public List getActivities(String type, Activity currentActivity) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getAllBPELVariables(com.sun.bpel.model.BPELElement)
	 */
	public Collection getAllBPELVariables(BPELElement element) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getAllImportedWSDLDefinitions()
	 */
	public Collection getAllImportedWSDLDefinitions() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getAllImportedXMLSchemas()
	 */
	public Collection getAllImportedXMLSchemas() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getAllPortTypes()
	 */
	public Collection getAllPortTypes() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getBPELProperty(javax.xml.namespace.QName)
	 */
	public MessageProperty getBPELProperty(QName propertyQName) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getBPELPropertyAlias(javax.xml.namespace.QName)
	 */
	public Collection getBPELPropertyAlias(QName propName) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getBPELVariables(javax.xml.namespace.QName)
	 */
	public Collection getBPELVariables(QName messageQName) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getDestinationPartners()
	 */
	public List getDestinationPartners() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getEnableInstanceCompensation()
	 */
	public String getEnableInstanceCompensation() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getExpressionLanguage()
	 */
	public String getExpressionLanguage() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getFaultHandlers()
	 */
	public FaultHandlers getFaultHandlers() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getImportedWSDLDefinitions(java.lang.String)
	 */
	public Collection getImportedWSDLDefinitions(String targetNamespace) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getImportedXMLSchemas(java.lang.String)
	 */
	public Collection getImportedXMLSchemas(String targetNamespace) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getImports()
	 */
	public List getImports() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getImports(java.lang.String)
	 */
	public List getImports(String namespace) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getInvokes()
	 */
	public List getInvokes() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getPortType(javax.xml.namespace.QName)
	 */
	public PortType getPortType(QName name) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getQueryLanguage()
	 */
	public String getQueryLanguage() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getSourcePartners()
	 */
	public List getSourcePartners() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getSuppressJoinFailure()
	 */
	public String getSuppressJoinFailure() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getWSDLMessage(javax.xml.namespace.QName)
	 */
	public Message getWSDLMessage(QName name) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getXSDElement(javax.xml.namespace.QName)
	 */
	public SchemaGlobalElement getXSDElement(QName elementQName) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getXSDType(javax.xml.namespace.QName)
	 */
	public SchemaType getXSDType(QName elementQName) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#removeImport(com.sun.bpel.model.Import)
	 */
	public void removeImport(Import oldImport) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setAbstractProcess(java.lang.String)
	 */
	public void setAbstractProcess(String abstractProcess) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setAbstractProcess(java.lang.String, java.lang.String)
	 */
	public void setAbstractProcess(String name, String abstractProcess) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setEnableInstanceCompensation(java.lang.String)
	 */
	public void setEnableInstanceCompensation(String enableInstanceCompensation) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setEnableInstanceCompensation(java.lang.String, java.lang.String)
	 */
	public void setEnableInstanceCompensation(String name,
			String enableInstanceCompensation) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setExpressionLanguage(java.lang.String)
	 */
	public void setExpressionLanguage(String expressionLanguage) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setExpressionLanguage(java.lang.String, java.lang.String)
	 */
	public void setExpressionLanguage(String name, String expressionLanguage) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setFaultHandlers(com.sun.bpel.model.FaultHandlers)
	 */
	public void setFaultHandlers(FaultHandlers faultHandlers) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setName(java.lang.String, java.lang.String)
	 */
	public void setName(String name, String name2) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setQueryLanguage(java.lang.String)
	 */
	public void setQueryLanguage(String queryLanguage) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setQueryLanguage(java.lang.String, java.lang.String)
	 */
	public void setQueryLanguage(String name, String queryLanguage) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setSuppressJoinFailure(java.lang.String)
	 */
	public void setSuppressJoinFailure(String suppressJoinFailure) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setSuppressJoinFailure(java.lang.String, java.lang.String)
	 */
	public void setSuppressJoinFailure(String name, String suppressJoinFailure) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setTargetNamespace(java.lang.String, java.lang.String)
	 */
	public void setTargetNamespace(String name, String targetNamespace) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getAllBPELOnMessage()
	 */
	public Collection getAllBPELOnMessage() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getAllBPELOnMessage(java.lang.String)
	 */
	public Collection getAllBPELOnMessage(String partnerLinkName) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getAllBPELReceive()
	 */
	public Collection getAllBPELReceive() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getAllBPELReceive(java.lang.String)
	 */
	public Collection getAllBPELReceive(String partnerLinkName) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getBPELPartnerLink(java.lang.String)
	 */
	public PartnerLink getBPELPartnerLink(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getCorrelationSet(java.lang.String)
	 */
	public CorrelationSet getCorrelationSet(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getCorrelationSets()
	 */
	public CorrelationSets getCorrelationSets() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getPartnerLinks()
	 */
	public PartnerLinks getPartnerLinks() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#getVariables()
	 */
	public Variables getVariables() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#setCorrelationSets(com.sun.bpel.model.CorrelationSets)
	 */
	public void setCorrelationSets(CorrelationSets correlationSets) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#setPartnerLinks(com.sun.bpel.model.PartnerLinks)
	 */
	public void setPartnerLinks(PartnerLinks partnerLinks) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcessOrScope#setVariables(com.sun.bpel.model.Variables)
	 */
	public void setVariables(Variables variables) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.VariableScope#getBPELVariable(java.lang.String)
	 */
	public Variable getBPELVariable(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELElement#addDocumentation(com.sun.bpel.model.BPELDocumentation)
	 */
	public void addDocumentation(BPELDocumentation documentation) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELElement#getDocumentations()
	 */
	public List getDocumentations() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELElement#getTrace()
	 */
	public Trace getTrace() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELElement#removeDocumentation(com.sun.bpel.model.BPELDocumentation)
	 */
	public void removeDocumentation(BPELDocumentation documentation) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELElement#setTrace(com.sun.bpel.model.extensions.Trace)
	 */
	public void setTrace(Trace trace) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#addChangeListener(javax.swing.event.ChangeListener)
	 */
	public void addChangeListener(ChangeListener arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#addXMLElementListener(com.sun.wsdl.model.common.model.XMLElementListener)
	 */
	public void addXMLElementListener(XMLElementListener arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#createAndSetNamespacePrefix(java.lang.String, java.lang.String)
	 */
	public String createAndSetNamespacePrefix(String arg0, String arg1) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#fireXMLAttributeAdded(com.sun.wsdl.model.common.model.XMLAttributeEvent)
	 */
	public void fireXMLAttributeAdded(XMLAttributeEvent arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#fireXMLAttributeRemoved(com.sun.wsdl.model.common.model.XMLAttributeEvent)
	 */
	public void fireXMLAttributeRemoved(XMLAttributeEvent arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#fireXMLAttributedModified(com.sun.wsdl.model.common.model.XMLAttributeEvent)
	 */
	public void fireXMLAttributedModified(XMLAttributeEvent arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getAttribute(java.lang.String)
	 */
	public XMLAttribute getAttribute(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getAttributeValue(java.lang.String)
	 */
	public Object getAttributeValue(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getChildrenTags()
	 */
	public String[] getChildrenTags() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getDefaultNamespace()
	 */
	public String getDefaultNamespace() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getDocumentation()
	 */
	public Documentation getDocumentation() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getLineLabel()
	 */
	public String getLineLabel() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getNamespace(java.lang.String)
	 */
	public String getNamespace(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getNamespacePrefix(java.lang.String)
	 */
	public String getNamespacePrefix(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getNamespacePrefixes()
	 */
	public String[] getNamespacePrefixes() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getNamespaceSize()
	 */
	public int getNamespaceSize() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getNamespaces()
	 */
	public Map getNamespaces() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getOrderedAttributes()
	 */
	public List getOrderedAttributes() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getOtherAttributes()
	 */
	public Map getOtherAttributes() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getOwnerDocument()
	 */
	public XMLDocument getOwnerDocument() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getPresentationMap()
	 */
	public Map getPresentationMap() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getRawPresentationMap(boolean)
	 */
	public Map getRawPresentationMap(boolean arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getTotalNamespaces()
	 */
	public Map getTotalNamespaces() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#getXmlAttributes()
	 */
	public XMLAttribute[] getXmlAttributes() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#removeChangeListener(javax.swing.event.ChangeListener)
	 */
	public void removeChangeListener(ChangeListener arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#removeXMLElementListener(com.sun.wsdl.model.common.model.XMLElementListener)
	 */
	public void removeXMLElementListener(XMLElementListener arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setAttribute(int, java.lang.String)
	 */
	public void setAttribute(int arg0, String arg1) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setAttribute(java.lang.String, java.lang.String)
	 */
	public void setAttribute(String arg0, String arg1) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setAttribute(java.lang.String, java.lang.Object)
	 */
	public void setAttribute(String arg0, Object arg1) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setAttribute(int, java.lang.String, java.lang.String)
	 */
	public void setAttribute(int arg0, String arg1, String arg2) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setDefaultNamespace(java.lang.String)
	 */
	public void setDefaultNamespace(String arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setDocumentation(com.sun.wsdl.model.common.model.Documentation)
	 */
	public void setDocumentation(Documentation arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setDocumentationAtTail(com.sun.wsdl.model.common.model.Documentation)
	 */
	public void setDocumentationAtTail(Documentation arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setLineLabel(java.lang.String)
	 */
	public void setLineLabel(String arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setNamespace(java.lang.String, java.lang.String)
	 */
	public void setNamespace(String arg0, String arg1) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setNamespaces(java.util.Map)
	 */
	public void setNamespaces(Map arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setOtherAttributes(java.lang.String, java.lang.String)
	 */
	public void setOtherAttributes(String arg0, String arg1) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setOtherAttributes(javax.xml.namespace.QName, java.lang.String)
	 */
	public void setOtherAttributes(QName arg0, String arg1) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setOwnerDocument(com.sun.wsdl.model.common.model.XMLDocument)
	 */
	public void setOwnerDocument(XMLDocument arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setPresentationMap(java.util.Map)
	 */
	public void setPresentationMap(Map arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#setQualifiedName(javax.xml.namespace.QName)
	 */
	public void setQualifiedName(QName arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLElement#superAccept(com.sun.wsdl.model.common.visitor.Visitor)
	 */
	public boolean superAccept(Visitor arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#accept(com.sun.wsdl.model.common.visitor.Visitor)
	 */
	public boolean accept(Visitor arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#addChild(com.sun.wsdl.model.common.model.XMLNode)
	 */
	public void addChild(XMLNode arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#addChild(int, com.sun.wsdl.model.common.model.XMLNode)
	 */
	public void addChild(int arg0, XMLNode arg1) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#addChild(int, com.sun.wsdl.model.common.model.XMLNode, com.sun.wsdl.model.common.model.XMLNode)
	 */
	public void addChild(int arg0, XMLNode arg1, XMLNode arg2) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#addChildAtTail(com.sun.wsdl.model.common.model.XMLNode)
	 */
	public void addChildAtTail(XMLNode arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#addPropertyChangeListener(java.beans.PropertyChangeListener)
	 */
	public void addPropertyChangeListener(PropertyChangeListener arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#addXMLNodeListener(com.sun.wsdl.model.common.model.XMLNodeListener)
	 */
	public void addXMLNodeListener(XMLNodeListener arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#firePropertyChangeEvent(java.beans.PropertyChangeEvent)
	 */
	public void firePropertyChangeEvent(PropertyChangeEvent arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#fireXMLNodeAdded(com.sun.wsdl.model.common.model.XMLNodeEvent)
	 */
	public void fireXMLNodeAdded(XMLNodeEvent arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#fireXMLNodeRemoved(com.sun.wsdl.model.common.model.XMLNodeEvent)
	 */
	public void fireXMLNodeRemoved(XMLNodeEvent arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#fireXMLNodeValueChanged(java.beans.PropertyChangeEvent)
	 */
	public void fireXMLNodeValueChanged(PropertyChangeEvent arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getChildren()
	 */
	public List getChildren() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getLeadingWhitespace()
	 */
	public char[] getLeadingWhitespace() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getLocalName()
	 */
	public String getLocalName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getLocator()
	 */
	public Locator getLocator() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getParent()
	 */
	public XMLNode getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getQualifiedName()
	 */
	public String getQualifiedName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getSiblingSequenceOrder()
	 */
	public int getSiblingSequenceOrder() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getTrailingWhitespace()
	 */
	public char[] getTrailingWhitespace() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getValue()
	 */
	public String getValue() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#getXPath()
	 */
	public String getXPath() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#hasChildren()
	 */
	public boolean hasChildren() {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#indexOfChild(com.sun.wsdl.model.common.model.XMLNode)
	 */
	public int indexOfChild(XMLNode arg0) {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#merge(com.sun.wsdl.model.common.model.XMLNode)
	 */
	public void merge(XMLNode arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#removeChild(com.sun.wsdl.model.common.model.XMLNode)
	 */
	public void removeChild(XMLNode arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#removePropertyChangeListener(java.beans.PropertyChangeListener)
	 */
	public void removePropertyChangeListener(PropertyChangeListener arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#removeXMLNodeListener(com.sun.wsdl.model.common.model.XMLNodeListener)
	 */
	public void removeXMLNodeListener(XMLNodeListener arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#setLeadingWhitespace(char[])
	 */
	public void setLeadingWhitespace(char[] arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#setLocalName(java.lang.String)
	 */
	public void setLocalName(String arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#setLocator(org.xml.sax.Locator)
	 */
	public void setLocator(Locator arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#setParent(com.sun.wsdl.model.common.model.XMLNode)
	 */
	public void setParent(XMLNode arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#setQualifiedName(java.lang.String)
	 */
	public void setQualifiedName(String arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#setSiblingSequenceOrder(int)
	 */
	public void setSiblingSequenceOrder(int arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#setTrailingWhitespace(char[])
	 */
	public void setTrailingWhitespace(char[] arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#setValue(java.lang.String)
	 */
	public void setValue(String arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLNode#swapChildren(com.sun.wsdl.model.common.model.XMLNode, com.sun.wsdl.model.common.model.XMLNode)
	 */
	public boolean swapChildren(XMLNode arg0, XMLNode arg1) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.EventHandlersHolder#getEventHandlers()
	 */
	public EventHandlers getEventHandlers() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.EventHandlersHolder#setEventHandlers(com.sun.bpel.model.EventHandlers)
	 */
	public void setEventHandlers(EventHandlers eventHandlers) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.CompensationHandlerHolder#getCompensationHandler()
	 */
	public CompensationHandler getCompensationHandler() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.CompensationHandlerHolder#setCompensationHandler(com.sun.bpel.model.CompensationHandler)
	 */
	public void setCompensationHandler(CompensationHandler compensationHandler) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLDocumentElement#getTargetNamespace()
	 */
	public String getTargetNamespace() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.wsdl.model.common.model.XMLDocumentElement#setTargetNamespace(java.lang.String)
	 */
	public void setTargetNamespace(String arg0) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.SingleActivityHolder#getActivity()
	 */
	public Activity getActivity() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.SingleActivityHolder#setActivity(com.sun.bpel.model.Activity)
	 */
	public void setActivity(Activity activity) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.NamedElement#getName()
	 */
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.NamedElement#setName(java.lang.String)
	 */
	public void setName(String name) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.meta.ScopingElement#getScopeId()
	 */
	public long getScopeId() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.meta.RActivityHolder#getChildActivity()
	 */
	public RActivity getChildActivity() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.meta.RActivityHolder#setChildActivity(com.sun.bpel.model.meta.RActivity)
	 */
	public void setChildActivity(RActivity act) {
		// TODO Auto-generated method stub

	}

    public SchemaTypeLoader getSchemaTypeLoader() {
        // TODO Auto-generated method stub
        return null;
    }

    public void setSchemaTypeLoader(SchemaTypeLoader loader) {
        // TODO Auto-generated method stub
        
    }

	public boolean isEventHandlersDefined() {
		// TODO Auto-generated method stub
		return false;
	}

	public void setEventHandlersDefined() {
		// TODO Auto-generated method stub
		
	}

    /** @see com.sun.bpel.model.BPELProcess#getNMPropToPropAliasMap()
     */
    public Map<QName, Collection<MessagePropertyAlias>> getNMPropToPropAliasMap() {
        // TODO Auto-generated method stub
        return null;
    }

    /** @see com.sun.bpel.model.BPELProcess#setNMPropToPropAliasMap()
     */
    public void setNMPropToPropAliasMap() {
        // TODO Auto-generated method stub
        
    }

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#getWaitingRequestLifeSpan()
	 */
	public String getWaitingRequestLifeSpan() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setWaitingRequestLifeSpan(java.lang.String)
	 */
	public void setWaitingRequestLifeSpan(String waitingRequestLifeSpan) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.BPELProcess#setWaitingRequestLifeSpan(java.lang.String, java.lang.String)
	 */
	public void setWaitingRequestLifeSpan(String name,
			String waitingRequestLifeSpan) {
		// TODO Auto-generated method stub
		
	}

	public String getAtomicTxType() {
		// TODO Auto-generated method stub
		return null;
	}

	public void setAtomicTxType(String atomic) {
		// TODO Auto-generated method stub
		
	}

	public void setAtomicTxType(String qName, String atomic) {
		// TODO Auto-generated method stub
		
	}

    public String getSerializedBPELDocument() {
        // TODO Auto-generated method stub
        return null;
    }

    public String getEnableLogging() {
        return "yes";
    }

    public void setEnableLogging(String enableLogging) {
    }

    public void setEnableLogging(String qName, String enableLogging) {
    }

    public String getExtraNDC() {
        return null;
    }

    public void setExtraNDC(String extraNDC) {
    }

    public void setExtraNDC(String qName, String extraNDC) {
    }

}
