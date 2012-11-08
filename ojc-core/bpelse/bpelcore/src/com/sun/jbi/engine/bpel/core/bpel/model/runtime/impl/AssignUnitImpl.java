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
 * @(#)AssignUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import net.sf.hulp.measure.Measurement;

import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.mozilla.javascript.Scriptable;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXParseException;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Copy;
import com.sun.bpel.model.ExtensionAssignOperation;
import com.sun.bpel.model.From;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.SunExtExpression;
import com.sun.bpel.model.To;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RMutableExpressionElement;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.bpel.model.meta.impl.RAssignImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.VariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.exception.SystemException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.core.bpel.util.ValidationResult;
import com.sun.jbi.engine.bpel.core.bpel.util.Validator;
import com.sun.jbi.engine.bpel.core.bpel.util.XmlBeansValidator;
import com.sun.wsdl4j.ext.WSDL4JExt;
import com.sun.xml.transform.sware.SwareDOMImplementation;
import com.sun.xml.transform.sware.SwareDOMImplementationFactory;
import com.sun.xml.transform.sware.schema.ImplementationType;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * Assign activity unit implementation
 * 
 * @author Sun Microsystems
 */
/**
 * @author mpottlapelli
 *
 */
/**
 * @author mpottlapelli
 *
 */
public class AssignUnitImpl extends ActivityUnitImpl {
	private static final Logger LOGGER = Logger.getLogger(AssignUnitImpl.class.getName());
	private static final String CLASSIDENTIFIER_FORMEASUREMENT = "BPEL-SE.AssignUnitImpl";

	private static CopyUnitImpl copyUnit = new CopyUnitImpl();    

	/**
	 * Creates a new AssignUnitImpl object.
	 * 
	 * @param parentActUnit parent activity unit
	 * @param act activity
	 * @param branchId branch ID
	 */
	public AssignUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
		super(context, parentActUnit, act, branchId);
	}

	/**
	 * @see ActivityUnitImpl#doAction(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
	 *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
	 *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
	 */
	public boolean doAction(final ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs)
	throws Exception {

		//        LOGGER.log(Level.FINEST, " assign start: "+this+"; "+System.nanoTime());
		frame.setProgramCounter(this);
		frame.onLineChange(this);
		BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
		frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);

		final Map<RVariable, RuntimeVariable> updateVariableList = new HashMap<RVariable, RuntimeVariable>();
		final Map<PartnerLink, RuntimePartnerLink> updatedPartnerLinks = new HashMap<PartnerLink, RuntimePartnerLink>(); 
		Set<RVariable> variableNeedDOMSort = new HashSet<RVariable>();

		RAssignImpl assign = (RAssignImpl) mAct;
		ExtensionAssignOperation extension = assign.getExtensionAssignOperation();
		if (extension != null) {
			//executeExtensionAssignOperation(extension);
			executeExtensionAssignOperation(extension, updateVariableList);
		} else {
			Collection copies = assign.getCopies();
			executeCopies(frame, rObjs, updateVariableList, variableNeedDOMSort, updatedPartnerLinks, copies);
		}

		Iterator varValuesItr = updateVariableList.values().iterator();

		//perform validation if required before the variable becomes updated
        validate(assign, updateVariableList, rObjs);
		// update the bpel instance with the variable got updated
		while (varValuesItr.hasNext()) {
			RuntimeVariable rv = (RuntimeVariable) varValuesItr.next();
			Measurement m2 = null;
			if(Measurement.isInstalled()){
				String measure = "DOM Sorting : [ variable-> " + rv.getVariableDef().getName() + " ]";
				m2 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, measure);
			}
			if (variableNeedDOMSort.contains(rv.getVariableDef())) {
				validateOrAndTransformVariable(rv);
			}
			if(Measurement.isInstalled()){
				m2.end();
			}
			mContext.setRuntimeVariable(rv);     
			StateContext stateCtx = mContext.getStateContext();
			mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
					rv, getBranchId());
		}
		Iterator<RuntimePartnerLink> pLinkValuesItr = updatedPartnerLinks.values().iterator();

		// update the bpel instance with the partnerlink that got updated
		while (pLinkValuesItr.hasNext()) {
			RuntimePartnerLink rPLink = pLinkValuesItr.next();
			mContext.setRuntimePartnerLink(rPLink.getStaticModel(), rPLink);  
			StateContext stateCtx = mContext.getStateContext();
			mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
					rPLink, getBranchId());
		}

		BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
		frame.onActivityComplete(this);
		frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);           
		//        LOGGER.log(Level.FINEST, " assign stop: "+this+"; "+System.nanoTime());

		return true;
	}

	private Validator getValidator() {
		return XmlBeansValidator.getInstance();
	}

    private void validate(RAssignImpl assign, Map<RVariable, 
            RuntimeVariable> updatedVariables, RequiredObjects rObjs)
    {
        boolean isValidate = rObjs.getEngine().isValidationEnabled() && XMLAttribute.YES.equals(assign.getValidate());
		if (!isValidate) {
//            LOGGER.log(Level.FINE, " validation is disabled or assign validation isn't enabled");
			//don't need validation at all
			return;
		}
		//            LOGGER.log(Level.INFO, " is validate true");

		ValidationResult<RuntimeVariable> valResult = getValidator().validate(updatedVariables.values());
		if (valResult != null) {
			StringBuilder error = new StringBuilder();
			Collection<Object> errors = valResult.getErrors();
			for (Iterator it = errors.iterator(); it.hasNext();) {
				String localError = "" + it.next();
				error.append(localError).append(System.getProperty("line.separator"));
			}

			Object valResultValue = valResult.getValue();
			// TODO set correct BPCOR-number
			String error2throw = I18n.loc(
					"BPCOR-7134: Invalid variable value \n{0}\nis assigned to {1} in BP instance ({2}) at line {3} BP {4}. Error Summary: {5}",
					valResultValue instanceof Element ? DOMHelper.createXmlString((Element)valResultValue) : valResultValue, valResult.getSource().getVariableDef().getName(),
							mContext.getProcessInstance().getId(), mAct.getLocator().getLineNumber(),
							mContext.getProcessInstance().getBPELProcessManager().getBPELProcess().getBPELId(), error.toString());
			//        LOGGER.log(Level.INFO, " assign validate error stop: "+this+"; "+System.nanoTime());
			throw new StandardException(StandardException.Fault.InvalidVariables, error2throw);
		}
	}

	private void executeCopies(final ICallFrame frame, RequiredObjects rObjs, 
			final Map<RVariable, RuntimeVariable> updateVariableList,
			Set<RVariable> variableNeedDOMSort,
			final Map<PartnerLink, RuntimePartnerLink> updatedPartnerLinks,
			Collection copies) 
	throws Exception 
	{
		Iterator iter = copies.iterator();

		//Local variable scope so that copy execution resolves the variables created by copy siblings
		//This also helps make Assign atomic, but it is not implemented yet.
		VariableScope variableScope = new VariableScope() {
			public RuntimeVariable getRuntimeVariable(RVariable variable) {
				RuntimeVariable rv = (RuntimeVariable) updateVariableList.get(variable);
				if (rv != null) {
					return rv;
				}
				return mContext.getRuntimeVariable(variable);
			}

			public void setRuntimeVariable(RuntimeVariable runtimeVariable) {
				throw new UnsupportedOperationException();
			}                          

			public Map getRuntimeVariables() {
				throw new UnsupportedOperationException();
			}

			public RuntimeVariable createRuntimeVariable(RVariable variable) {
				return mContext.createRuntimeVariable(variable);
			}   

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#createRuntimePartnerLink(PartnerLink)
			 */
			public RuntimePartnerLink createRuntimePartnerLink(PartnerLink partnerlink) {
				return mContext.createRuntimePartnerLink(partnerlink);
			}

		};

		//Local variable scope so that copy execution resolves the PLVariables created by copy siblings
		//This also helps make Assign atomic, but it is not implemented yet.
		PartnerLinkScope pLinksScope = new PartnerLinkScope() {
			public RuntimePartnerLink getRuntimePartnerLink(PartnerLink pLink) {
				RuntimePartnerLink rPLink = (RuntimePartnerLink) updatedPartnerLinks.get(pLink);
				if (rPLink != null) {
					return rPLink;
				}
				return mContext.getRuntimePartnerLink(pLink);
			}

			public void setRuntimePartnerLink(PartnerLink pLink, RuntimePartnerLink runtimePLink) {
				// add the partnerlink into map, so that if a subsequent copy needs 
				// to use the partnerlink that was just updated, it will be available
				updatedPartnerLinks.put(pLink, runtimePLink);
			}

			public Map getRuntimePartnerLinks() {
				return mContext.getRuntimePartnerLinks();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#createRuntimePartnerLink(PartnerLink)
			 */
			public RuntimePartnerLink createRuntimePartnerLink(PartnerLink partnerlink) {
				return mContext.createRuntimePartnerLink(partnerlink);
			}
		};

		Context dlgCtx = getDelegateCtx(variableScope, pLinksScope, mContext);
		Copy copy = null;
		To to = null;
		Set<RVariable> varEventSet = new HashSet<RVariable>();
		// execute copy units      
		while (iter.hasNext()) {

                    boolean newVar = false;
                    copy = (Copy) iter.next();
                    to = copy.getTo();
                    // get variable we are updating
                    RVariable variable = ((RVariableElement) to).getRVariable();
                    if (variable != null) {
                            RuntimeVariable varForUpdate = (RuntimeVariable) updateVariableList.get(variable);

                            if (varForUpdate == null) {
                                    varForUpdate = mContext.getRuntimeVariable(variable);
                                    // FIX if the variable is obtained from the context, we need to do a deep copy
                                    // to keep it isolated from the bpelinstance.

                                    if (varForUpdate == null) {
                                            varForUpdate = variableScope.createRuntimeVariable(variable);
                                            newVar = true;
                                    }
                                    // add the variable into map, so that if a subsequent copy needs
                                    // to use the variable that was just updated, it will be available. Also needed by variable
                                    // monitoring.
                                    updateVariableList.put(variable, varForUpdate);
                            }
                    }
                    if(((RMutableExpressionElement)to).isModifiesDocStructure()){
                            variableNeedDOMSort.add(variable);
                    }
                    // get PartnerLink we are updating
                    PartnerLink pLink = to.getBPELPartnerLink();
                    if (pLink != null) {
                            RuntimePartnerLink pLinkForUpdate = updatedPartnerLinks.get(pLink);
                            if (pLinkForUpdate == null) {
                                    pLinkForUpdate = mContext.getRuntimePartnerLink(pLink);
                                    if (pLinkForUpdate == null) {
                                            pLinkForUpdate = mContext.createRuntimePartnerLink(pLink);
                                    }
                                    // add the partnerlink into map, so that if a subsequent copy needs
                                    // to use the partnerlink that was just updated, it will be available
                                    updatedPartnerLinks.put(pLink, pLinkForUpdate);
                            }
                    }
                    From from = copy.getFrom();
                    Measurement m1 = null;
                    if(Measurement.isInstalled()){
                            String fromString = from.getVariable();
                            fromString = fromString != null ? "variable->" + fromString : from.getExpression();

                            String toString = to.getVariable();
                            toString = toString != null ? "variable->" + toString : to.getQuery();

                            String measure = "Copy : [ From { " + fromString +" } To { " + toString + " }]";
                            m1 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, measure);
                    }
                    // execute the copy now
                    try {
                            copyUnit.doCopy(frame, copy, dlgCtx);
                    } catch (IgnoreMissingFromDataException igmfde) {
                            if (newVar) {
                                    updateVariableList.remove(variable);
                                    variableNeedDOMSort.remove(variable);
                            }
                            continue;
                    } finally {
                            if(Measurement.isInstalled()){
                                    m1.end();
                            }
                    }
                    //post the valChange event
                    if ((variable != null) &&
                                    (frame.getProcessInstance().getMonitorMgr().generateEventsForVariable(variable.getUniqueId())))
                    {
                            varEventSet.add(variable);
                    }
		}		
		
		if (frame.getProcessInstance().getMonitorMgr().generateEventsForProcess() && (!varEventSet.isEmpty())) {
			Map<VariableEvent.VariableType, List<Variable>> variableMap = new HashMap<VariableType, List<Variable>>();
			variableMap.put(VariableType.NEW, new ArrayList<Variable>(varEventSet.size()));
			List<Variable> newVarList = variableMap.get(VariableType.NEW);
			for (RVariable variable : varEventSet) {
				Object value = updateVariableList.get(variable).getSerializedValue();
				Variable var = new VariableImpl(variable, (Object) value, mContext);
				newVarList.add(var);
			}
			frame.getProcessInstance().getMonitorMgr().postVariableEvent(this, variableMap, false, null, null);
		}
	}

	/*
	 * 
	 */
	private void validateOrAndTransformVariable(RuntimeVariable rv) throws ParserConfigurationException {
		if (rv.getVariableDef().getMessageType() != null) {
			Message message = rv.getVariableDef().getWSDLMessageType();
			WSMessage messageValue = rv.getWSMessage();
			Collection parts = message.getParts().values();
			Iterator partsItr = parts.iterator();
			while (partsItr.hasNext()) {
				Part part = (Part) partsItr.next();
				QName xmlTypeName = null;
				SchemaTypeLoader typeLoader = WSDL4JExt.getSchemaTypeLoader(part);
				if (part.getElementName() != null) {
					SchemaGlobalElement elem = typeLoader.findElement(part.getElementName());
					if (elem.getType() != null) {
						xmlTypeName = elem.getType().getName();
					} else {
						continue;
					}
				} else {
					xmlTypeName = part.getTypeName();
					SchemaType xmlType = typeLoader.findType(xmlTypeName);
					//when a document is of simple type there is no need to transform
					//this protects from WSDL model bug related to user defined simple type
					//When validate feature is implemented this can not be skipped
					if(xmlType == null || xmlType.isSimpleType()){
						continue;
					}
				}

				Element partValue = messageValue.getPart(part.getName());
				if (partValue == null) {
					continue;
				}
				Document partDoc = partValue.getOwnerDocument();
				// validate the document, if it fails transform using schema aware transformer
				// validate is expensive, so no more validation
				// if (!isValidDocument(schema, partDoc)) {
				try {
					transformDocument(partValue, typeLoader, xmlTypeName);
				} catch (TransformerException exp) {
					String error = I18n.loc(
							"BPCOR-6175: Invalid document \n{0}\nis assigned to {1} in BP instance ({2}) at line {3} BP {4}",
							DOMHelper.createXmlString(partValue), rv.getVariableDef().getName(),
							mContext.getProcessInstance().getId(), mAct.getLocator().getLineNumber(),
							mContext.getProcessInstance().getBPELProcessManager().getBPELProcess().getBPELId());
					throw new StandardException(StandardException.Fault.InvalidVariables, error, exp);
				}

				// }
			}
		} else {
			QName xmlTypeName;
			SchemaTypeLoader typeLoader;
			if (rv.getVariableDef().getElement() != null) {
				SchemaGlobalElement elem = rv.getVariableDef().getXSDElement();
				typeLoader = elem.getTypeSystem();
				if (elem.getType() != null) {
					xmlTypeName = elem.getType().getName();
				} else {
					return;
				}
			} else {
				SchemaType xmlType = rv.getVariableDef().getXSDType();
				// when a document is of simple type there is no need to
				// transform
				// When validate feature is implemented this can not be skipped
				if (xmlType == null || xmlType.isSimpleType()) {
					return;
				}
				typeLoader = xmlType.getTypeSystem();
				xmlTypeName = xmlType.getName();
			}
			// Document doc =
			// ((Node)rv.getXSDVariableData()).getOwnerDocument();
			Node xsdElement = (Node) rv.getXSDVariableData();
			// if (!isValidDocument(schema, doc)) {
			try {
				transformDocument(xsdElement, typeLoader, xmlTypeName);
			} catch (TransformerException exp) {
				String error = I18n.loc(
						"BPCOR-6175: Invalid document \n{0}\nis assigned to {1} in BP instance ({2}) at line {3} BP {4}",
						DOMHelper.createXmlString(xsdElement.getOwnerDocument().getDocumentElement()), rv.getVariableDef().getName(),
						mContext.getProcessInstance().getId(), mAct.getLocator().getLineNumber(), 
						mContext.getProcessInstance().getBPELProcessManager().getBPELProcess().getBPELId());
				throw new StandardException(
						StandardException.Fault.InvalidVariables, error, exp);
			}
			//}
		}
	}

	//private void transformDocument(Document doc, Schema schema, QName xsdType) throws Exception {
	private void transformDocument(Node node, SchemaTypeLoader xmlTypeLoader,
			QName xmlTypeName) throws ParserConfigurationException,
			TransformerConfigurationException, TransformerException {
		SwareTypeSystem swareTypeSystem = SwareTypeSystem.Factory.newSchemaTypeSystem(ImplementationType.XMLBEANS, xmlTypeLoader);
		Transformer tfm = getSwareDOMImplementation(node.getOwnerDocument()).createReorderTransformer(swareTypeSystem, null);
		DOMSource source = new DOMSource(node);
		DOMResult result = new DOMResult();
		tfm.setParameter("xmltype", xmlTypeName);

		if (LOGGER.isLoggable(Level.FINEST)) {
			LOGGER.log(Level.FINEST, I18n.loc(
					"BPCOR-3003: \ndocument before transformation:\n {0}",
                    DOMHelper.createXmlString(node.getOwnerDocument().getDocumentElement())));
		}

		tfm.transform(source, result);
		Node transformedNode = result.getNode();

		if (LOGGER.isLoggable(Level.FINEST)) {
			LOGGER.log(Level.FINEST, I18n.loc(
					"BPCOR-3052: \ndocument after transformation:\n {0}",
					DOMHelper.createXmlString(transformedNode.getOwnerDocument().getDocumentElement())));
		}
		// TODO: Importing the node from transformed document to variable
		// document is not efficient
		if (node == transformedNode) {
			return;
		}
		updateNode(node, transformedNode);
	}

	private void updateNode(Node targetNode, Node fromNode) {
		NodeList children = targetNode.getChildNodes();
		int count = children.getLength();
		for (int i = count; --i >= 0;) {
			Node child = children.item(i);
			targetNode.removeChild(child);
		}
		children = fromNode.getChildNodes();
		count = children.getLength();
		for (int j = count; --j > 0; ) {
			Node child = children.item(j);
			targetNode.appendChild(child);
		}
	}

	/**
	 * Gets the schema aware DOM implementation.
	 * 
	 * @return an instance of SwareDOMImplementation
	 * @throws ParserConfigurationException
	 */
	private static SwareDOMImplementation saDomImpl= null;
	private static SwareDOMImplementation getSwareDOMImplementation(Document doc)
	throws ParserConfigurationException {
		if (saDomImpl == null) {
			DOMImplementation domImpl = doc.getImplementation();
			saDomImpl = SwareDOMImplementationFactory.newSwareDOMImplementation(domImpl);
		}
		return saDomImpl;
	}
	static class ErrorCounter implements ErrorHandler {
		int errorCount = 0;
		public void error(SAXParseException e) {
			errorCount++;
		}
		public void fatalError(SAXParseException e) {
			errorCount++;
		}
		public void warning(SAXParseException e) {
			//do nothing
		}
	}

	private static Context getDelegateCtx(final VariableScope varScope, 
			final PartnerLinkScope pLinkScope, final Context parentCtx) {
		Context ctx = new Context() {

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getParentContext()
			 */
			public Context getParentContext() {
				throw new UnsupportedOperationException();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getProcessInstance()
			 */
			public BPELProcessInstance getProcessInstance() {
				return parentCtx.getProcessInstance();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initUponRecovery(Collection, Collection)
			 */
			public void initUponRecovery(Collection<RuntimeVariable> runtimeVariables, Collection<RuntimePartnerLink> runtimePLinks) {
				throw new UnsupportedOperationException();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initEHUponRecovery(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers)
			 */
			public void initEHUponRecovery(RuntimeEventHandlers rEH) {
				throw new UnsupportedOperationException();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#getRuntimePartnerLink(com.sun.bpel.model.PartnerLink)
			 */
			public RuntimePartnerLink getRuntimePartnerLink(PartnerLink pLink) {
				return pLinkScope.getRuntimePartnerLink(pLink);
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#setRuntimePartnerLink(com.sun.bpel.model.PartnerLink, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink)
			 */
			public void setRuntimePartnerLink(PartnerLink pLink, RuntimePartnerLink runtimePLink) {
				pLinkScope.setRuntimePartnerLink(pLink, runtimePLink);
			}

			public Map getRuntimePartnerLinks() {
				return pLinkScope.getRuntimePartnerLinks();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariable(com.sun.bpel.model.meta.RVariable)
			 */
			public RuntimeVariable getRuntimeVariable(RVariable variable) {
				return varScope.getRuntimeVariable(variable);
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariables()
			 */
			public Map getRuntimeVariables() {
				throw new UnsupportedOperationException();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#setRuntimeVariable(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable)
			 */
			public void setRuntimeVariable(RuntimeVariable runtimeVariable) {
				throw new UnsupportedOperationException();
			}                                  

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addRequest(com.sun.bpel.model.meta.RStartElement, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
			 */
			public void addRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
				throw new UnsupportedOperationException();
			}

			/**
			 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RStartElement,
			 * com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
			 */
			public void removeRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
				 throw new UnsupportedOperationException();
			}

            /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addToCRMPUpdateList(java.lang.String)
             */
			public void addToCRMPUpdateList(String updateValueKey) {
				throw new UnsupportedOperationException();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#crmpUpdateListContains(java.lang.String)
			 */
			public boolean crmpUpdateListContains(String updateValueKey) {
				throw new UnsupportedOperationException();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#declareDefaultMessageExchange()
			 */
			public void declareDefaultMessageExchange() {
				throw new UnsupportedOperationException();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RReply)
			 */
			public MessageContainer removeRequest(RReply reply) {
				throw new UnsupportedOperationException();
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#sendErrorsForPendingRequests(java.lang.Exception)
			 */
			public void sendErrorsForPendingRequests(Exception error) {
				throw new UnsupportedOperationException();
			}

			/**
			 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#completePendingInOnlyRequests()
			 */
			public void completePendingInOnlyRequests() {
				throw new UnsupportedOperationException();
			}

			public FaultHandlingContext getFaultHandlingContext() {
				throw new UnsupportedOperationException();
			}
			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getStateContext()
			 */
			public StateContext getStateContext() {
				throw new UnsupportedOperationException();
			}

			public RuntimeVariable createRuntimeVariable(RVariable variable) {
				return varScope.createRuntimeVariable(variable);
			}

			/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#createRuntimePartnerLink(PartnerLink)
			 */
			public RuntimePartnerLink createRuntimePartnerLink(PartnerLink partnerlink) {
				return pLinkScope.createRuntimePartnerLink(partnerlink);
			}
		};
		return ctx;
	}
	/**
	 * It executes the script embeded in
	 * {http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/DataHandling}Expression element. 
	 * Expression is ws-bpel extension element defined as child of extensionAssignOperation
	 * 
	 * It has three attributes expressionLanguage - language in which embedded
	 * script is authored only supported value is "urn:sun:bpel:JavaScript"
	 * inputVar - input variables to script, format is
	 * "<script-var-name>=<bpel-var-name>.<bpel-var-part>,..." 
	 * outputVar - output variables from script, , format is
	 * "<bpel-var-name>.<bpel-var-part>=<script-var-name>,..."
	 * 
	 * @param extension
	 *            extensionAssignOperation element to be executed
	 * @param updateVariableList
	 *            Map of variables updated while evaluating the
	 *            extensionAssignOperation
	 */
	private void executeExtensionAssignOperation(
			ExtensionAssignOperation extension,
			Map<RVariable, RuntimeVariable> updateVariableList) {
		SunExtExpression extExpression = extension.getSunExtExpression();
		String expressionLanguage = extExpression.getExpressionLanguage();

		String strInputVars = extExpression.getInputVars();
		Set<ExtLangVariable> inputVariables = ExtLangVariable.parse(strInputVars, false);
		String strOutputVars = extExpression.getOutputVars();
		Set<ExtLangVariable> outputVariables = ExtLangVariable.parse(strOutputVars, true);

		org.mozilla.javascript.Context cx = org.mozilla.javascript.Context.enter();
		try {
			Scriptable scope = cx.initStandardObjects();
			// set the Rhino scope with bpel variables
			for (ExtLangVariable inputVariable : inputVariables) {
				RVariable bpelVarModel = (RVariable) BPELHelper.getMatchingVariable(inputVariable.getBpelVarName(), mAct);
				RuntimeVariable bpelVar = mContext.getRuntimeVariable(bpelVarModel);
				//check for UninitializedVariable error
				Utility.verifyValue(inputVariable.getBpelVarName(), inputVariable.getBpelVarPart(), bpelVar);
				if (inputVariable.isXSDVariable()) {
					Object value = bpelVar.getXSDVariableData();
					scope.put(inputVariable.getExtLangVarName(), scope, value);
				} else {
					WSMessage value = bpelVar.getWSMessage();
					scope.put(inputVariable.getExtLangVarName(), scope, value.getPart(inputVariable.getBpelVarPart()));
				}
			}

			String script = extExpression.getExpression();
			// Now evaluate the script. We'll ignore the result.
			cx.evaluateString(scope, script, "extensionAssignOperation.Expression", 1, null);

			// get the script variables from Rhino scope and update bpel
			// variables
			for (ExtLangVariable outputVariable : outputVariables) {
				RVariable bpelVarModel = (RVariable) BPELHelper.getMatchingVariable(outputVariable.getBpelVarName(), mAct);
				RuntimeVariable bpelVar = mContext.getRuntimeVariable(bpelVarModel);
				// if the variable is not defined on the context yet
				if (bpelVar == null) {
					bpelVar = mContext.createRuntimeVariable(bpelVarModel);
				}
				Object value = scope.get(outputVariable.getExtLangVarName(), scope);
				if (outputVariable.isXSDVariable()) {
					SchemaType schemaType = bpelVarModel.getXSDType();
					if (schemaType == null) {
						SchemaGlobalElement schemaGlobalElem = bpelVarModel.getXSDElement();
						if (schemaGlobalElem != null) {
							schemaType = schemaGlobalElem.getType();
						} 
						if (schemaType == null) {
							throw new RuntimeException(I18n.loc("BPCOR-6191: Unable to get Schema Type for " +
									"variable {0}.", bpelVarModel.getName()));
						}
					} 

					if (!schemaType.isSimpleType()) {
						// DEVNOTE VM: when the output variable is based on element type, we need to create a DOM 
						// even if it is of simple type. This is so that the element can be copied over to the 
						// variable. However we do not do this because the call the Utility.updateNode() takes care
						// of the element creation when needed. This is better for performance.
						value = DOMHelper.createDOM(value.toString());
					} 
					bpelVar.setXSDVariableData(value);
				} else {
					WSMessage message = bpelVar.getWSMessage();
					if (message == null) {
						Utility.initializeVariableValue(bpelVar);
					} else {
						Utility.createCopyIfRequired(bpelVar);
					}
					// get the message one more time
					message = bpelVar.getWSMessage();
					String partName = outputVariable.getBpelVarPart();
					Node partNode = message.getPart(partName);
					if (partNode == null) {
						partNode = message.createPart(partName);
					}

					// Get the schema type
					SchemaType schemaType = null;
					Part part = bpelVarModel.getWSDLMessageType().getPart(partName);                    
					if (part != null) {
						if (part.getTypeName() != null) {
							schemaType = WSDL4JExt.getSchemaTypeLoader(part).findType(part.getTypeName());
						} else {
							SchemaField schemaField = Utility.getGlobalElement(bpelVarModel, part.getElementName());
							if (schemaField != null) {
								schemaType = schemaField.getType();
							}
						}
					} 

					if (schemaType == null) {
						throw new RuntimeException(I18n.loc("BPCOR-6191: Unable to get Schema Type for " +
								"variable {0}.", bpelVarModel.getName()));
					}

					if (!schemaType.isSimpleType()) {
						// DEVNOTE VM: when the output variable is based on element type, we need to create a DOM 
						// even if it is of simple type. This is so that the element can be copied over to the 
						// variable. However we do not do this because the call the Utility.updateNode() takes care
						// of the element creation when needed. This is better for performance.
						value = DOMHelper.createDOM(value.toString());
					} 
					Utility.updateNode(partNode, value);
				}
				updateVariableList.put(bpelVarModel, bpelVar);
			}
		} catch (Exception exp) {
			String error = I18n.loc("Error in executing extensionAssignOperation");
			throw new SystemException(error, exp);
		} finally {
			org.mozilla.javascript.Context.exit();
		}
	}
}

/**
 * It models inputVars and outputVar defined on
 * extensionAssignOperation.Expression to process them conveniently .
 */
class ExtLangVariable {
	private String extLangVarName;
	private String bpelVarName;
	private String bpelVarPart;
	private boolean isXSDVariable;

	ExtLangVariable(String extLangVarName, String bpelVarName,
			String bpelVarNamePart, boolean isXSDVariable) {
		this.extLangVarName = extLangVarName;
		this.bpelVarName = bpelVarName;
		this.bpelVarPart = bpelVarNamePart;
		this.isXSDVariable = isXSDVariable;
	}

	public String getExtLangVarName() {
		return extLangVarName;
	}

	public String getBpelVarName() {
		return bpelVarName;
	}

	public String getBpelVarPart() {
		return bpelVarPart;
	}

	public boolean isXSDVariable() {
		return isXSDVariable;
	}

	public String toString() {
		return " { extLangVarName: " + extLangVarName + " bpelVarName: "
		+ bpelVarName + " bpelVarPart: " + bpelVarPart
		+ " isXSDVariable: " + isXSDVariable + " } ";
	}
	/**
	 * It parses inputVar and ouputVar defined on extensionAssignOperation.Expression. Format for
	 * " input is <script-var-name>=<bpel-var-name>.<bpel-var-part>,..." and output is 
	 * "<bpel-var-name>.<bpel-var-part>=<script-var-name>,..."

	 * @param extLangVariableExp - comma separated list of input/output variables as per the format defiend above. 
	 * @param output when it is true, expression is in output var format. 
	 * @return
	 */

	static Set<ExtLangVariable> parse(String extLangVariableExp, boolean output) {
		Set<ExtLangVariable> extLangVariables = new HashSet<ExtLangVariable>();
		StringTokenizer st = new StringTokenizer(extLangVariableExp, ",");
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			int indexofEqual = token.indexOf('=');
			String extLangVarName = token.substring(0, indexofEqual);
			String bpelVarName = token.substring(indexofEqual + 1);

			if (output) {
				String tmp = bpelVarName;
				bpelVarName = extLangVarName;
				extLangVarName = tmp;
			}

			boolean isXSDVariable = true;
			String bpelVarPart = null;

			int indexofDot = bpelVarName.indexOf('.');
			if (indexofDot > 0) {
				isXSDVariable = false;
				bpelVarPart = bpelVarName.substring(indexofDot + 1);
				bpelVarName = bpelVarName.substring(0, indexofDot);
			}

			extLangVariables.add(new ExtLangVariable(extLangVarName,
					bpelVarName, bpelVarPart, isXSDVariable));
		}

		return extLangVariables;
	}
}
