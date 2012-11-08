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
 * @(#)AdaptorFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.immutable.impl;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.xml.namespace.QName;

import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.BindingFault;
import com.sun.wsdl.model.BindingInput;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.BindingOutput;
import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.Operation;
import com.sun.wsdl.model.OperationFault;
import com.sun.wsdl.model.OperationInput;
import com.sun.wsdl.model.OperationOutput;
import com.sun.wsdl.model.Part;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.Service;
import com.sun.wsdl.model.PartnerLinkRole;
import com.sun.wsdl.model.PartnerLinkType;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.Types;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLExtensibleElement;
import com.sun.wsdl.model.WSDLMessage;
import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.bpel.PropertyAlias;
import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.immutable.AdaptorFactory;
import com.sun.wsdl.model.immutable.BindingFaultImm;
import com.sun.wsdl.model.immutable.BindingImm;
import com.sun.wsdl.model.immutable.BindingInputImm;
import com.sun.wsdl.model.immutable.BindingOperationImm;
import com.sun.wsdl.model.immutable.BindingOutputImm;
import com.sun.wsdl.model.immutable.ImportImm;
import com.sun.wsdl.model.immutable.MetaProviderSetImm;
import com.sun.wsdl.model.immutable.OperationFaultImm;
import com.sun.wsdl.model.immutable.OperationImm;
import com.sun.wsdl.model.immutable.OperationInputImm;
import com.sun.wsdl.model.immutable.OperationOutputImm;
import com.sun.wsdl.model.immutable.PartImm;
import com.sun.wsdl.model.immutable.PortTypeImm;
import com.sun.wsdl.model.immutable.ServiceImm;
import com.sun.wsdl.model.immutable.ServiceLinkPortTypeImm;
import com.sun.wsdl.model.immutable.ServiceLinkRoleImm;
import com.sun.wsdl.model.immutable.ServiceLinkTypeImm;
import com.sun.wsdl.model.immutable.ServicePortImm;
import com.sun.wsdl.model.immutable.TypesImm;
import com.sun.wsdl.model.immutable.WSDLDefinitionsImm;
import com.sun.wsdl.model.immutable.WSDLDocumentImm;
import com.sun.wsdl.model.immutable.WSDLElementImm;
import com.sun.wsdl.model.immutable.WSDLExtensibleElementImm;
import com.sun.wsdl.model.immutable.WSDLMessageImm;
import com.sun.wsdl.model.immutable.extensions.ExtensibilityElementImm;
import com.sun.wsdl.model.immutable.extensions.PropertyAliasImm;
import com.sun.wsdl.model.immutable.extensions.PropertyImm;

/**
 *  
 * Created on Jul 14, 2004
 *
 * @author Sun Microsystems
 * @version 1.0
 * @since   5.1.0
 */

public final class AdaptorFactoryImpl implements AdaptorFactory {
        
        
    /** @see com.stc.bpms.common.integration.model.adaptor.AdaptorFactory#getImmutable(com.stc.bpms.common.model.wsdl.WSDLDocument)
     */
    public WSDLDocumentImm getImmutable(WSDLDocument wsdlDoc) {
        return new WSDLDocumentImmImpl(wsdlDoc);
    }
            
    private class WSDLDocumentImmImpl implements WSDLDocumentImm {
        
        WSDLDocument mWSDLDoc;
        WSDLDocumentImmImpl(WSDLDocument wsdlDoc) {
            mWSDLDoc = wsdlDoc;
        }
        
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDocumentImm#getDocumentDefinitions()
         */
        public WSDLDefinitionsImm getDocumentDefinitions() {
            return new WSDLDefinitionsImmImpl(mWSDLDoc.getDocumentDefinitions(), this);
        }
        

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDocumentImm#serialize(java.lang.Object[])
         */
        public String serialize(Object[] formatArgs) {
            StringWriter writer = new StringWriter();
            mWSDLDoc.serialize(writer);
            return writer.toString();
        }

    }
        
    private class WSDLDefinitionsImmImpl extends WSDLElementImmImpl implements WSDLDefinitionsImm {

    	WSDLDefinitions mWSDLDef;
        List mWSDLDocImms = new ArrayList();
        List mImportImms = new ArrayList();
        List mWSDLMesgs = new ArrayList();
        List mServiceLinkTypes = new ArrayList();
        List mPortTypes = new ArrayList();
        List mTypes = new ArrayList();
        List mServices = new ArrayList();
        List mAllWSDLDocs = new ArrayList();
        List mBindings = new ArrayList();
        List mPropAliases = new ArrayList();
        List mProps = new ArrayList();
                                
        WSDLDefinitionsImmImpl(WSDLDefinitions wsdlDef, WSDLDocumentImm ownerDoc) {
            super(wsdlDef, ownerDoc);
            mWSDLDef = wsdlDef;
        }
        
        /** @see com.stc.bpms.common.integration.model.WSDLElementImm#getTargetNamespace()
         */
        public String getTargetNamespace() {
            return mWSDLDef.getTargetNamespace();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getName()
         */
        public String getName() {
            return mWSDLDef.getName();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getImports()
         */
        public List getImports() {
            if (mImportImms.isEmpty()) {
                Collection docs = mWSDLDef.getImports();
                for (Iterator itr = docs.iterator(); itr.hasNext();) {
                    Import importElem = (Import) itr.next();
                    mImportImms.add(new ImportImmImpl(importElem, mOwnerDoc)); 
                }                               
            }
            return Collections.unmodifiableList(mImportImms);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getImportedDocuments()
         */
        public List getImportedDocuments() {
            if (mWSDLDocImms.isEmpty()) {
                Collection docs = mWSDLDef.getImportedDocuments();
                for (Iterator itr = docs.iterator(); itr.hasNext();) {
                    WSDLDocument wsdlDoc = (WSDLDocument) itr.next();
                    mWSDLDocImms.add(new WSDLDocumentImmImpl(wsdlDoc)); 
                }                               
            }

            return Collections.unmodifiableList(mWSDLDocImms);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getTypes()
         */
        public TypesImm getTypes() {
            return new TypesImmImpl(mWSDLDef.getTypes(), mOwnerDoc);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getMessages()
         */
        public List getMessages() {
            if (mWSDLMesgs.isEmpty()) {
                Collection msgs = mWSDLDef.getMessages();
                for (Iterator itr = msgs.iterator(); itr.hasNext();) {
                    WSDLMessage mesg = (WSDLMessage) itr.next();
                    mWSDLMesgs.add(new WSDLMessageImmImpl(mesg, mOwnerDoc)); 
                }                               
            }
            return Collections.unmodifiableList(mWSDLMesgs);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getPortTypes()
         */
        public List getPortTypes() {
            if (mPortTypes.isEmpty()) {
                Collection portTypes = mWSDLDef.getPortTypes();
                for (Iterator itr = portTypes.iterator(); itr.hasNext();) {
                    PortType portType = (PortType) itr.next();
                    mPortTypes.add(new PortTypeImmImpl(portType, mOwnerDoc)); 
                }               
            }
            return Collections.unmodifiableList(mPortTypes);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getBindings()
         */
        public List getBindings() {
            if (mBindings.isEmpty()) {
                Collection docs = mWSDLDef.getBindings();
                for (Iterator itr = docs.iterator(); itr.hasNext();) {
                    Binding binding = (Binding) itr.next();
                    mBindings.add(new BindingImmImpl(binding, mOwnerDoc)); 
                }                               
            }
            return Collections.unmodifiableList(mBindings);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getServices()
         */
        public List getServices() {
            if (mServices.isEmpty()) {
                Collection services = mWSDLDef.getServices();
                for (Iterator itr = services.iterator(); itr.hasNext();) {
                    Service service = (Service) itr.next();
                    mServices.add(new ServiceImmImpl(service, mOwnerDoc)); 
                }               
            }
            return Collections.unmodifiableList(mServices);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getServiceLinkTypes()
         */
        public List getServiceLinkTypes() {
            if (mServiceLinkTypes.isEmpty()) {
                Collection serviceTypes = mWSDLDef.getServiceLinkTypes();
                for (Iterator itr = serviceTypes.iterator(); itr.hasNext();) {
                    PartnerLinkType serviceType = (PartnerLinkType) itr.next();
                    mServiceLinkTypes.add(new ServiceLinkTypeImmImpl(serviceType, mOwnerDoc)); 
                }               
            }
            return Collections.unmodifiableList(mServiceLinkTypes);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getBPWSProperties()
         */
        public List getBPWSProperties() {
            if (mProps.isEmpty()) {
                Collection props = mWSDLDef.getBPWSProperties();
                for (Iterator itr = props.iterator(); itr.hasNext();) {
                    Property prop = (Property) itr.next();
                    mProps.add(new PropertyImmImpl(prop, mOwnerDoc)); 
                }                               
            }
            return Collections.unmodifiableList(mProps);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLDefinitionsImm#getBPWSPropertyAliases()
         */
        public List getBPWSPropertyAliases() {
            if (mPropAliases.isEmpty()) {
                Collection mPropAliases = mWSDLDef.getBPWSPropertyAliases();
                for (Iterator itr = mPropAliases.iterator(); itr.hasNext();) {
                    PropertyAlias propAlias = (PropertyAlias) itr.next();
                    mPropAliases.add(new PropertyAliasImmImpl(propAlias, mOwnerDoc)); 
                }                               
            }
            return Collections.unmodifiableList(mPropAliases);
        }
        
       
    }

    private class WSDLElementImmImpl implements WSDLElementImm {
                
        protected XMLElement mElem;
        protected WSDLDocumentImm mOwnerDoc;
                
        WSDLElementImmImpl(XMLElement elem, WSDLDocumentImm ownerDoc) {
            mOwnerDoc = ownerDoc;
            mElem = elem;
        }
        
        /** @see com.stc.bpms.common.integration.model.WSDLElementImm#getOtherAttributes()
         */
        public Map getOtherAttributes() {
            return mElem.getOtherAttributes();
        }

        /** @see com.stc.bpms.common.integration.model.WSDLElementImm#getDefaultNamespace()
         */
        public String getNamespace() {
            return mElem.getDefaultNamespace();
        }

        /** @see com.stc.bpms.common.integration.model.WSDLElementImm#getNamespace(java.lang.String)
         */
        public String getNamespace(String prefix) {
            return mElem.getNamespace(prefix);
        }

        /** @see com.stc.bpms.common.integration.model.WSDLElementImm#getNamespacePrefix(java.lang.String)
         */
        public String getNamespacePrefix(String namespaceURI) {
            return mElem.getNamespace(namespaceURI);
        }

        /** @see com.stc.bpms.common.integration.model.WSDLElementImm#getOwnerDocument()
         */
        public WSDLDocumentImm getOwnerDocument() {
            return mOwnerDoc;
        }
    }
        
    private class BindingFaultImmImpl extends WSDLExtensibleElementImmImpl implements BindingFaultImm {
                
        BindingFaultImmImpl(BindingFault fault, WSDLDocumentImm ownerDoc) {
            super(fault, ownerDoc);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingFaultImm#getName()
         */
        public String getName() {
            return ((BindingFault) mElem).getName();
        }

    }
        
    private class BindingImmImpl extends WSDLExtensibleElementImmImpl implements BindingImm {

        List mOpers = new ArrayList(); 
                
        BindingImmImpl(Binding binding, WSDLDocumentImm ownerDoc) {
            super(binding, ownerDoc);
        }
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingImm#getBindingOperation()
         */
        public List getBindingOperation() {
            if (mOpers.isEmpty()) {
                int count = ((Binding) mElem).getBindingOperationSize();
                for (int i = 0; i < count; i++) {
                    BindingOperation oper = ((Binding) mElem).getBindingOperation(i);
                    mOpers.add(new BindingOperationImmImpl(oper, getOwnerDocument())); 
                }               
            }
            return Collections.unmodifiableList(mOpers);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingImm#getName()
         */
        public String getName() {
            return ((Binding) mElem).getName();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingImm#getType()
         */
        public String getType() {
        	QName portTypeQName = ((Binding) mElem).getType();
        	if(portTypeQName != null) {
        		return portTypeQName.toString();
        	}
        	
            return null;
        }
    }

    private class BindingInputImmImpl extends WSDLExtensibleElementImmImpl implements BindingInputImm {

                
        BindingInputImmImpl(BindingInput bInput, WSDLDocumentImm wsdlDoc) {
            super(bInput, wsdlDoc);     
        }
    }
        
    private class BindingOperationImmImpl extends WSDLElementImmImpl implements BindingOperationImm {
                
        private List mBFaults = new ArrayList();
                 
        BindingOperationImmImpl(BindingOperation bOper, WSDLDocumentImm wsdlDoc) {
            super(bOper, wsdlDoc);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingOperationImm#getBindingFaults()
         */
        public List getBindingFaults() {
            if (mBFaults.isEmpty()) {
                int count = ((BindingOperation) mElem).getBindingFaultSize();
                for (int i = 0; i < count; i++) {
                    BindingFault fault = ((BindingOperation) mElem).getBindingFault(i); 
                    mBFaults.add(new BindingFaultImmImpl(fault, mOwnerDoc));
                }
            }
            return Collections.unmodifiableList(mBFaults);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingOperationImm#getBindingInput()
         */
        public BindingInputImm getBindingInput() {
            if (((BindingOperation) mElem).getBindingInput() == null) {
                return null;
            }
            return new BindingInputImmImpl
                (((BindingOperation) mElem).getBindingInput(), mOwnerDoc);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingOperationImm#getBindingOutput()
         */
        public BindingOutputImm getBindingOutput() {
            if (((BindingOperation) mElem).getBindingOutput() == null) {
                return null;
            }
            return new BindingOutputImmImpl
                (((BindingOperation) mElem).getBindingOutput(), mOwnerDoc);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingOperationImm#getName()
         */
        public String getName() {
            return ((BindingOperation) mElem).getName();
        }
    }
        
    private class BindingOutputImmImpl extends WSDLExtensibleElementImmImpl implements BindingOutputImm {
                
        BindingOutputImmImpl(BindingOutput bOutput, WSDLDocumentImm wsdlDoc) {
            super(bOutput, wsdlDoc);    
        }
    }
        
    private class ImportImmImpl extends WSDLElementImmImpl implements ImportImm {
                
        ImportImmImpl(Import imp, WSDLDocumentImm ownerDoc) {
            super(imp, ownerDoc);       
        }
                        
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ImportImm#getLocation()
         */
        public String getLocation() {
            return ((Import) mElem).getLocation();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ImportImm#getNamespaceAttr()
         */
        public String getNamespaceAttr() {
            return ((Import) mElem).getNamespaceAttr();
        }
    }
        
    private class OperationFaultImmImpl extends WSDLElementImmImpl implements OperationFaultImm {
                
        OperationFaultImmImpl(OperationFault fault, WSDLDocumentImm ownerDoc) {
            super(fault, ownerDoc);
        }
                                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.OperationFaultImm#getMessage()
         */
        public QName getMessage() {
            return ((OperationFault) mElem).getMessage();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.OperationFaultImm#getName()
         */
        public String getName() {
            return ((OperationFault) mElem).getName();
        }
    }
        
    private class OperationImmImpl extends WSDLElementImmImpl implements OperationImm {
                
        private List mFaults = new ArrayList();
                 
        OperationImmImpl(Operation bOper, WSDLDocumentImm ownerDoc) {
            super(bOper, ownerDoc);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingOperationImm#getBindingFaults()
         */
        public List getFaults() {
            if (mFaults.isEmpty()) {
                int count = ((Operation) mElem).countFaults();
                for (int i = 0; i < count; i++) {
                    OperationFault fault = ((Operation) mElem).getFault(i); 
                    mFaults.add(new OperationFaultImmImpl(fault, mOwnerDoc));
                }
            }
            return Collections.unmodifiableList(mFaults);
        }               

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.OperationImm#getInput()
         */
        public OperationInputImm getInput() {
            if (((Operation) mElem).getInput() == null) {
                return null;
            }
            return new OperationInputImmImpl(((Operation) mElem).getInput(), mOwnerDoc);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.OperationImm#getName()
         */
        public String getName() {
            return ((Operation) mElem).getName();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.OperationImm#getOperationType()
         */
        public int getOperationType() {
            return ((Operation) mElem).getOperationType();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.OperationImm#getOutput()
         */
        public OperationOutputImm getOutput() {
            if (((Operation) mElem).getOutput() == null) {
                return null;
            }
            return new OperationOutputImmImpl(((Operation) mElem).getOutput(), mOwnerDoc);
        }
    }
        
    private class OperationInputImmImpl extends WSDLElementImmImpl implements OperationInputImm {
                
        OperationInputImmImpl(OperationInput bInput, WSDLDocumentImm wsdlDoc) {
            super(bInput, wsdlDoc);     
        }
                 
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingInputImm#getMessage()
         */
        public QName getMessage() {
            return  ((OperationInput) mElem).getMessage();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.BindingInputImm#getName()
         */
        public String getName() {
            return ((OperationInput) mElem).getName();
        }
    }
        
    private class OperationOutputImmImpl extends WSDLElementImmImpl implements OperationOutputImm {
                
        OperationOutputImmImpl(OperationOutput output, WSDLDocumentImm wsdlDoc) {
            super(output, wsdlDoc);     
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.OperationOutputImm#getMessage()
         */
        public QName getMessage() {
            return ((OperationOutput) mElem).getMessage();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.OperationOutputImm#getName()
         */
        public String getName() {
            return ((OperationOutput) mElem).getName();
        }
    }
        
    private class PartImmImpl extends WSDLElementImmImpl implements PartImm {

        PartImmImpl(Part part, WSDLDocumentImm ownerDoc) {
            super(part, ownerDoc);
        }
                        
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.PartImm#getElement()
         */
        public QName getElement() {
            return ((Part) mElem).getElement();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.PartImm#getName()
         */
        public String getName() {
            return ((Part) mElem).getName();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.PartImm#getType()
         */
        public QName getType() {
            return ((Part) mElem).getType();
        }
        
    }
        
    private class PortTypeImmImpl extends WSDLElementImmImpl implements PortTypeImm {

        List mOpers = new ArrayList();
        PortTypeImmImpl(PortType pType, WSDLDocumentImm ownerDoc) {
            super(pType, ownerDoc);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.PortTypeImm#getName()
         */
        public String getName() {
            return ((PortType) mElem).getName();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.PortTypeImm#getOperations()
         */
        public List getOperations() {
            if (mOpers.isEmpty()) {
                Collection opers = ((PortType) mElem).getOperations();
                for (Iterator itr = opers.iterator(); itr.hasNext();) {
                    Operation oper = (Operation) itr.next();
                    mOpers.add(new OperationImmImpl(oper, mOwnerDoc));
                }
            }
            return Collections.unmodifiableList(mOpers);
        }
    }
        
    private class ServiceImmImpl extends WSDLElementImmImpl implements ServiceImm {
                
        List mPorts = new ArrayList();
        ServiceImmImpl(Service service, WSDLDocumentImm ownerDoc) {
            super(service, ownerDoc);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ServiceImm#getName()
         */
        public String getName() {
            return ((Service) mElem).getName();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ServiceImm#getPorts()
         */
        public List getPorts() {
            if (mPorts.isEmpty()) {
                Collection ports = ((Service) mElem).getPorts();
                for (Iterator itr = ports.iterator(); itr.hasNext();) {
                    ServicePort port = (ServicePort) itr.next();
                    mPorts.add(new ServicePortImmImpl(port, mOwnerDoc));
                }
            }
            return Collections.unmodifiableList(mPorts);
        }
    }
        
    private class ServiceLinkRoleImmImpl extends WSDLElementImmImpl implements ServiceLinkRoleImm {
                
        List mPortTypes = new ArrayList();
                
        ServiceLinkRoleImmImpl(PartnerLinkRole sLink, WSDLDocumentImm ownerDoc) {
            super(sLink, ownerDoc);
        }
                                  
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ServiceLinkRoleImm#getName()
         */
        public String getName() {
            return ((PartnerLinkRole) mElem).getName();
        }
       
    }
        
    private class ServiceLinkTypeImmImpl extends WSDLElementImmImpl implements ServiceLinkTypeImm {
                
        List mRoles = new ArrayList();
                
        ServiceLinkTypeImmImpl(PartnerLinkType sLinkType, WSDLDocumentImm ownerDoc) {
            super(sLinkType, ownerDoc);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ServiceLinkTypeImm#getName()
         */
        public String getName() {
            return ((PartnerLinkType) mElem).getName();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ServiceLinkTypeImm#getRoles()
         */
        public List getRoles() {
            if (mRoles.isEmpty()) {
                Collection roles = ((PartnerLinkType) mElem).getRoles();
                for (Iterator itr = roles.iterator(); itr.hasNext();) {
                    PartnerLinkRole role = (PartnerLinkRole) itr.next();
                    mRoles.add(new ServiceLinkRoleImmImpl(role, mOwnerDoc));
                }
            }
            return Collections.unmodifiableList(mRoles);
        }
    }
        
    private class ServicePortImmImpl extends WSDLElementImmImpl implements ServicePortImm {
                
        ServicePortImmImpl(ServicePort sPort, WSDLDocumentImm ownerDoc) {
            super(sPort, ownerDoc);
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ServicePortImm#getBinding()
         */
        public String getBinding() {
        	QName bindingQName = ((ServicePort) mElem).getBinding();
        	if(bindingQName != null) {
        		return bindingQName.toString();
        	}
            return null;
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.ServicePortImm#getName()
         */
        public String getName() {
            return ((ServicePort) mElem).getName();
        }
    }
        
    private class TypesImmImpl extends WSDLExtensibleElementImmImpl implements TypesImm {
                
        TypesImmImpl(Types types, WSDLDocumentImm ownerDoc) {
            super(types, ownerDoc);
        }
    }
        
    private class WSDLExtensibleElementImmImpl extends WSDLElementImmImpl 
        implements WSDLExtensibleElementImm {
                        
        List mExtensibleElements = new ArrayList();
                
        WSDLExtensibleElementImmImpl(WSDLExtensibleElement extElem, WSDLDocumentImm ownerDoc) {
            super(extElem, ownerDoc);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLExtensibleElementImm#getExtensibilityElements()
         */
        public List getExtensibilityElements() {
            if (mExtensibleElements.isEmpty()) {
                Collection extensibleElems = ((WSDLExtensibleElement) mElem).getExtensibilityElements();
                for (Iterator itr = extensibleElems.iterator(); itr.hasNext();) {
                    WSDLExtensibleElement extensibleElem = (WSDLExtensibleElement) itr.next();
                    mExtensibleElements.add(new WSDLExtensibleElementImmImpl(extensibleElem, mOwnerDoc));
                }
            }
            return Collections.unmodifiableList(mExtensibleElements); 
        }
    }
        
    private class WSDLMessageImmImpl extends WSDLElementImmImpl implements WSDLMessageImm {
                
        List mParts = new ArrayList();
                
        WSDLMessageImmImpl(WSDLMessage msg, WSDLDocumentImm ownerDoc) {
            super(msg, ownerDoc);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLMessageImm#getName()
         */
        public String getName() {
            return ((WSDLMessage) mElem).getName();
        }

        /** @see com.stc.bpms.common.integration.model.wsdlproducer.WSDLMessageImm#getParts()
         */
        public List getParts() {
            if (mParts.isEmpty()) {
                int count = ((WSDLMessage) mElem).getPartSize();
                for (int i = 0; i < count; i++) {
                    Part part = (Part) ((WSDLMessage) mElem).getPart(i);
                    mParts.add(new PartImmImpl(part, mOwnerDoc));
                }
            }
            return Collections.unmodifiableList(mParts);
        }
    }
        
    private class ExtensibilityElementImmImpl extends WSDLExtensibleElementImmImpl 
        implements ExtensibilityElementImm {
                        
        ExtensibilityElementImmImpl(ExtensibilityElement extenElem, WSDLDocumentImm ownerDoc) {
            super(extenElem, ownerDoc);
        }
                
        /** @see com.stc.bpms.common.integration.model.wsdlproducer.extensions.ExtensibilityElementImm#getElementType()
         */
        public QName getElementType() {
            return ((ExtensibilityElement) mElem).getElementType();
        }
    }
    
    private class PropertyAliasImmImpl extends WSDLElementImmImpl implements PropertyAliasImm {
        
        PropertyAliasImmImpl(PropertyAlias pAlias, WSDLDocumentImm ownerDoc) {
            super(pAlias, ownerDoc);
        }
        
        /**
         * @see com.stc.bpms.common.integration.model.wsdlproducer.extensions.PropertyAliasImm#getMessageType()
         */
        public QName getMessageType() {
            
            return ((PropertyAlias) mElem).getMessageType();
        }

        /**
         * @see com.stc.bpms.common.integration.model.wsdlproducer.extensions.PropertyAliasImm#getPart()
         */
        public String getPart() {
            
            return ((PropertyAlias) mElem).getPart();
        }

        /**
         * @see com.stc.bpms.common.integration.model.wsdlproducer.extensions.PropertyAliasImm#getPropertyName()
         */
        public QName getPropertyName() {
            
            return ((PropertyAlias) mElem).getPropertyName();
        }

        /**
         * @see com.stc.bpms.common.integration.model.wsdlproducer.extensions.PropertyAliasImm#getQuery()
         */
        public String getQuery() {
            
            return ((PropertyAlias) mElem).getQuery();
        }

        /**
         * @see com.stc.bpms.common.integration.model.wsdlproducer.extensions.PropertyAliasImm#getSelect()
         */
        public String getSelect() {
            
            return ((PropertyAlias) mElem).getQuery();
        }

    }
    
    private class PropertyImmImpl extends WSDLElementImmImpl implements PropertyImm {

        PropertyImmImpl(Property prop, WSDLDocumentImm ownerDoc) {
            super(prop, ownerDoc);
        }
                
        /**
         * @see com.stc.bpms.common.integration.model.wsdlproducer.extensions.PropertyImm#getName()
         */
        public String getName() {
            return ((Property) mElem).getName();
        }

        /**
         * @see com.stc.bpms.common.integration.model.wsdlproducer.extensions.PropertyImm#getType()
         */
        public QName getType() {
            String typeStr = ((Property) mElem).getType();
            if (typeStr != null) {
            	return NamespaceUtility.resolveAndGetQName(typeStr, mElem);
            }
            return null;
        }
    }
}
