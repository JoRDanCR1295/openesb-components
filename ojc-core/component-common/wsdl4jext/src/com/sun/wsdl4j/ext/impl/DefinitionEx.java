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
 * @(#)DefinitionExImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Fault;
import javax.wsdl.Import;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.Types;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.schema.Schema;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.w3c.dom.Element;

import com.ibm.wsdl.DefinitionImpl;
import com.sun.wsdl4j.ext.DeferredActionAccepter;
import com.sun.wsdl4j.ext.SchemaTypeLoaderHolder;
import com.sun.wsdl4j.ext.WSDLElementEx;
import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

public class DefinitionEx extends DefinitionImpl implements
        WSDLElementEx, SchemaTypeLoaderHolder, DeferredActionAccepter {

    private static final long serialVersionUID = 1L;
    
    public enum VisitStyle {
        SELF_FIRST,
        CHILD_FIRST
    }
    
    protected SchemaTypeSystem _schemaTypeSystem;
    protected SchemaTypeLoader _schemaTypeLoader;
    protected Set<MessageProperty> _messageProperties =
        new LinkedHashSet<MessageProperty>();
    protected Map<QName, MessageProperty> _messagePropertyMap =
        new HashMap<QName, MessageProperty>();
    protected Set<MessagePropertyAlias> _propertyAliases =
        new LinkedHashSet<MessagePropertyAlias>();
    protected Map<QName, Collection<MessagePropertyAlias>> _propertyAliasMap =
        new HashMap<QName, Collection<MessagePropertyAlias>>();
    protected Set<PartnerLinkType> _partnerLinkTypes =
        new LinkedHashSet<PartnerLinkType>();
    protected Map<QName, PartnerLinkType> _partnerLinkTypeMap =
        new HashMap<QName, PartnerLinkType>();

    @Override
    public Binding createBinding() {
        BindingEx binding = new BindingEx();
        binding.setContainingDefinition(this);
        return binding;
    }

    @Override
    public BindingFault createBindingFault() {
        BindingFaultEx bindingFault = new BindingFaultEx();
        bindingFault.setContainingDefinition(this);
        return bindingFault;
    }

    @Override
    public BindingInput createBindingInput() {
        BindingInputEx bindingInput = new BindingInputEx();
        bindingInput.setContainingDefinition(this);
        return bindingInput;
    }

    @Override
    public BindingOperation createBindingOperation() {
        BindingOperationEx bindingOperation = new BindingOperationEx();
        bindingOperation.setContainingDefinition(this);
        return bindingOperation;
    }

    @Override
    public BindingOutput createBindingOutput() {
        BindingOutputEx bindingOutput = new BindingOutputEx();
        bindingOutput.setContainingDefinition(this);
        return bindingOutput;
    }

    @Override
    public Fault createFault() {
        FaultEx fault = new FaultEx();
        fault.setContainingDefinition(this);
        return fault;
    }

    @Override
    public Import createImport() {
        ImportEx imp = new ImportEx();
        imp.setDefinition(this);
        return imp;
    }

    @Override
    public Input createInput() {
        InputEx input = new InputEx();
        input.setContainingDefinition(this);
        return input;
    }

    @Override
    public Message createMessage() {
        MessageEx message = new MessageEx();
        message.setContainingDefinition(this);
        return message;
    }

    @Override
    public Operation createOperation() {
        OperationEx operation = new OperationEx();
        operation.setContainingDefinition(this);
        return operation;
    }

    @Override
    public Output createOutput() {
        OutputEx output = new OutputEx();
        output.setContainingDefinition(this);
        return output;
    }

    @Override
    public Part createPart() {
        PartEx part = new PartEx();
        part.setContainingDefinition(this);
        return part;
    }

    @Override
    public Port createPort() {
        PortEx port = new PortEx();
        port.setContainingDefinition(this);
        return port;
    }

    @Override
    public PortType createPortType() {
        PortTypeEx portType = new PortTypeEx();
        portType.setContainingDefinition(this);
        return portType;
    }

    @Override
    public Service createService() {
        ServiceEx service = new ServiceEx();
        service.setContainingDefinition(this);
        return service;
    }

    @Override
    public Types createTypes() {
        TypesEx types = new TypesEx();
        types.setContainingDefinition(this);
        return types;
    }

    ////////////////////////////////////
    //Methods for interface DefinitionEx
    ////////////////////////////////////
    
    public SchemaTypeSystem getSchemaTypeSystem() {
        return _schemaTypeSystem;
    }
    
    public SchemaTypeLoader getSchemaTypeLoader() {
        if (_schemaTypeLoader == null) {
            Set<SchemaTypeLoader> loaders =
                new LinkedHashSet<SchemaTypeLoader>();
            loaders.add(XmlBeans.getContextTypeLoader());
            Map<String, List<Import>> imports = getImports();
            if (imports != null) {
                for (Entry<String, List<Import>> entry : imports.entrySet()) {
                    List<Import> list = entry.getValue();
                    if (list != null) {
                        for (Import imp : list) {
                            DefinitionEx childDef =
                                (DefinitionEx) imp.getDefinition();
                            loaders.addAll(
                                    childDef.getSchemaTypeSystems());
                        }
                    }
                }
            }
            
            if (_schemaTypeSystem != null) {
                loaders.add(_schemaTypeSystem);
            }
            List<SchemaTypeLoader> reverseLoaders =
                new ArrayList<SchemaTypeLoader>();
            for (SchemaTypeLoader loader : loaders) {
                reverseLoaders.add(0, loader);
            }
            _schemaTypeLoader =
                    XmlBeans.typeLoaderUnion(
                            reverseLoaders.toArray(new SchemaTypeLoader[0]));
        }
        return _schemaTypeLoader;
    }
    
    public Collection<MessageProperty> getMessageProperties() {
        return getMessageProperties(false);
    }

    public Collection<MessageProperty> getMessageProperties(boolean deep) {
        if (!deep) {
            return makeReadOnly(_messageProperties);
        }
        
        GetMessagePropertyVisitor visitor = new GetMessagePropertyVisitor();
        accept(null, visitor, VisitStyle.CHILD_FIRST, false);
        return makeReadOnly(visitor.getMessageProperties());
    }

    public MessageProperty getMessageProperty(QName name) {
        return getMessageProperty(name, false);
    }

    public MessageProperty getMessageProperty(QName name, boolean deep) {
        if (!deep) {
            return _messagePropertyMap.get(name);
        }
        
        GetMessagePropertyVisitor visitor =
            new GetMessagePropertyVisitor(name);
        accept(null, visitor, VisitStyle.CHILD_FIRST, false);
        return visitor.getMessageProperty();
    }

    public Collection<MessagePropertyAlias> getMessagePropertyAliases(
            QName name) {
        return getMessagePropertyAliases(name, false);
    }

    public Collection<MessagePropertyAlias> getMessagePropertyAliases(
            QName name, boolean deep) {
        if (!deep) {
            if (!_propertyAliasMap.containsKey(name)) {
                return new LinkedHashSet<MessagePropertyAlias>();
            }
            return makeReadOnly(_propertyAliasMap.get(name));
        }
        
        GetPropertyAliasVisitor visitor =
            new GetPropertyAliasVisitor(name);
        accept(null, visitor, VisitStyle.CHILD_FIRST, false);
        return makeReadOnly(visitor.getPropertyAliases());
    }

    public Collection<MessagePropertyAlias> getMessagePropertyAliases() {
        return getMessagePropertyAliases(false);
    }

    public Collection<MessagePropertyAlias> getMessagePropertyAliases(
            boolean deep) {
        if (!deep) {
            return makeReadOnly(_propertyAliases);
        }
        
        GetPropertyAliasVisitor visitor = new GetPropertyAliasVisitor();
        accept(null, visitor, VisitStyle.CHILD_FIRST, false);
        return makeReadOnly(visitor.getPropertyAliases());
    }

    public PartnerLinkType getPartnerLinkType(QName name) {
        return getPartnerLinkType(name, false);
    }

    public PartnerLinkType getPartnerLinkType(QName name, boolean deep) {
        if (!deep) {
            return _partnerLinkTypeMap.get(name);
        }
        
        GetPartnerLinkTypeVisitor visitor =
            new GetPartnerLinkTypeVisitor(name);
        accept(null, visitor, VisitStyle.CHILD_FIRST, false);
        return visitor.getPartnerLinkType();
    }

    public Collection<PartnerLinkType> getPartnerLinkTypes() {
        return getPartnerLinkTypes(false);
    }

    public Collection<PartnerLinkType> getPartnerLinkTypes(boolean deep) {
        if (!deep) {
            return makeReadOnly(_partnerLinkTypes);
        }
        
        GetPartnerLinkTypeVisitor visitor = new GetPartnerLinkTypeVisitor();
        accept(null, visitor, VisitStyle.CHILD_FIRST, false);
        return makeReadOnly(visitor.getPartnerLinkTypes());
    }

    ////////
    //Others
    ////////
    
    public void setMessageProperties(Set<MessageProperty> properties) {
        _messageProperties = properties;
        _messagePropertyMap.clear();
        for (MessageProperty prop : properties) {
            _messagePropertyMap.put(prop.getName(), prop);
        }
    }

    public void setMessagePropertyAliases(
            Set<MessagePropertyAlias> propertyAliases) {
        _propertyAliases = propertyAliases;
        _propertyAliasMap.clear();
        for (MessagePropertyAlias alias : propertyAliases) {
            Collection<MessagePropertyAlias> aliases =
                _propertyAliasMap.get(alias.getName());
            if (aliases == null) {
                aliases = new LinkedHashSet<MessagePropertyAlias>();
                _propertyAliasMap.put(alias.getName(), aliases);
            }
            aliases.add(alias);
        }
    }

    public void setPartnerLinkTypes(
            Set<PartnerLinkType> partnerLinkTypes) {
        _partnerLinkTypes = partnerLinkTypes;
        _partnerLinkTypeMap.clear();
        for (PartnerLinkType plt : partnerLinkTypes) {
            _partnerLinkTypeMap.put(plt.getName(), plt);
        }
    }

    public void setSchemaTypeLoader(SchemaTypeLoader loader) {
        _schemaTypeLoader = loader;
    }
    
    public void setSchemaTypeSystem(SchemaTypeSystem typeSystem) {
        _schemaTypeSystem = typeSystem;
    }
    
    public Collection<SchemaTypeSystem> getSchemaTypeSystems() {
        GetTypeSystemVisitor visitor = new GetTypeSystemVisitor();
        accept(null, visitor, VisitStyle.CHILD_FIRST, false);
        return visitor.getSchemaTypeSystems();
    }
    
    /**
     * Resolves everything that needs to be resolved.  This method is
     * more for the scenario that a schema type loader is passed in from
     * outside.
     */
    public void resolveAll() {
        //no-op for now
    }
    
    /**
     * Visits a WSDL definition and all its imported WSLDs recursively.
     * 
     * @param parentDef the WSDL definition that imports the current WSDL
     * @param visitor the visitor
     * @param style the visiting style
     * @param excludeSelf if the current WSDL definition should be excluded
     *        for visiting
     * @return <code>false</code> to stop further visiting, <code>true</code>
     *         to continue visiting.
     */
    public boolean accept(DefinitionEx parentDef, AbstractVisitor visitor,
            VisitStyle style, boolean excludeSelf) {
        
        return accept(parentDef, visitor, style, excludeSelf,
                new HashSet<DefinitionEx>());
        
    }
    
    /**
     * Visits a WSDL definition and all its imported WSLDs recursively.
     * 
     * @param parentDef the WSDL definition that imports the current WSDL
     * @param visitor the visitor
     * @param style the visiting style
     * @param excludeSelf if the current WSDL definition should be excluded
     *        for visiting
     * @param visited the set of WSDL definitions that have been visited
     * @return <code>false</code> to stop further visiting, <code>true</code>
     *         to continue visiting.
     */
    protected boolean accept(DefinitionEx parentDef, AbstractVisitor visitor,
            VisitStyle style, boolean excludeSelf, Set<DefinitionEx> visited) {

        if (visited.contains(this)) {
            return true;
        }
        visited.add(this);
        if (style.equals(VisitStyle.SELF_FIRST)) {
            boolean conti = true;
            if (!excludeSelf) {
                conti = visitor.visit(parentDef, this);
            }
            if (!conti) {
                return false;
            }
        }
        
        Map<String, List<Import>> imports = getImports();
        if (imports != null) {
            for (Entry<String, List<Import>> entry : imports.entrySet()) {
                List<Import> list = entry.getValue();
                if (list != null) {
                    for (Import imp : list) {
                        DefinitionEx childDef =
                            (DefinitionEx) imp.getDefinition();
                        if (!childDef.accept(
                                this, visitor, style, false, visited)) {
                            return false;
                        }
                    }
                }
            }
        }
        
        if (style.equals(VisitStyle.CHILD_FIRST)) {
            boolean conti = true;
            if (!excludeSelf) {
                conti = visitor.visit(parentDef, this);
            }
            if (!conti) {
                return false;
            }
        }
        return true;
    }
    
    private <E> Collection<E> makeReadOnly(Collection<E> c) {
        return Collections.unmodifiableCollection(c);
    }

    //Implements WSDLElementEx methods
    
    public Definition getContainingDefinition() {
        return this;
    }

    public void setContainingDefinition(Definition definition) {
        //ignore
    }

    //Implements DeferredActionAccepter methods
    
    public void performDeferredAction() {
        //Clear all schema nodes to free up heap space.
        if (getTypes() != null) {
            List<ExtensibilityElement> eeList =
                getTypes().getExtensibilityElements();
            ExtensibilityElement[] eeArray =
                eeList.toArray(new ExtensibilityElement[0]);
            for (int i = 0; i < eeArray.length; i++) {
                if (eeArray[i] instanceof Schema) {
                    Element domElem = ((Schema) eeArray[i]).getElement();
                    if (domElem.getParentNode() instanceof Element) {
                        ((Element) domElem.getParentNode()).removeChild(domElem);
                    }
                    getTypes().removeExtensibilityElement(eeArray[i]);
                }
            }
        }
    }
}
