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
 * @(#)CastorSwareTypeSystemImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.ModelGroup;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.SimpleType;
import org.exolab.castor.xml.schema.XMLType;

import com.sun.xml.transform.sware.schema.ImplementationType;
import com.sun.xml.transform.sware.schema.SwareComplexType;
import com.sun.xml.transform.sware.schema.SwareElement;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType;
import com.sun.xml.transform.sware.schema.SwareSchema;
import com.sun.xml.transform.sware.schema.SwareSchemaException;
import com.sun.xml.transform.sware.schema.SwareType;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;


/**
 * The instance of this class can hold multiple XML schemas and facilitates
 * look-up of schema components.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
class CastorSwareTypeSystemImpl implements SwareTypeSystem {

    private final Set<Schema> mSchemaSet;
    
    /**
     * Schema namespace map.  Maps from a schema to a map that maps from
     * a namespace URI to its prefix.
     */
    private final Map<Schema, Map<String, String>> mSchemaNamespaceMaps;
    
    /**
     * schema substitution gtoup map.  Maps from a schema to a map that maps
     * from a element declaration's qualified name to a set of qualified names
     * that refer to the elememt declarations, which can be the substitutes
     * of the element declaration.  
     */
    private final Map<Schema, Map<QName, Set<QName>>>
        mSchemaSubstitutionGroupMaps;

    private final Map<ComplexType, CastorFlatComplexTypeImpl> mFlatComplexTypes;
    
    /**
     * Default constructor
     */
    public CastorSwareTypeSystemImpl() {
        this(new HashSet<Schema>());
    }

    /**
     * Constructs from a set of schemas.
     * 
     * @param schemaList a list of schemas
     */
    public CastorSwareTypeSystemImpl(Set<Schema> schemaSet) {
        mSchemaSet = schemaSet;
        mSchemaNamespaceMaps = new HashMap<Schema, Map<String, String>>();
        mSchemaSubstitutionGroupMaps =
            new HashMap<Schema, Map<QName, Set<QName>>>();
        mFlatComplexTypes =
            new HashMap<ComplexType, CastorFlatComplexTypeImpl>();
    }
    
    static QName getQualifiedName (ElementDecl elem)
            throws SwareSchemaException {
        if (elem.isReference()) {
            elem = elem.getReference();
        }
        String localPart = elem.getName();
        if (localPart == null) {
            throw new SwareSchemaException(
                    "Non-reference element declaration must have a name.");
        }
        if (elem.getSchema() == null) {
            throw new SwareSchemaException(
                    "Element declaration must belong to a schema");
        }
        boolean isQualified = false;
        if (elem.getForm() != null && elem.getForm().isQualified()) {
            isQualified = true;
        } else {
            if (elem.getParent() instanceof Schema) {
                isQualified = true;
            } else {
                if (elem.getSchema().getElementFormDefault() != null
                        && elem.getSchema().getElementFormDefault().isQualified()) {
                    isQualified = true;
                }
            }
        }
        if (isQualified && elem.getSchema().getTargetNamespace() != null
                && elem.getSchema().getTargetNamespace().length() > 0) {
            return new QName(elem.getSchema().getTargetNamespace(), localPart);
        }
        return new QName(localPart);
    }

    static QName getQualifiedName (XMLType type)
            throws SwareSchemaException {
        String typeName = type.getName();
        if (typeName == null) {
            return null;
        }
        if (type.getSchema() == null) {
            throw new SwareSchemaException(
                    "XML type must belong to a schema");
        }
        if (type.getSchema().getTargetNamespace() != null
                && type.getSchema().getTargetNamespace().length() > 0) {
            return new QName(type.getSchema().getTargetNamespace(), typeName);
        }
        return new QName(typeName);
    }

    static QName getQualifiedName (ModelGroup group)
            throws SwareSchemaException {
        if (group.isReference()) {
            group = group.getReference();
        }
        String groupName = group.getName();
        if (groupName == null) {
            throw new SwareSchemaException("Must be a named group.");
        }
        if (group.getSchema() == null) {
            throw new SwareSchemaException("Group must belong to a schema");
        }
        if (group.getSchema().getTargetNamespace() != null
                && group.getSchema().getTargetNamespace().length() > 0) {
            return new QName(group.getSchema().getTargetNamespace(), groupName);
        }
        return new QName(groupName);
    }

    public ImplementationType getImplementationType() {
        return ImplementationType.CASTOR;
    }
    
    /**
     * Adds a schema to the system.
     * 
     * @param schema a schema
     * 
     * @see SwareTypeSystem#addSchema(SwareSchema)
     */
    public void addSchema(SwareSchema swareSchema)
            throws SwareSchemaException {
        if (!swareSchema.getImplementationType().equals(
                ImplementationType.CASTOR)) {
            throw new IllegalArgumentException(
                    "The schema implementation type must be CASTOR, found: "
                    + swareSchema.getImplementationType());
        }
        Schema schema = (Schema) swareSchema.getOpaqueWrappedObject();
        if (mSchemaSet.contains(schema)) {
            return;
        }
        mSchemaSet.add(schema);
    }
    
    /**
     * Finds an element declaration in the system.
     * 
     * @param qName the qualified name of the element declaration
     * @return the element declaraction
     * @see SwareTypeSystem#findElement(QName)
     */
    public SwareElement findElement(QName qName)
            throws SwareSchemaException {
        ElementDecl elemDecl = castorFindElement(qName);
        if (elemDecl != null) {
            return new CastorSwareElementImpl(elemDecl);
        }
        return null;
    }
    
    /**
     * Finds an XML type definition in the system.
     * 
     * @param qName the qualified name of the XML type
     * @return the XML type definition
     * @see SwareTypeSystem#findType(QName)
     */
    public SwareType findType(QName qName) throws SwareSchemaException {
        XMLType xmlType = castorFindType(qName);
        if (xmlType instanceof ComplexType) {
            return new CastorSwareComplexTypeImpl((ComplexType) xmlType);
        } else if (xmlType instanceof SimpleType) {
            return new CastorSwareSimpleTypeImpl((SimpleType) xmlType);
        }
        //Not found
        return null;
    }
    
    /**
     * @see SwareTypeSystem#getSubstitutionSet(QName)
     */
    public Collection<QName> getSubstitutionSet(QName qName)
            throws SwareSchemaException {
        for (Schema schema : mSchemaSet) {
            Map<QName, Set<QName>> map = getSubstGrpMap(schema);
            Set<QName> substSet = map.get(qName);
            if (substSet != null) {
                return Collections.unmodifiableCollection(substSet);
            }
        }
        return null;
    }

    /**
     * @see SwareTypeSystem#getFlatComplexType(SwareComplexType)
     */
    public SwareFlatComplexType getFlatComplexType(
            SwareComplexType swareComplexType)
            throws SwareSchemaException {
        if (!swareComplexType.getImplementationType().equals(
                ImplementationType.CASTOR)) {
            throw new IllegalArgumentException(
                    "The schema implementation type must be CASTOR, found: "
                    + swareComplexType.getImplementationType());
        }
        ComplexType complexType =
            (ComplexType) swareComplexType.getOpaqueWrappedObject();
        CastorFlatComplexTypeImpl fct = mFlatComplexTypes.get(complexType); 
        if (fct != null) {
            return fct;
        }
        fct = new CastorFlatComplexTypeImpl(this, complexType);
        mFlatComplexTypes.put(complexType, fct);
        return fct;
    }
    
    /**
     * Finds an element declaration in the system.
     * 
     * @param qName the qualified name of the element declaration
     * @return the element declaraction
     */
    ElementDecl castorFindElement(QName qName)
            throws SwareSchemaException {
        for (Schema schema : mSchemaSet) {
            boolean foundSchema = false;
            String prefix = null;
            if (qName.getNamespaceURI() != null) {
                if (qName.getNamespaceURI().equals(schema.getTargetNamespace())
                        || schema.getImportedSchema(qName.getNamespaceURI())
                            != null) {
                    Map<String, String> nsToPrefixMap =
                        getNsToPrefixMap(schema);
                    prefix = nsToPrefixMap.get(qName.getNamespaceURI());
                    foundSchema = true;
                }                    
            } else {
                if (schema.getTargetNamespace() == null ||
                        schema.getImportedSchema("") != null) {
                    foundSchema = true;
                }
            }
            if (foundSchema) {
                String cName = qName.getLocalPart(); 
                if (prefix != null && prefix.length() > 0) {
                    cName = prefix + ":" + cName;
                }
                ElementDecl eDecl = null;
                try {
                    eDecl = schema.getElementDecl(cName);
                } catch (IllegalArgumentException e) {
                    //Don't know why it uses this Exception.
                    //It is a truly abuse.
                }
                if (eDecl != null) {
                    return eDecl;
                }
            }
        }
        //Not found
        return null;
    }
    
    /**
     * Finds an XML type definition in the system.
     * 
     * @param qName the qualified name of the XML type
     * @return the XML type definition
     */
    XMLType castorFindType(QName qName) throws SwareSchemaException {
        for (Schema schema : mSchemaSet) {
            boolean foundSchema = false;
            String prefix = null;
            if (qName.getNamespaceURI() != null) {
                if (qName.getNamespaceURI().equals(schema.getTargetNamespace())
                        || schema.getImportedSchema(qName.getNamespaceURI())
                            != null) {
                    Map<String, String> nsToPrefixMap =
                        getNsToPrefixMap(schema);
                    prefix = nsToPrefixMap.get(qName.getNamespaceURI());
                    foundSchema = true;
                }                    
            } else {
                if (schema.getTargetNamespace() == null ||
                        schema.getImportedSchema("") != null) {
                    foundSchema = true;
                }
            }
            if (foundSchema) {
                String cName = qName.getLocalPart(); 
                if (prefix != null && prefix.length() > 0) {
                    cName = prefix + ":" + cName;
                }
                XMLType xmlType = null;
                try {
                    xmlType = schema.getType(cName);
                } catch (IllegalArgumentException e) {
                    //Don't know why it uses this Exception.
                    //It is a truly abuse.
                }
                if (xmlType != null) {
                    return xmlType;
                }
            }
        }
        //Not found
        return null;
    }
    
    /**
     * Gets the namespace URI to prefix map of a schema.
     * 
     * @param schema a schema
     * @return the namespace URI to prefix map of the schema
     */
    private Map<String, String> getNsToPrefixMap(Schema schema) {
        Map<String, String> map = mSchemaNamespaceMaps.get(schema);
        if (map != null) {
            return map;
        }
        map = new HashMap<String, String>();
        if (schema.getNamespaces() == null
                || schema.getNamespaces().isEmpty()) {
            return Collections.unmodifiableMap(map);
        }
        for (Iterator iter = schema.getNamespaces().entrySet().iterator();
                iter.hasNext();) {
            Map.Entry entry = (Map.Entry) iter.next();
            map.put((String) entry.getValue(), (String) entry.getKey());
        }
        map = Collections.unmodifiableMap(map);
        mSchemaNamespaceMaps.put(schema, map);
        return map;
    }

    /**
     * Builds up global substitution group mapping.  The key is the QName
     * of the substitution group head. And the value is a set of QNames that
     * refer to the elements that can be the substitutes of the
     * substitution group head.
     * 
     * @param schema A Castor Schema instance
     * @return an substitution group map
     */
    private Map<QName, Set<QName>> getSubstGrpMap(Schema schema)
            throws SwareSchemaException {
        Map<QName, Set<QName>> substGrpMap =
            mSchemaSubstitutionGroupMaps.get(schema);
        if (substGrpMap != null) {
            return substGrpMap;
        }
        Map<QName, ElementDecl> eDeclMap = getAllElementDecls(schema);
        substGrpMap = new HashMap<QName, Set<QName>>();
        for (Map.Entry<QName, ElementDecl> entry: eDeclMap.entrySet()) {
            ElementDecl eDecl = entry.getValue();
            QName substGrpHead = getSubstGrpHead(eDecl);
            if (substGrpHead == null) {
                continue;
            }
            Set<QName> existingSet;
            while (true) {
                if (substGrpHead == null) {
                    break;
                }
                if ((existingSet = substGrpMap.get(substGrpHead)) == null) {
                    existingSet = new HashSet<QName>();
                    substGrpMap.put(substGrpHead, existingSet);
                }
                if (existingSet.contains(entry.getKey())) {
                    break;
                }
                existingSet.add(entry.getKey());
                ElementDecl headerDecl = eDeclMap.get(substGrpHead);
                substGrpHead = getSubstGrpHead(headerDecl);
            }
        }
        substGrpMap = Collections.unmodifiableMap(substGrpMap);
        mSchemaSubstitutionGroupMaps.put(schema, substGrpMap);
        return substGrpMap;
    }
    
    /**
     * Gets the substitution affiliation of an ElementDecl, returned as a
     * QName. 
     * 
     * @param eDecl an instance of Castor's ElementDecl
     * @return The QName of substitution affiliation
     */
    private QName getSubstGrpHead(ElementDecl eDecl) {
        Schema schema;
        if (eDecl.isReference()) {
            schema = eDecl.getReference().getSchema();
        } else {
            schema = eDecl.getSchema();
        }
        String header = eDecl.getSubstitutionGroup();
        if (header == null) {
            return null;
        }
        int pos = header.indexOf(':');
        String prefix = pos > 0 ? header.substring(0, pos) : "";
        String nsURI = schema.getNamespace(prefix);
        if (nsURI != null && nsURI.length() > 0) {
            return new QName(nsURI,
                    pos >= 0 ? header.substring(pos + 1) : header);
        } 
        return new QName(header);
    }
    
    /**
     * Gets all the global element declarations accessible in the schema.
     * 
     * @param schema the schema
     * @return a map of element declarations.
     */
    private Map<QName, ElementDecl> getAllElementDecls(Schema schema)
            throws SwareSchemaException {
        Set<Schema> schemaSet = new HashSet<Schema>();
        Map<QName, ElementDecl> eDeclMap = new HashMap<QName, ElementDecl>();
        getAllElementDecls(schema, eDeclMap, schemaSet);
        return eDeclMap;
    }
    
    /**
     * Gets all global element declarations in a schema, including those in
     * imported schemas.
     * 
     * @param schema the schema
     * @param eDeclMap a map of element declarations. Key is QName
     * @param schemaSet the set of already visited schemas
     */
    private void getAllElementDecls(Schema schema,
        Map<QName, ElementDecl> eDeclMap, Set<Schema> schemaSet)
            throws SwareSchemaException {
        if (schema == null) {
            return;
        }
        if (schemaSet.contains(schema)) {
            return;
        }

        schemaSet.add(schema);

        for (Enumeration e = schema.getElementDecls(); e.hasMoreElements();) {
            ElementDecl eDecl = (ElementDecl) e.nextElement();
            eDeclMap.put(getQualifiedName(eDecl), eDecl);
        }

        for (Enumeration e = schema.getImportedSchema();
                e.hasMoreElements();) {
            getAllElementDecls((Schema) e.nextElement(), eDeclMap, schemaSet);
        }
    }
}
