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
 * @(#)XBeanSwareTypeSystemImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;

import com.sun.xml.transform.sware.schema.ImplementationType;
import com.sun.xml.transform.sware.schema.SwareComplexType;
import com.sun.xml.transform.sware.schema.SwareElement;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType;
import com.sun.xml.transform.sware.schema.SwareSchema;
import com.sun.xml.transform.sware.schema.SwareSchemaException;
import com.sun.xml.transform.sware.schema.SwareType;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * Schema type system implemented using XmlBeans.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class XBeanSwareTypeSystemImpl implements SwareTypeSystem {
    
    private final Set<Schema> mSchemaSet = new HashSet<Schema>();
    private SchemaTypeLoader mSchemaTypeLoader =
        XmlBeans.getContextTypeLoader();
    private final Map<SchemaType, XBeanFlatComplexTypeImpl> mFlatComplexTypes;
    
    public XBeanSwareTypeSystemImpl() {
        mFlatComplexTypes =
            new HashMap<SchemaType, XBeanFlatComplexTypeImpl>();
    }
    
    public XBeanSwareTypeSystemImpl(Set<Schema> schemaSet)
    throws SwareSchemaException {
        this();
        if (!schemaSet.isEmpty()) {
            addSchemas(schemaSet.toArray(new Schema[0]));
        }
    }
    
    public XBeanSwareTypeSystemImpl(SchemaTypeLoader typeLoader) {
        this();
        mSchemaTypeLoader = typeLoader;
    }
    
    public void addSchema(SwareSchema swareSchema) throws SwareSchemaException {
        if (!swareSchema.getImplementationType().equals(
                ImplementationType.XMLBEANS)) {
            throw new IllegalArgumentException(
                    "The schema implementation type must be XMLBEANS, found: "
                    + swareSchema.getImplementationType());
        }
        Schema schema = (Schema) swareSchema.getOpaqueWrappedObject();
        if (mSchemaSet.contains(schema)) {
            return;
        }
        addSchemas(new Schema[]{schema});
    }

    public SwareElement findElement(QName name) throws SwareSchemaException {
        SchemaGlobalElement elemDecl = mSchemaTypeLoader.findElement(name);
        if (elemDecl != null) {
            return new XBeanSwareElementImpl(elemDecl);
        }
        return null;
    }

    public SwareType findType(QName name) throws SwareSchemaException {
        SchemaType xmlType = mSchemaTypeLoader.findType(name);
        if (xmlType != null) {
            if (xmlType.isSimpleType()) {
                return new XBeanSwareSimpleTypeImpl(xmlType);
            }
            return new XBeanSwareComplexTypeImpl(xmlType);
        }
        return null;
    }

    public SwareFlatComplexType getFlatComplexType(
            SwareComplexType swareComplexType) throws SwareSchemaException {
        if (!swareComplexType.getImplementationType().equals(
                ImplementationType.XMLBEANS)) {
            throw new IllegalArgumentException(
                    "The schema implementation type must be XMLBEANS, found: "
                    + swareComplexType.getImplementationType());
        }
        SchemaType complexType =
            (SchemaType) swareComplexType.getOpaqueWrappedObject();
        XBeanFlatComplexTypeImpl fct = mFlatComplexTypes.get(complexType); 
        if (fct != null) {
            return fct;
        }
        fct = new XBeanFlatComplexTypeImpl(this, complexType);
        mFlatComplexTypes.put(complexType, fct);
        return fct;
    }

    public Collection<QName> getSubstitutionSet(QName name)
            throws SwareSchemaException {
        SchemaGlobalElement elemDecl = mSchemaTypeLoader.findElement(name);
        if (elemDecl != null) {
            QName[] qNames = elemDecl.substitutionGroupMembers();
            return Arrays.asList(qNames);
        }
        return null;
    }

    public ImplementationType getImplementationType() {
        return ImplementationType.XMLBEANS;
    }
    
    public SchemaGlobalElement xbeanFindElement(QName elemName) {
        return mSchemaTypeLoader.findElement(elemName);
    }
    
    private synchronized void addSchemas(Schema[] schemas)
    throws SwareSchemaException {
        XmlOptions options = new XmlOptions();
        options.setCompileDownloadUrls();
        SchemaTypeSystem ts;
        try {
            ts = XmlBeans.compileXsd(schemas, mSchemaTypeLoader, options);
        } catch (XmlException e) {
            throw new SwareSchemaException("Compiling XSD failed.", e); 
        }
        mSchemaTypeLoader =
            XmlBeans.typeLoaderUnion(
                    new SchemaTypeLoader[]{mSchemaTypeLoader, ts});
        for (int i = 0; i < schemas.length; i++) {
            mSchemaSet.add(schemas[i]);
        }
    }
}
