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
 * @(#)WSDL4JExt.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.WSDLElement;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlError;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;
import org.xml.sax.EntityResolver;

import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;
import com.sun.wsdl4j.ext.impl.DefinitionEx;
import com.sun.wsdl4j.ext.impl.WSDLFactoryEx;
import com.sun.wsdl4j.ext.impl.WSDLReaderEx;

/**
 * A utility class represents Sun's extension to WSDL4J.  This class hides all
 * the specifics of the extension and only exposes some simple methods with
 * almost zero learning curve to use.
 * 
 * To use the getXXX(...) methods provided in this class, one must use the
 * WSDL reader returned by this utility class to read a WSDL.  Otherwise, a
 * class cast exception will occur.
 *   
 * @author Jun Xu
 * @version $Revision: 1.6 $
 */
public class WSDL4JExt {
    
    private static WSDLFactoryEx _factory;
    
    static {
        _factory = new WSDLFactoryEx();
    }
    
    private static final Logger _logger =
        Logger.getLogger(WSDL4JExt.class.getName());
    
    /**
     * Gets a BPEL message property from a WSDL definition. The lookup includes
     * imported WSDLs (deep lookup).
     *  
     * @param wsdlDef The WSDL definition
     * @param name The QName of the message property
     * @return The message property
     */
    public static MessageProperty getMessageProperty(
            Definition wsdlDef, QName name) {
        return getDefintionEx(wsdlDef).getMessageProperty(name, true);
    }

    /**
     * Gets all BPEL message properties from a WSDL definition including
     * its imported ones.
     *  
     * @param wsdlDef The WSDL definition
     * @return All message properties
     */
    public static Collection<MessageProperty> getMessageProperties(
            Definition wsdlDef) {
        return getDefintionEx(wsdlDef).getMessageProperties(true);
    }

    /**
     * Gets a collection of message property aliases that have same message
     * property name from a WSDL definition including its imported ones.
     * 
     * @param wsdlDef The WSDL definition
     * @param name The QName of the message property
     * @return A collection of message property aliases. Might be empty, but
     *          never <code>null</code>
     */
    public static Collection<MessagePropertyAlias> getMessagePropertyAliases(
            Definition wsdlDef, QName name) {
        return getDefintionEx(wsdlDef).getMessagePropertyAliases(name, true);
    }

    /**
     * Gets all message property aliases a WSDL definition including its
     * imported ones.
     * 
     * @param wsdlDef The WSDL definition
     * @return All message property aliases. Might be empty, but
     *          never <code>null</code>
     */
    public static Collection<MessagePropertyAlias> getMessagePropertyAliases(
            Definition wsdlDef) {
        return getDefintionEx(wsdlDef).getMessagePropertyAliases(true);
    }

    /**
     * Gets a partner link type from a WSDL definition including its imported
     * ones.
     * 
     * @param wsdlDef The WSDL definition
     * @param name The QName of the partner link type
     * @return The partner link type
     */
    public static PartnerLinkType getPartnerLinkType(
            Definition wsdlDef, QName name) {
        return getDefintionEx(wsdlDef).getPartnerLinkType(name, true);
    }

    /**
     * Gets all partner link types from a WSDL definition including its
     * imported ones.
     * 
     * @param wsdlDef The WSDL definition
     * @return All partner link types
     */
    public static Collection<PartnerLinkType> getPartnerLinkTypes(
            Definition wsdlDef) {
        return getDefintionEx(wsdlDef).getPartnerLinkTypes(true);
    }
    
    /**
     * Gets a schema type loader, which can be used to look up XML schema
     * components reachable from a WSDL definition.
     * 
     * @param wsdlElem the WSDL element
     * @return The schema type loader created from the WSDL definition that
     *         this WSDL element belongs to.
     */
    public static SchemaTypeLoader getSchemaTypeLoader(
            WSDLElement wsdlElem) {
        return getDefintionEx(wsdlElem).getSchemaTypeLoader();
    }
    
    /**
     * Gets a WSDL reader. The reader will use an entity resolver (if any),
     * a deferred action registry (if any) and a cache (if any).
     * 
     * @param resolver The entity resolver. Might be <code>null</code>
     * @param deferredActionRegistry The registry that collects all referenced
     *            XML schema documents while loading WSDLs using this reader.
     *            Might be <code>null</code> then no lazy resolution on XSDs,
     *            which means schema type system will be created for each
     *            WSDL definition once the definition is loaded.
     * @param wsdlCache The cache used to avoid loading WSDL definition multiple
     *              times
     * @param xsdCache The cache used to avoid loading XSD multiple times
     * @return A WSDL reader
     */
    public static WSDLReader newWSDLReader(EntityResolver resolver,
            DeferredActionRegistry deferredActionRegistry,
            Map<String, Definition> wsdlCache,
            Map<String, javax.wsdl.extensions.schema.Schema> xsdCache) {
        WSDLReaderEx reader = _factory.newWSDLReaderEx();
        reader.setEntityResolver(resolver);
        reader.setDeferredActionRegistry(deferredActionRegistry);
        reader.setWSDLCache(wsdlCache);
        if (xsdCache != null) {
            reader.setXSDCache(xsdCache);
        }
        return reader;
    }
    
    /**
     * Gets a WSDL reader. The reader will use the entity resolver (if any).
     * 
     * @param resolver The entity resolver. Might be <code>null</code>
     * @param deferredActionRegistry The registry that collects all referenced
     *            XML schema documents while loading WSDLs using this reader.
     *            Might be <code>null</code> then no lazy resolution on XSDs,
     *            which means schema type system will be created for each
     *            WSDL definition once the definition is loaded.
     * @return A WSDL reader
     */
    public static WSDLReader newWSDLReader(EntityResolver resolver,
            DeferredActionRegistry deferredActionRegistry) {
        return newWSDLReader(resolver, deferredActionRegistry, null, null);
    }
    
    /**
     * Gets a WSDL reader. The reader will use an entity resolver (if any).
     * 
     * @param resolver The entity resolver. Might be <code>null</code>
     * @return A WSDL reader
     */
    public static WSDLReader newWSDLReader(EntityResolver resolver) {
        return newWSDLReader(resolver, null);
    }
    
    /**
     * Creates a new instance of WSDL definition.  Please note the document
     * base URI is <code>null</code> for the returned instance.
     * 
     * @return A new WSDL definition instance.
     */
    public static Definition newDefinition() {
        return new DefinitionEx();
    }
    
    /**
     * Builds a schema type loader out of the schema documents supplied.
     * 
     * @param schemaCollection The schema document collection
     * @return The schema type loader
     * @throws XmlException Any problem encountered when compiling the schema
     *      type system.
     */
    public static SchemaTypeLoader buildSchemaTypeLoader(
            Collection<Schema> schemaCollection,
            EntityResolver resolver) throws XmlException {
        XmlOptions options = new XmlOptions();
        if (resolver != null) {
            options.setEntityResolver(resolver);
        }
        options.setCompileDownloadUrls();
        options.setCompileNoUpaRule();
        List<XmlError> events = new ArrayList<XmlError>(); 
        options.setErrorListener(events);
        options.setLoadIgnoreDuplicates();
        SchemaTypeSystem typeSystem =
            XmlBeans.compileXsd(
                    schemaCollection.toArray(new Schema[0]),
                    XmlBeans.getContextTypeLoader(), options);
        for (XmlError xe : events) {
            if (xe.getSeverity() == XmlError.SEVERITY_ERROR) {
                throw new XmlException(xe);
            }
            if (xe.getSeverity() == XmlError.SEVERITY_WARNING) {
                if (_logger.isLoggable(Level.WARNING)) {
                    _logger.log(Level.WARNING, xe.toString());
                }
            }
        }
        SchemaTypeLoader[] typeLoaders = new SchemaTypeLoader[2];
        typeLoaders[0] = XmlBeans.getContextTypeLoader();
        typeLoaders[1] = typeSystem;
        return XmlBeans.typeLoaderUnion(typeLoaders);
    }
    
    public static void applySingleSchemaTypeLoader(DeferredActionRegistry registry,
            EntityResolver entityResolver) throws XmlException {
        SchemaTypeLoader typeLoader =
            buildSchemaTypeLoader(registry.getSchemaDocumentCollector(),
                    entityResolver);
        for (SchemaTypeLoaderHolder h : registry.getTypeLoaderHolderCollector()) {
            h.setSchemaTypeLoader(typeLoader);
        }
        for (DeferredActionAccepter d : registry.getActionAccepterCollector()) {
            d.performDeferredAction();
        }
    }
    
    private static DefinitionEx getDefintionEx(WSDLElement wsdlElem) {
        if (!(wsdlElem instanceof WSDLElementEx)) {
            throw new ClassCastException(
                    "The WSDL element instance is not of required type. found='"
                    + wsdlElem.getClass().getName() + ", expected='"
                    + WSDLElementEx.class.getName() + "'");
        }
        Definition wsdlDef = ((WSDLElementEx) wsdlElem).getContainingDefinition();
        if (!(wsdlDef instanceof DefinitionEx)) {
            throw new ClassCastException(
                    "The WSDL definition instance is not of required type. "
                    + "found='" + wsdlDef.getClass().getName()
                    + "', expected='" + DefinitionEx.class.getName() + "'");
        }
        return (DefinitionEx) wsdlDef;
    }
}
