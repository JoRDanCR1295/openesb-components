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
 * @(#)WSDLReaderExImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.impl;

import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.Import;
import javax.wsdl.Message;
import javax.wsdl.Types;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.extensions.schema.SchemaReference;
import javax.wsdl.factory.WSDLFactory;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlError;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.extensions.schema.SchemaConstants;
import com.ibm.wsdl.util.StringUtils;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;
import com.ibm.wsdl.util.xml.XPathUtils;
import com.ibm.wsdl.xml.WSDLReaderImpl;

import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.NamespaceDeclarations;
import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.PartnerLinkRole;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;
import com.sun.wsdl4j.ext.bpel.impl.MessagePropertyAliasImpl;
import com.sun.wsdl4j.ext.bpel.impl.MessagePropertyImpl;
import com.sun.wsdl4j.ext.bpel.impl.PartnerLinkRoleImpl;
import com.sun.wsdl4j.ext.bpel.impl.PartnerLinkTypeImpl;

/**
 * The class extends <code>com.ibm.wsdl.xml.WSDLReaderImpl</code> to
 * make WSDL reader be aware of an entity resolver during WSDL reading
 * process.  As such, a catalog entity resolver may be used to store
 * well known WSDLs.
 *  
 * @author Jun Xu
 * @version $Revision: 1.12 $
 */
public class WSDLReaderEx extends WSDLReaderImpl {

    protected EntityResolver _resolver;
    protected DeferredActionRegistry _deferredActionRegistry;
    protected Map<String, Definition> _wsdlCache;
    
    private static final Logger _logger =
        Logger.getLogger(WSDLReaderEx.class.getName());
    
    ////////////////////////////////////
    // New methods added in WSDLReaderEx
    ////////////////////////////////////
    
    /**
     * Sets the entity resolver for the WSDL reader to use.
     * 
     * @param resolver the entity resolver
     */
    public void setEntityResolver(EntityResolver resolver) {
        _resolver = resolver;
    }
    
    /**
     * Gets the entity resolver used by the WSDL reader.
     * 
     * @return The entity resolver used by the WSDL reader.  May be
     *         <code>null</code>.
     */
    public EntityResolver getEntityResolver() {
        return _resolver;
    }
    
    /**
     * Sets the deferred action registry.
     * 
     * @param registry The deferred action registry.
     */
    public void setDeferredActionRegistry(DeferredActionRegistry registry) {
        _deferredActionRegistry = registry;
    }
    
    /**
     * Gets the deferred action registry.
     * 
     * @return The deferred action registry.
     */
    public DeferredActionRegistry getDeferredActionRegistry() {
        return _deferredActionRegistry;
    }
    
    /**
     * Sets the cache to avoid loading same WSDL definition multiple times.
     * 
     * @param cache The cache used to store the WSDL definitions that are
     *              already loaded.
     */
    public void setWSDLCache(Map<String, Definition> cache) {
        _wsdlCache = cache;
    }
    
    /**
     * Gets the cache that is used to avoid loading same WSDL definition
     * multiple times.
     * 
     * @return the cache used to store WSDL definitions that have been loaded.
     */
    public Map<String, Definition> getWSDLCache() {
        return _wsdlCache;
    }
    
    /**
     * Gets the cache used to store all XSDs loaded so far.
     * 
     * @param cache The cache used to store all XSDs loaded so far
     */
    public void setXSDCache(Map<String, Schema> cache) {
        allSchemas = cache;
    }
    
    /**
     * Gets the cache used to store all XSDs loaded so far.
     * 
     * @return The cache used to store all XSDs loaded so far.
     */
    public Map<String, Schema> getXSDCache() {
        return allSchemas;
    }

    public void setFactory(WSDLFactory wsdlFactory) {
        factory = wsdlFactory;
    }
    
    /////////////////////
    // Overridden methods
    /////////////////////
    
    @Override
    protected Import parseImport(Element importEl, Definition def,
            Map importedDefs) throws WSDLException {
        Import importDef = def.createImport();

        /*---------------------------------------------*\\
        || The implementation of this method is copied  ||
        || from the super class with minor modification.||
        || If a newer version of WSDL4J is used, please ||
        || make sure to pull in any changes in the super||
        || class into this class.                       ||
        \*----------------------------------------------*/
        
        try
        {
          String namespaceURI = DOMUtils.getAttribute(importEl,
                                                      Constants.ATTR_NAMESPACE);
          String locationURI = DOMUtils.getAttribute(importEl,
                                                     Constants.ATTR_LOCATION);
          String contextURI = null;

          if (namespaceURI != null)
          {
            importDef.setNamespaceURI(namespaceURI);
          }

          if (locationURI != null)
          {
            importDef.setLocationURI(locationURI);

            if (importDocuments)
            {
              try
              {
                contextURI = def.getDocumentBaseURI();
                Definition importedDef = null;
                InputStream inputStream = null;
                InputSource inputSource = null;
                URL url = null;

                if (loc != null)
                {
                  inputSource = loc.getImportInputSource(contextURI, locationURI);

                  /*
                    We now have available the latest import URI. This might
                    differ from the locationURI so check the importedDefs for it
                    since it is this that we pass as the documentBaseURI later.
                  */
                  String liu = loc.getLatestImportURI();

                  importedDef = (Definition)importedDefs.get(liu);
                  
                  inputSource.setSystemId(liu);
                }
                else
                {
                  URL contextURL = (contextURI != null)
                                   ? StringUtils.getURL(null, contextURI)
                                   : null;

                  url = StringUtils.getURL(contextURL, locationURI);
                  
                  if (_resolver != null) {
                      inputSource = _resolver.resolveEntity(null, locationURI);
                      if (inputSource != null && inputSource.getSystemId() != null) {
                          url = contextURL.toURI().resolve(
                                  StringUtils.getURL(null,
                                          inputSource.getSystemId()).toURI()).toURL();
                      }
                  }
                  importedDef = (Definition)importedDefs.get(url.toString());

                  if (importedDef == null)
                  {
                    if (inputSource == null) {
                        inputStream = StringUtils.getContentAsInputStream(url);

                        if (inputStream != null)
                        {
                          inputSource = new InputSource(inputStream);
                          inputSource.setSystemId(url.toString());
                        }
                    }
                  }
                }

                if (importedDef == null)
                {
                  if (inputSource == null)
                  {
                    throw new WSDLException(WSDLException.OTHER_ERROR,
                                            "Unable to locate imported document " +
                                            "at '" + locationURI + "'" +
                                            (contextURI == null
                                             ? "."
                                             : ", relative to '" + contextURI +
                                             "'."));
                  }

                  Document doc = getDocument2(inputSource, inputSource.getSystemId());

                  if (inputStream != null)
                  {
                    inputStream.close();
                  }

                  Element documentElement = doc.getDocumentElement();

                  /*
                    Check if it's a wsdl document.
                    If it's not, don't retrieve and process it.
                    This should later be extended to allow other types of
                    documents to be retrieved and processed, such as schema
                    documents (".xsd"), etc...
                  */
                  if (QNameUtils.matches(Constants.Q_ELEM_DEFINITIONS,
                                         documentElement))
                  {
                    if (verbose)
                    {
                      System.out.println("Retrieving document at '" + locationURI +
                                         "'" +
                                         (contextURI == null
                                          ? "."
                                          : ", relative to '" + contextURI + "'."));
                    }

                    String urlString =
                      (loc != null)
                      ? loc.getLatestImportURI()
                      : (url != null)
                        ? url.toString()
                        : locationURI;

                    importedDef = readWSDL(urlString,
                                           documentElement,
                                           importedDefs);
                  }
                  else
                  {
                    QName docElementQName = QNameUtils.newQName(documentElement);

                    if (SchemaConstants.XSD_QNAME_LIST.contains(docElementQName))
                    {
                      if (verbose)
                      {
                        System.out.println("Retrieving schema wsdl:imported from '" + locationURI +
                                           "'" +
                                           (contextURI == null
                                            ? "."
                                            : ", relative to '" + contextURI + "'."));
                      }
                        
                      WSDLFactory factory = getWSDLFactory();

                      importedDef = factory.newDefinition();

                      if (extReg != null)
                      {
                        importedDef.setExtensionRegistry(extReg);
                      }

                      String urlString =
                        (loc != null)
                        ? loc.getLatestImportURI()
                        : (url != null)
                          ? url.toString()
                          : locationURI;

                      importedDef.setDocumentBaseURI(urlString);

                      Types types = importedDef.createTypes();
                      types.addExtensibilityElement(
                          parseSchema(Types.class, documentElement, importedDef));
                      importedDef.setTypes(types);
                      postParsingHandling(importedDef.getDocumentBaseURI(),
                              (DefinitionEx) importedDef);
                    }
                  }
                }

                if (importedDef != null)
                {
                  importDef.setDefinition(importedDef);
                }
              }
              catch (WSDLException e)
              {
               throw e;
              }
              catch (RuntimeException e)
              {
                throw e;
              }
              catch (Exception e)
              {
                throw new WSDLException(WSDLException.OTHER_ERROR,
                                        "Unable to resolve imported document at '" +
                                        locationURI + 
                                        (contextURI == null 
                                        ? "'." : "', relative to '" + contextURI + "'")
                                        , e);
              }
            } //end importDocs
          } //end locationURI
          
        }
        catch (WSDLException e)
        {
          if (e.getLocation() == null)
          {
            e.setLocation(XPathUtils.getXPathExprFromNode(importEl));
          }
          else
          {
            //If definitions are being parsed recursively for nested imports
            //the exception location must be built up recursively too so
            //prepend this element's xpath to exception location.
            String loc = XPathUtils.getXPathExprFromNode(importEl) + e.getLocation();
            e.setLocation(loc);
          }

          throw e; 
        }

        //register any NS decls with the Definition
        NamedNodeMap attrs = importEl.getAttributes();
        registerNSDeclarations2(attrs, def);

        Element tempEl = DOMUtils.getFirstChildElement(importEl);

        while (tempEl != null)
        {
          if (QNameUtils.matches(Constants.Q_ELEM_DOCUMENTATION, tempEl))
          {
            importDef.setDocumentationElement(tempEl);
          }
          else
          {
            importDef.addExtensibilityElement(
              parseExtensibilityElement(Import.class, tempEl, def));        
          }

          tempEl = DOMUtils.getNextSiblingElement(tempEl);
         }

        parseExtensibilityAttributes(importEl, Import.class, importDef, def);
        
        return importDef; 
        
    }

    @Override
    protected ExtensibilityElement parseSchema(Class parentType, Element el,
            Definition def, ExtensionRegistry extReg) throws WSDLException {
        
        /*---------------------------------------------*\\
        || The implementation of this method is copied  ||
        || from the super class with minor modification.||
        || If a newer version of WSDL4J is used, please ||
        || make sure to pull in any changes in the super||
        || class into this class.                       ||
        \*----------------------------------------------*/
        
        /*
         * This method returns ExtensibilityElement rather than Schema because we
         * do not insist that a suitable XSD schema deserializer is registered.
         * PopulatedExtensionRegistry registers SchemaDeserializer by default, but 
         * if the user chooses not to register a suitable deserializer then the
         * UnknownDeserializer will be used, returning an UnknownExtensibilityElement. 
         */
         
        Schema schema = null;
        SchemaReference schemaRef = null;
        try
        {

          QName elementType = QNameUtils.newQName(el);
          
          ExtensionDeserializer exDS = 
            extReg.queryDeserializer(parentType, elementType);
          
          //Now unmarshall the DOM element.
          ExtensibilityElement ee =  
            exDS.unmarshall(parentType, elementType, el, def, extReg);
          
          if (ee instanceof Schema)
          {
            schema = (Schema) ee;
          }
          else
          {
            //Unknown extensibility element, so don't do any more schema parsing on it.
            return ee;
          }


          //Keep track of parsed schemas to avoid duplicating Schema objects
          //through duplicate or circular references (eg: A imports B imports A).
          if (schema.getDocumentBaseURI() != null) 
          {
            this.allSchemas.put(schema.getDocumentBaseURI(), schema);
          }
              
          //At this point, any SchemaReference objects held by the schema will not 
          //yet point to their referenced schemas, so we must now retrieve these 
          //schemas and set the schema references.
              
          //First, combine the schema references for imports, includes and redefines 
          //into a single list
          
          ArrayList allSchemaRefs = new ArrayList();
        
          Collection ic = schema.getImports().values();
          Iterator importsIterator = ic.iterator();
          while(importsIterator.hasNext())
          {
            allSchemaRefs.addAll( (Collection) importsIterator.next() );
          }
        
          allSchemaRefs.addAll(schema.getIncludes());
          allSchemaRefs.addAll(schema.getRedefines());
              
          //Then, retrieve the schema referred to by each schema reference. If the 
          //schema has been read in previously, use the existing schema object. 
          //Otherwise unmarshall the DOM element into a new schema object.
              
          ListIterator schemaRefIterator = allSchemaRefs.listIterator();
              
          while(schemaRefIterator.hasNext()) 
          {
            try
            {
              schemaRef = (SchemaReference) schemaRefIterator.next();
                  
              if (schemaRef.getSchemaLocationURI() == null)
              {
                //cannot get the referenced schema, so ignore this schema reference
                continue;
              }
              
              if (verbose)
              {
                System.out.println("Retrieving schema at '" + 
                                   schemaRef.getSchemaLocationURI() +
                                  (schema.getDocumentBaseURI() == null
                                   ? "'."
                                   : "', relative to '" + 
                                   schema.getDocumentBaseURI() + "'."));
              }

                  
              InputStream inputStream = null;
              InputSource inputSource = null;
                  
              //This is the child schema referred to by the schemaReference
              Schema referencedSchema = null;
                  
              //This is the child schema's location obtained from the WSDLLocator or the URL
              String location = null;

              if (loc != null)
              {
                //Try to get the referenced schema using the wsdl locator
                inputSource = loc.getImportInputSource(
                  schema.getDocumentBaseURI(), schemaRef.getSchemaLocationURI());
            
                if (inputSource == null)
                {
                  throw new WSDLException(WSDLException.OTHER_ERROR,
                            "Unable to locate with a locator "
                            + "the schema referenced at '"
                            + schemaRef.getSchemaLocationURI() 
                            + "' relative to document base '"
                            + schema.getDocumentBaseURI() + "'");
                }
                location = loc.getLatestImportURI();
                    
                //if a schema from this location has been read previously, use it.
                referencedSchema = (Schema) this.allSchemas.get(location);
              }
              else
              {
                // We don't have a wsdl locator, so try to retrieve the schema by its URL
                String contextURI = schema.getDocumentBaseURI();
                URL contextURL = (contextURI != null) ? StringUtils.getURL(null, contextURI) : null;
                URL url = StringUtils.getURL(contextURL, schemaRef.getSchemaLocationURI());
                location = url.toExternalForm();
                        
                if (_resolver != null) {
                    inputSource = _resolver.resolveEntity(null, schemaRef.getSchemaLocationURI());
                    if (inputSource != null && inputSource.getSystemId() != null) {
                        url = contextURL.toURI().resolve(
                                StringUtils.getURL(null,
                                        inputSource.getSystemId()).toURI()).toURL();
                        location = url.toExternalForm();
                    }
                }
                
                //if a schema from this location has been retrieved previously, use it.
                referencedSchema = (Schema) this.allSchemas.get(location);

                if (referencedSchema == null)
                {
                    if (inputSource == null) {
                        inputStream = StringUtils.getContentAsInputStream(url);
                        if (inputStream != null) {
                            inputSource = new InputSource(inputStream);
                        }
                    }
                
                  if (inputSource == null)
                  {
                    throw new WSDLException(WSDLException.OTHER_ERROR,
                              "Unable to locate with a url "
                              + "the document referenced at '"
                              + schemaRef.getSchemaLocationURI()
                              + "'"
                              + (contextURI == null ? "." : ", relative to '"
                              + contextURI + "'."));
                  }
                }  
              } //end if loc
                  
              // If we have not previously read the schema, get its DOM element now.
              if (referencedSchema == null)
              {
                  if (inputSource.getSystemId() == null) {
                      inputSource.setSystemId(location);
                  }
                Document doc = getDocument2(inputSource, location);

                if (inputStream != null)
                {
                  inputStream.close();
                }

                Element documentElement = doc.getDocumentElement();

                // If it's a schema doc process it, otherwise the schema reference remains null

                QName docElementQName = QNameUtils.newQName(documentElement);

                if (SchemaConstants.XSD_QNAME_LIST.contains(docElementQName))
                {
                  //We now need to call parseSchema recursively to parse the referenced
                  //schema. The document base URI of the referenced schema will be set to 
                  //the document base URI of the current schema plus the schemaLocation in 
                  //the schemaRef. We cannot explicitly pass in a new document base URI
                  //to the schema deserializer, so instead we will create a dummy 
                  //Definition and set its documentBaseURI to the new document base URI. 
                  //We can leave the other definition fields empty because we know
                  //that the SchemaDeserializer.unmarshall method uses the definition 
                  //parameter only to get its documentBaseURI. If the unmarshall method
                  //implementation changes (ie: its use of definition changes) we may need 
                  //to rethink this approach.
                  
                  WSDLFactory factory = getWSDLFactory();
                  Definition dummyDef = factory.newDefinition();
                
                  dummyDef.setDocumentBaseURI(location);

                  //By this point, we know we have a SchemaDeserializer registered
                  //so we can safely cast the ExtensibilityElement to a Schema.
                  referencedSchema = (Schema) parseSchema( parentType, 
                                                           documentElement, 
                                                           dummyDef,
                                                           extReg);
                }
            
              } //end if referencedSchema

              schemaRef.setReferencedSchema(referencedSchema);      
            }
            catch (WSDLException e)
            {
              throw e;
            }
                catch (RuntimeException e)
                {
                  throw e;
                }
            catch (Exception e)
            {
                  throw new WSDLException(WSDLException.OTHER_ERROR,
                        "An error occurred trying to resolve schema referenced at '" 
                        + schemaRef.getSchemaLocationURI() 
                        + "'"
                        + (schema.getDocumentBaseURI() == null ? "." : ", relative to '"
                        + schema.getDocumentBaseURI() + "'."),
                        e);
            }
            
          } //end while loop

          return schema;

        }
        catch (WSDLException e)
        {
          if (e.getLocation() == null)
          {
            e.setLocation(XPathUtils.getXPathExprFromNode(el));
          }
          else
          {
            //If this method has been called recursively for nested schemas
            //the exception location must be built up recursively too so
            //prepend this element's xpath to exception location.
            String loc = XPathUtils.getXPathExprFromNode(el) + e.getLocation();
            e.setLocation(loc);
          }

          throw e; 
        }
        
    }

    @Override
    public Definition readWSDL(String documentBaseURI, InputSource inputSource)
            throws WSDLException {
        String location = (inputSource.getSystemId() != null ? 
                inputSource.getSystemId() : "- WSDL Document -");

        /*---------------------------------------------*\\
        || The implementation of this method is copied  ||
        || from the super class with minor modification.||
        || If a newer version of WSDL4J is used, please ||
        || make sure to pull in any changes in the super||
        || class into this class.                       ||
        \*----------------------------------------------*/
        
        return readWSDL(documentBaseURI,
                     getDocument2(inputSource, location));
    }

    @Override
    public Definition readWSDL(String contextURI, String wsdlURI)
            throws WSDLException {
        
        /*---------------------------------------------*\\
        || The implementation of this method is copied  ||
        || from the super class with minor modification.||
        || If a newer version of WSDL4J is used, please ||
        || make sure to pull in any changes in the super||
        || class into this class.                       ||
        \*----------------------------------------------*/
        
        try
        {
          if (verbose)
          {
            System.out.println("Retrieving document at '" + wsdlURI + "'" +
                               (contextURI == null
                                ? "."
                                : ", relative to '" + contextURI + "'."));
          }

          URL contextURL = (contextURI != null)
                           ? StringUtils.getURL(null, contextURI)
                           : null;
          URL url = StringUtils.getURL(contextURL, wsdlURI);
          InputStream inputStream = StringUtils.getContentAsInputStream(url);
          InputSource inputSource = new InputSource(inputStream);
          inputSource.setSystemId(url.toString());
          Document doc = getDocument2(inputSource, url.toString());

          inputStream.close();

          Definition def = readWSDL(url.toString(), doc);

          return def;
        }
        catch (WSDLException e)
        {
          throw e;
        }
        catch (RuntimeException e)
        {
          throw e;
        }
        catch (Exception e)
        {
          throw new WSDLException(WSDLException.OTHER_ERROR,
                                  "Unable to resolve imported document at '" +
                                  wsdlURI +
                                  (contextURI == null
                                  ? "'."
                                  : "', relative to '" + contextURI + "'.")
                                  , e);
        }
    }

    @Override
    public Definition readWSDL(String documentBaseURI,
            Element definitionsElement) throws WSDLException {
        return readWSDL(documentBaseURI, definitionsElement, _wsdlCache);
    }
    
    private Document getDocument2(InputSource inputSource,
            String desc) throws WSDLException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        /*----------------------------------------------*\\
        || The implementation of this method is copied   ||
        || from the super class's method getDocument(...)||
        || with minor modification. If a newer version of||
        || WSDL4J is used, please make sure to pull in   ||
        || any changes in the super class into this      ||
        || class.                                        ||
        \*-----------------------------------------------*/
        
        factory.setNamespaceAware(true);
        factory.setValidating(false);

        try
        {
          DocumentBuilder builder = factory.newDocumentBuilder();
          if (_resolver != null) {
              builder.setEntityResolver(_resolver);
          }
          Document doc = builder.parse(inputSource);

          return doc;
        }
        catch (RuntimeException e)
        {
          throw e;
        }
        catch (Exception e)
        {
          throw new WSDLException(WSDLException.PARSER_ERROR,
                                    "Problem parsing '" + desc + "'.",
                                    e);
        }
    }

    private static void registerNSDeclarations2(NamedNodeMap attrs, Definition def)
    {
        
        /*---------------------------------------------*\\
        || The implementation of this method is copied  ||
        || from the super class with minor modification.||
        || If a newer version of WSDL4J is used, please ||
        || make sure to pull in any changes in the super||
        || class into this class.                       ||
        \*----------------------------------------------*/
        
        int size = attrs.getLength();
        
        for (int i = 0; i < size; i++)
        {
            Attr attr = (Attr)attrs.item(i);
            String namespaceURI = attr.getNamespaceURI();
            String localPart = attr.getLocalName();
            String value = attr.getValue();
            
            if (namespaceURI != null && namespaceURI.equals(Constants.NS_URI_XMLNS))
            {
                if (localPart != null && !localPart.equals(Constants.ATTR_XMLNS))
                {
                    DOMUtils.registerUniquePrefix(localPart, value, def);
                }
                else
                {
                    DOMUtils.registerUniquePrefix(null, value, def);
                }
            }
        }
    }

    @Override
    protected Definition parseDefinitions(String documentBaseURI,
            Element defEl, Map importedDefs) throws WSDLException {
        
        if (importedDefs != null && importedDefs.containsKey(documentBaseURI)) {
            return (Definition) importedDefs.get(documentBaseURI);
        }
        
        DefinitionEx wsdlDef = (DefinitionEx)
            super.parseDefinitions(documentBaseURI, defEl, importedDefs);
        
        postParsingHandling(documentBaseURI, wsdlDef);
        
        return wsdlDef;
    }
    
    protected void postParsingHandling(String documentBaseURI,
            DefinitionEx wsdlDef) throws WSDLException {
        
        //Do some post-parsing handling on extensibility elements for
        //BPEL and XML schema.
        
        if (wsdlDef.getExtensibilityElements() != null) {
            Set<MessageProperty> properties =
                new LinkedHashSet<MessageProperty>();
            Set<MessagePropertyAlias> propAliases =
                new LinkedHashSet<MessagePropertyAlias>();
            Set<PartnerLinkType> plTypes =
                new LinkedHashSet<PartnerLinkType>();
        
            for (Object el : wsdlDef.getExtensibilityElements()) {
                if (el instanceof MessagePropertyImpl) {
                    properties.add((MessageProperty) el);
                } else if (el instanceof MessagePropertyAliasImpl) {
                    resolveReferences(wsdlDef, (MessagePropertyAliasImpl) el);
                    propAliases.add((MessagePropertyAlias) el);
                } else if (el instanceof PartnerLinkTypeImpl) {
                    resolveReferences(wsdlDef, (PartnerLinkTypeImpl) el);
                    plTypes.add((PartnerLinkType) el);
                }
            }
            if (!properties.isEmpty()) {
                wsdlDef.setMessageProperties(properties);
            }
            if (!propAliases.isEmpty()) {
                wsdlDef.setMessagePropertyAliases(propAliases);
            }
            if (!plTypes.isEmpty()) {
                wsdlDef.setPartnerLinkTypes(plTypes);
            }
        }
        
        XmlOptions options = new XmlOptions();
        if (_resolver != null) {
            options.setEntityResolver(_resolver);
        }
        options.setCompileDownloadUrls();
        options.setDocumentSourceName(documentBaseURI);
        options.setCompileNoUpaRule();
        List<XmlError> events = new ArrayList<XmlError>(); 
        options.setErrorListener(events);
        options.setLoadIgnoreDuplicates();
        
        List<SchemaDocument> xmlObjList = new ArrayList<SchemaDocument>();
        SchemaTypeSystem typeSystem = null;
        if (wsdlDef.getTypes() != null) {
            List<ExtensibilityElement> eeList =
                wsdlDef.getTypes().getExtensibilityElements();
            try {
                for (ExtensibilityElement ee : eeList) {
                    if (ee instanceof Schema) {
                        deriveAllNsPrefixes(((Schema) ee).getElement());
                        xmlObjList.add(
                                SchemaDocument.Factory.parse(
                                        ((Schema) ee).getElement(), options));
                    }
                }
                if (!xmlObjList.isEmpty()) {
                    if (_deferredActionRegistry != null) {
                        //lazy building schema type systems. Just collect
                        //all schema documents here.
                        for (SchemaDocument schemaDoc : xmlObjList) {
                            _deferredActionRegistry.
                                getSchemaDocumentCollector().add(
                                        schemaDoc.getSchema());
                        }
                    } else {
                        typeSystem =
                            XmlBeans.compileXsd(
                                    xmlObjList.toArray(new XmlObject[0]),
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
                    }
                }
            } catch (XmlException e) {
                throw new WSDLException(WSDLException.OTHER_ERROR,
                        "Compiling XML type system from the WSDL failed ("
                        + documentBaseURI + ").", e);
            }
        }
        if (typeSystem != null) {
            wsdlDef.setSchemaTypeSystem(typeSystem);
        }
        if (_deferredActionRegistry != null) {
            _deferredActionRegistry.getTypeLoaderHolderCollector().add(wsdlDef);
            _deferredActionRegistry.getActionAccepterCollector().add(wsdlDef);
        }
    }
    
    protected void resolveReferences(DefinitionEx wsdlDef,
            MessagePropertyAliasImpl propAlias) {
        QName messageTypeName = propAlias.getMessageTypeName();
        if (messageTypeName != null) {
            Message messageType = wsdlDef.getMessage(messageTypeName);
            propAlias.setMessageType(messageType);
            String partName = propAlias.getPartName();
            if (partName != null) {
                propAlias.setPart(messageType.getPart(partName));
            }
        }
    }
    
    protected void resolveReferences(DefinitionEx wsdlDef,
            PartnerLinkTypeImpl plType) {
        for (PartnerLinkRole role : plType.getRoles()) {
            PartnerLinkRoleImpl roleImpl = (PartnerLinkRoleImpl) role;
            if (roleImpl.getPortTypeName() != null) {
                roleImpl.setPortType(
                        wsdlDef.getPortType(roleImpl.getPortTypeName()));
            }
        }
    }
    
    /**
     * Move all namespace declarations from ancestor elements to the
     * element specified.  The method modifies the element directly.
     *   
     * @param elem The element to which all namespace declarations need to be
     *             placed.
     */
    protected void deriveAllNsPrefixes(Element elem) {
        if (!(elem.getParentNode() instanceof Element)) {
            //Nothing to derive
            return;
        }
        NamespaceDeclarations declarations =
            NamespaceDeclarationsImpl.newInstance(elem);
        if (declarations.isEmpty()) {
            //No declarations at all
            return;
        }
        elem.removeAttributeNS(
                NamespaceDeclarations.XMLNS_URI,
                NamespaceDeclarations.XMLNS_PREFIX);
        Map<String, String> prefixNsMap = declarations.getAll();
        for (Entry<String, String> entry : prefixNsMap.entrySet()) {
            if ("".equals(entry.getKey())) {
                elem.setAttributeNS(NamespaceDeclarations.XMLNS_URI,
                        NamespaceDeclarations.XMLNS_PREFIX, entry.getValue());
            } else {
                elem.setAttributeNS(NamespaceDeclarations.XMLNS_URI,
                        NamespaceDeclarations.XMLNS_PREFIX + ":" + entry.getKey(),
                        entry.getValue());
            }
        }
        return;
    }
}
