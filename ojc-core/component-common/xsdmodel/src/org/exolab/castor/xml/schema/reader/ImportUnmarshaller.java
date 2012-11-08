/**
 * Redistribution and use of this software and associated documentation
 * ("Software"), with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * 1. Redistributions of source code must retain copyright
 *    statements and notices.  Redistributions must also contain a
 *    copy of this document.
 *
 * 2. Redistributions in binary form must reproduce the
 *    above copyright notice, this list of conditions and the
 *    following disclaimer in the documentation and/or other
 *    materials provided with the distribution.
 *
 * 3. The name "Exolab" must not be used to endorse or promote
 *    products derived from this Software without prior written
 *    permission of Intalio, Inc.  For written permission,
 *    please contact info@exolab.org.
 *
 * 4. Products derived from this Software may not be called "Exolab"
 *    nor may "Exolab" appear in their names without prior written
 *    permission of Intalio, Inc. Exolab is a registered
 *    trademark of Intalio, Inc.
 *
 * 5. Due credit should be given to the Exolab Project
 *    (http://www.exolab.org/).
 *
 * THIS SOFTWARE IS PROVIDED BY INTALIO, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 * INTALIO, INC. OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Copyright 1999-2002 (C) Intalio, Inc. All Rights Reserved.
 *
 * 
 */

package org.exolab.castor.xml.schema.reader;

import java.beans.PropertyChangeEvent;
//-- imported classes and packages
import org.exolab.castor.net.*;
import org.exolab.castor.xml.*;
import org.exolab.castor.xml.schema.*;
import org.exolab.castor.util.Configuration;
import org.xml.sax.*;

import java.net.URL;
import java.util.Vector;

public class ImportUnmarshaller extends ComponentReader {
    
    /** SeeBeyond extension.
     * Hold the <code>SchemaUnmarshaller</code> parent.
     */
    private SchemaUnmarshaller mSchemaUnmarshaller = null;
    
    public ImportUnmarshaller(Schema schema, AttributeSet atts, Resolver resolver, URIResolver uriResolver,
                              Locator locator, SchemaUnmarshallerState state, SchemaUnmarshaller schUnmarshaller)
            throws XMLException {
        super();
        setResolver(resolver);
        setURIResolver(uriResolver);
        setSchemaUnmarshaller(schUnmarshaller);
        
        URILocation uri = null;
        
        // SeeBeyond fix: need to close URILocation reader
        java.io.Reader uriReader = null;
        try {
        
        //-- Get schemaLocation
        String schemaLocation = atts.getValue(SchemaNames.SCHEMALOCATION_ATTR);
        //-- Get namespace
        String namespace = atts.getValue("namespace");
        
        if ((schemaLocation == null) && (namespace == null)) {
            //-- A legal <import/> element...just return
            return;
        }
        
        // SeeBeyond extension.
        // Convert any schemaLocations in the MSWindows format to the proper
        // URI format.  This is to support customers who refuse to change
        // their XSD files to use a URI compliant format.  Refer to Bug1416.
        schemaLocation = winToURIFormat(schemaLocation);
        
        if (schemaLocation != null) {
            if (schemaLocation.indexOf("\\") != -1) {
                String err = "'" + schemaLocation
                             + "' is not a valid URI as defined by IETF RFC 2396."
                             + "  The URI mustn't contain '\\'.";
                throw new SchemaException(err);
            }
            
            if (namespace == null) {
                namespace = "";
            }
            
            try {
                String documentBase = locator.getSystemId();
                //RIT this logic is wrong if for ex doc base is
                //http://soap.shared.xconnect.trx.com
                //then it will set 	documentBase to http:// which is wrong
                
//                if (documentBase != null) {
//                    if (!documentBase.endsWith("/")) {
//                        documentBase = documentBase.substring(0, documentBase.lastIndexOf('/') + 1 );
//                    }
//                }
                
                uri = getURIResolver().resolve(schemaLocation, documentBase);
                if (uri != null) {
                    schemaLocation = uri.getAbsoluteURI();
                }
            } catch (URIException urix) {
                throw new XMLException(urix);
            }
        } else {
            schemaLocation = namespace;
            try {
                uri = getURIResolver().resolveURN(namespace);
            } catch (URIException urix) {
                throw new XMLException(urix);
            }

            if (uri == null) {
                String err = "Unable to resolve Schema corresponding to namespace '"
                             + namespace + "'.";
                throw new SchemaException(err);
            }
        }
        
        //-- Is this namespace one the schema knows about?
        if (!schema.isKnownNamespace(namespace)) {
            throw new SchemaException("namespace '" + namespace + "' not declared in schema");
        }
        if (namespace.equals(schema.getTargetNamespace()) ) {
            throw new SchemaException("the 'namespace' attribute in the <import> element cannot be the"
                                      + " same of the targetNamespace of the global schema");
        }

        //-- Schema object to hold import schema
        Schema importedSchema = schema.getDirectlyImportedSchema(namespace);
        
        //-- Have we already imported this XML Schema file?
        if (state.processed(schemaLocation)) {
            if (importedSchema == null) {
                // SeeBeyond fix: set importedSchema variable
                importedSchema = state.getSchema(schemaLocation);
                schema.addImportedSchema(importedSchema);
            }
            
            // SeeBeyond extension: alert URI resolver so schema can be persisted in repository
            firePropertyChange(new PropertyChangeEvent(
                new Object[] {null, null, atts.getValue(SchemaNames.NAMESPACE),
                              atts.getValue(SchemaNames.SCHEMALOCATION_ATTR)},
                "SCHEMA_ALREADY_IMPORTED", uri, importedSchema));
            
            return;
        }

        // SeeBeyond Fix:  don't let it proceed any further
        if (uri == null) {
            String err = "Unable to resolve Schema corresponding to";
            if (namespace != null) {
                err += (" namespace '" + namespace + "'");
            }
            if (schemaLocation != null) {
                err += (" schemaLocation '" + schemaLocation + "'");
            }
            throw new SchemaException(err);
        }

        boolean alreadyLoaded = false;
        boolean addSchema = false;
        if (importedSchema == null) {
            if (uri instanceof SchemaLocation) {
                importedSchema = ((SchemaLocation) uri).getSchema();
                schema.addImportedSchema(importedSchema);
                alreadyLoaded = true;
            
                // SeeBeyond extension: alert URI resolver so schema can be persisted in repository
                firePropertyChange(new PropertyChangeEvent(
                    new Object[] {null, null, atts.getValue(SchemaNames.NAMESPACE),
                                  atts.getValue(SchemaNames.SCHEMALOCATION_ATTR)},
                    "SCHEMA_ALREADY_LOADED", uri, importedSchema));
            } else {
                importedSchema = new Schema();
                addSchema = true;
            }
        }
        
        state.markAsProcessed(schemaLocation, importedSchema);
        
        if (alreadyLoaded) {
            return;
        }
        
        //-- Parser Schema
        Parser parser = null;
        try {
            parser = state.getConfiguration().getParser();
        } catch (RuntimeException rte) {
            // ignore
        }
        
        if (parser == null) {
            throw new SchemaException("Error failed to create parser for import");
        } else {
            //-- Create Schema object and setup unmarshaller
            SchemaUnmarshaller schemaUnmarshaller = new SchemaUnmarshaller(state);
            schemaUnmarshaller.setURIResolver(getURIResolver());
            schemaUnmarshaller.setSchema(importedSchema);
            
            // SeeBeyond extension: chain the SchemaUnmarshaller back to the SchemaReader
            if (getSchemaUnmarshaller() != null) {
                schemaUnmarshaller.setSchemaReader(getSchemaUnmarshaller().getSchemaReader());
            }
            
            Sax2ComponentReader handler = new Sax2ComponentReader(schemaUnmarshaller);
            parser.setDocumentHandler(handler);
            parser.setErrorHandler(handler);
        }
        
        try {
            // SeeBeyond fix: capture the URI reader so can close later if necessary
            uriReader = uri.getReader();
            
            InputSource source = new InputSource(uriReader);
            source.setSystemId(uri.getAbsoluteURI());
            
            // SeeBeyond extension: alert URI resolver that a new XML Schema import is started
            if (addSchema) {
                firePropertyChange(new PropertyChangeEvent(
                    new Object[] {null, null, atts.getValue(SchemaNames.NAMESPACE),
                                  atts.getValue(SchemaNames.SCHEMALOCATION_ATTR)},
                    "SCHEMA_IMPORT_STARTED", uri, importedSchema));
            }
                              
            parser.parse(source);
        } catch (java.io.IOException ioe) {
            
            // SeeBeyond extension: alert URI resolver that a new XML Schema import is started
            if (addSchema) {
                firePropertyChange(new PropertyChangeEvent(
                    new Object[] {null, null, atts.getValue(SchemaNames.NAMESPACE),
                                  atts.getValue(SchemaNames.SCHEMALOCATION_ATTR)},
                    "SCHEMA_IMPORT_ABORTED", uri, importedSchema));
            }
            throw new SchemaException("Error reading import file '" + schemaLocation+"': " + ioe);
        } catch (org.xml.sax.SAXException sx) {
            
            // SeeBeyond extension: alert URI resolver that a new XML Schema import is started
            if (addSchema) {
                firePropertyChange(new PropertyChangeEvent(
                    new Object[] {null, null, atts.getValue(SchemaNames.NAMESPACE),
                                  atts.getValue(SchemaNames.SCHEMALOCATION_ATTR)},
                    "SCHEMA_IMPORT_ABORTED", uri, importedSchema));
            }
            throw new SchemaException(sx);
        }
        
        //-- Add schema to list of imported schemas (if not already present)
        if (addSchema) {
            
            // SeeBeyond fix: only set schemaLocation again if it has not been yet, i.e. null.  This causes
            // instability in the FileURIResolverImpl.propertyChange() code which handles imports of XSDs.
            // SeeBeyond has already changed the location to a PXUID but Castor keeps changing it back thus
            // make previous Private Extension Map entries for the import incorrect because later on, another
            // PXUID will be assigned to this imported schema and when all are serialized at the end, the PXUID
            // will not match any of the Private Extension Map entries of XSD Repository objects that did the
            // imported prior to this here "resetting" of the schemaLocation.
            //
            if (importedSchema.getSchemaLocation() == null) {
                importedSchema.setSchemaLocation(schemaLocation);
            }
            schema.addImportedSchema(importedSchema);
            
            // SeeBeyond extension: alert URI resolver so schema can be persisted in repository
            firePropertyChange(new PropertyChangeEvent(
                new Object[] {null, null, atts.getValue(SchemaNames.NAMESPACE),
                              atts.getValue(SchemaNames.SCHEMALOCATION_ATTR)},
                "SCHEMA_ADDED", uri, importedSchema));
        }
        
        // SeeBeyond fix: close unused/used Readers; otherwise there might be too many opened descriptors
        //                before VM garbage collects
        } finally {
            // Close the URILocation reader if necessary
            try {
                if (uriReader != null) {
                    uriReader.close();
                } else if (uri != null) {
                    uriReader = uri.getReader();
                    if (uriReader != null) {
                        uriReader.close();
                    }
                }
            } catch (java.io.IOException ioe) {
                // Ignore
            }
        }
    }
    
    
    /**
     * Sets the name of the element that this UnknownUnmarshaller handles
     * @param name the name of the element that this unmarshaller handles
     **/
    public String elementName() {
        return SchemaNames.IMPORT;
    } //-- elementName
    
    /**
     * Returns the Object created by this ComponentReader
     * @return the Object created by this ComponentReader
     **/
    public Object getObject() {
        return null;
    } //-- getObject
    
    /** SeeBeyond extension.
     * Gets the <code>SchemaUnmarshaller</code> parent.
     * @return  <code>SchemaUnmarshaller</code> parent.
     */
    public SchemaUnmarshaller getSchemaUnmarshaller() {
        return mSchemaUnmarshaller;
    }
    
    /** SeeBeyond extension.
     * Sets the <code>SchemaUnmarshaller</code> parent.
     * @param   schemaUnmarshaller  <code>SchemaUnmarshaller</code> parent.
     */
    public void setSchemaUnmarshaller(SchemaUnmarshaller schemaUnmarshaller) {
        mSchemaUnmarshaller = schemaUnmarshaller;
    }
    
    /** SeeBeyond extension.
     * Fires the property change to the registered listeners.
     * @param   event   Property change event.
     */
    public void firePropertyChange(PropertyChangeEvent event) {
        if ((getSchemaUnmarshaller() != null)
                && (getSchemaUnmarshaller().getSchemaReader() != null)) {
            getSchemaUnmarshaller().getSchemaReader().firePropertyChange(event);
        }
    }
}
