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
 * @(#)XMLSchemaImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.xsd.impl;


import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.reader.SchemaReader;
import org.exolab.castor.xml.schema.writer.SchemaWriter;
import org.xml.sax.InputSource;

import com.sun.wsdl.model.common.model.EInsightModelException;
import com.sun.wsdl.model.common.model.PrivateExtensionMapModel;
import com.sun.wsdl.model.common.model.impl.PrivateExtensionMapModelImpl;
import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.uri.impl.FileURIResolverImpl;
import com.sun.wsdl.model.xsd.XMLSchema;
import com.sun.jbi.internationalization.Messages;


/**
 * Models an XML Schema. Click <a href="http://www.w3.org/XML/Schema">here</a>
 * for more information.
 * Actually, this just provides a wrapper to an existing XML
 * Schema model. This may be expanded later to provide an API
 * into the actual model implementation. As it stands right now it's just
 * a placeholder.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class XMLSchemaImpl extends XMLSchema {
    
    private static final Messages MESSAGES = 
            Messages.getMessages(XMLSchemaImpl.class);
    private static final Logger LOGGER = 
            Messages.getLogger(XMLSchemaImpl.class);
    
    /** The schema. */
    private Object mSchema = null;
    
    /** The associate XSD repository object. */

    
    /** Holds reference to the creating EInsightManager. */

    
    /** Holds XMLSchema / Schema correlation map */
    protected Map mXMLSchemaSchemaMap = null;
    
    /** Holds Private Extension Map Model.
     * @since   5.1.0
     */
    protected PrivateExtensionMapModel mPrivateExtMapModel = null;
    
    /** Constructs a XMLSchema. */
    public XMLSchemaImpl() {
        super();
    }
    
    /** @see com.sun.wsdl.model.common.repository.EInsightManagerCreatable#initializeEInsightManager(
     *  com.sun.wsdl.model.common.repository.EInsightManager)
     */

    /** @see com.sun.wsdl.model.common.model.xsd.XMLSchema#getSchema()
     */
    public Object getSchema() {
        return mSchema;
    }
    
    /** @see com.sun.wsdl.model.common.model.xsd.XMLSchema#setSchema(java.lang.Object)
     */
    public void setSchema(Object schema) {
        mSchema = schema;
    }
        
    /** @see com.sun.wsdl.model.common.model.xsd.XMLSchema#getXSDRepositoryObject()
     */
    
    
    
    
//    /** @see com.sun.wsdl.model.common.model.xsd.XMLSchema#load(java.io.Reader)
//     */
//    public void load(Reader rdr) {
//        try {
//            InputSource inSrc = new InputSource(rdr);
//            inSrc.setSystemId("/no_xsd_import/supported/");
//            SchemaReader schRdr = new SchemaReader(inSrc);
//            BaseURIResolver resolver = new FileURIResolverImpl();
//            schRdr.setURIResolver(resolver);
//            schRdr.addPropertyChangeListener(resolver);
//
//            setSchema(schRdr.read());
//        } catch (Exception e) {
//            throw new EInsightModelException("Cannot load XML Schema from Reader", e);
//        } finally {
//            if (rdr != null) {
//                try {
//                    rdr.close();
//                } catch (IOException ioe) {
//                    // ignore
//                }
//            }
//        }
//    }
    
    /** @see com.sun.wsdl.model.common.model.xsd.XMLSchema#serialize(java.io.Writer)
     */
    public void serialize(Writer writer) {
        if (getSchema() != null) {
            try {
                SchemaWriter schWr = new SchemaWriter(writer);
                schWr.write((Schema) getSchema());
            } catch (Exception e) {
                throw new EInsightModelException(
                        MESSAGES.getString("XMLSchemaImpl.CANT_SERIALIZE_XML_SCHEMA"), e);
            }
        }
    }
    
    /** @see com.sun.wsdl.model.common.model.xsd.XMLSchema#getAllOwnedRepositoryObjects()
     */
    
    /** @see com.sun.wsdl.model.common.model.xsd.XMLSchema#getXMLSchemaSchemaMap()
     */
    public Map getXMLSchemaSchemaMap() {
        if (null == mXMLSchemaSchemaMap) {
            mXMLSchemaSchemaMap = new HashMap();
        }
        return mXMLSchemaSchemaMap;
    }

    /** @see com.sun.wsdl.model.common.model.xsd.XMLSchema#getPrivateExtensionMapModel()
     */
    public PrivateExtensionMapModel getPrivateExtensionMapModel() {
        if (null == mPrivateExtMapModel) {
            // See if present document is associated with a repository object
            mPrivateExtMapModel = new PrivateExtensionMapModelImpl();
        }
        return mPrivateExtMapModel;
    }
}
