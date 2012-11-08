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
 * @(#)CocoEncoderProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;
import javax.xml.transform.URIResolver;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;

import com.sun.encoder.DataNature;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderProperties;
import com.sun.encoder.EncoderProvider;
import com.sun.encoder.EncoderType;
import com.sun.encoder.MetaRef;

/**
 * Implementation of encoder provider for COBOL Copybook.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version $Revision: 1.3 $
 */
public class CocoEncoderProvider implements EncoderProvider {

    public static final String STYLE_ID = "cocoencoder-1.0";
    private EncoderType mEncoderType;
    
    public String getIdentification() {
        return STYLE_ID;
    }
    
    /**
     * Instantiates a COBOL Copybook encoder.
     * 
     * @see com.sun.encoder.EncoderProvider#newEncoder(com.sun.encoder.MetaRef,
     *      javax.xml.transform.URIResolver)
     */
    public Encoder newEncoder(MetaRef metaRef, URIResolver uriResolver)
            throws EncoderConfigurationException {
        return newEncoder(metaRef, uriResolver, new EncoderProperties());
    }

    /**
     * Instantiates a collection of COBOL Copybook encoders.
     * 
     * @see com.sun.encoder.EncoderProvider#newEncoders(
     *      java.util.Set, javax.xml.transform.URIResolver)
     */
    public Map newEncoders(Set metaRefs, URIResolver uriResolver)
            throws EncoderConfigurationException {
        return newEncoders(metaRefs, uriResolver, new EncoderProperties());
    }

    public DataNature getDataNature() {
        return DataNature.BYTE_BASED;
    }

    /**
     * Instantiates a COBOL Copybook encoder with additional properties.
     * 
     * @see com.sun.encoder.EncoderProvider#newEncoder(com.sun.encoder.MetaRef,
     *      javax.xml.transform.URIResolver, com.sun.encoder.EncoderProperties)
     */
    public Encoder newEncoder(MetaRef metaRef, URIResolver uriResolver,
            EncoderProperties properties)
            throws EncoderConfigurationException {
        CocoEncoder encoder = new CocoEncoder(mEncoderType, properties);
        encoder.setMeta(metaRef);
        return encoder;
    }

    /**
     * Instantiates a collection of COBOL Copybook encoders with additional
     * encoder properties.
     * 
     * @see com.sun.encoder.EncoderProvider#newEncoders(
     *      java.util.Set, javax.xml.transform.URIResolver,
     *      com.sun.encoder.EncoderProperties)
     */
    public Map newEncoders(Set metaRefs, URIResolver uriResolver,
            EncoderProperties properties)
            throws EncoderConfigurationException {
        if (metaRefs == null || metaRefs.isEmpty()) {
            throw new EncoderConfigurationException("No metadata references.");
        }
        Set<String> pathSet = new HashSet<String>();
        Set<URL> urlSet = new HashSet<URL>();
        List<XmlObject> xmlObjectList = new ArrayList<XmlObject>();
        try {
            XmlOptions options = new XmlOptions();
            options.put(XmlOptions.COMPILE_DOWNLOAD_URLS, Boolean.TRUE);
            for (Iterator iter = metaRefs.iterator(); iter.hasNext();) {
                MetaRef metaRef = (MetaRef) iter.next();
                if (metaRef.getURL() != null) {
                    if (!urlSet.contains(metaRef.getURL())) {
                        xmlObjectList.add(
                                SchemaDocument.Factory.parse(
                                        metaRef.getURL(), options));
                        urlSet.add(metaRef.getURL());
                    }
                } else {
                    if (!pathSet.contains(metaRef.getPath())) {
                        xmlObjectList.add(
                                SchemaDocument.Factory.parse(
                                        new File(metaRef.getPath())));
                        pathSet.add(metaRef.getPath());
                    }
                }
            }
            XmlObject[] xmlObjects = xmlObjectList.toArray(new XmlObject[0]);
            SchemaTypeSystem schemaTS;
            if (urlSet.size() > 0) {
                schemaTS =
                    XmlBeans.compileXsd(xmlObjects,
                            XmlBeans.getContextTypeLoader(), options);
            } else {
                schemaTS =
                    XmlBeans.compileXsd(xmlObjects,
                            XmlBeans.getContextTypeLoader(), null);
            }
            SchemaTypeLoader typeLoader =
                XmlBeans.typeLoaderUnion(
                    new SchemaTypeLoader[]{XmlBeans.getContextTypeLoader(),
                            schemaTS});
            Map<MetaRef, Encoder> encoderMap = new HashMap<MetaRef, Encoder>();
            for (Iterator iter = metaRefs.iterator(); iter.hasNext();) {
                MetaRef metaRef = (MetaRef) iter.next();
                QName qName = metaRef.getRootElemName();
                if (qName == null) {
                    throw new EncoderConfigurationException(
                            "Root element name cannot be null. path='"
                            + metaRef.getPath() + "'");
                }
                SchemaGlobalElement element = typeLoader.findElement(
                        metaRef.getRootElemName());
                if (element == null) {
                    throw new EncoderConfigurationException(
                            "Unable to find global element '"
                            + metaRef.getRootElemName() + "'");
                }
                CocoEncoder encoder = new CocoEncoder(mEncoderType, properties);
                if (metaRef.getURL() != null) {
                    encoder.setMeta(metaRef.getURL(), element);
                } else {
                    encoder.setMeta(
                            new File(metaRef.getPath()).toURL(), element);
                }         
                encoderMap.put(metaRef, encoder);
            }
            return encoderMap;
        } catch (XmlException e) {
            throw new EncoderConfigurationException(e);
        } catch (IOException e) {
            throw new EncoderConfigurationException(e);
        }
    }

    /**
     * Sets the encoder type from the encoder framework.  Just save it and pass
     * it to all encoder instances created from this provider.
     */
    public void setType(EncoderType encoderType) {
        mEncoderType = encoderType;
    }

    /**
     * Gets the aliases of the encoder type.
     */
    public String[] getAliases() {
        //Not using aliases for now.  Should aliases be needed, just return
        //them in a string array here.
        return null;
    }
}
