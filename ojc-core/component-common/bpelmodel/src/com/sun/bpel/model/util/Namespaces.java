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
 * @(#)Namespaces.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.xml.XMLConstants;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.BPELProcess.ATTR;
import com.sun.bpel.model.common.MessageManager;
import com.sun.bpel.model.common.visitor.XMLParseVisitorException;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLElement.WellKnownAttr;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;


/**
 * Namespace utilities.
 *
 * @author Sun Microsystems
 * @version 
 */
public class Namespaces {
    
    /** Message key for INCORRECT_NAMESPACE */
    private static final String INCORRECT_NAMESPACE = "INCORRECT_NAMESPACE";  // Not I18N
    
    /** Message key for INCORRECT_NAMESPACE_EXTRA */
    private static final String INCORRECT_NAMESPACE_EXTRA = "INCORRECT_NAMESPACE_EXTRA";  // Not I18N
    
    /** Quick exit in SAX parser */
    private static final String FOUND = "***FOUND IT***";  // Not I18N
    
    /** Logger */
    private static final Logger mLogger = Logger.getLogger(Namespaces.class.getName());
    
    /** Message Manager */
    private static MessageManager mMsg = MessageManager.getManager(Namespaces.class);
    
    /** Holds the target namespace of a document */
    private String targetNamespace = null;
    
    /** Holds the default namespace for a document */
    private String defaultNamespace = null;
    
    /** Holds the map of other namespace declarations */
    private Map otherNamespaces = null;
    
    /** Holds the map of other attributes in the main document element */
    private Map otherAttributes = null;
    
    /** Holds flag of whether document is a BPEL */
    private boolean flagBPEL = false;
    
    /** Holds flag of whether document is a WSDL */
    private boolean flagWSDL = false;
    
    /** Holds flag of whether document is a XSD */
    private boolean flagXSD = false;
    
    /** Creates a new instance of Namespaces */
    public Namespaces() {
    }
    
    /** Gets the target namespace.
     * @return  Target namespace.
     */
    public String getTargetNamespace() {
        return targetNamespace;
    }
    
    /** Gets the default namespace.
     * @return  Default namespace.
     */
    public String getDefaultNamespace() {
        return defaultNamespace;
    }
    
    /** Gets the map of other namespaces declared.
     * @return  Map of other namespaces (key is the namespace prefix).  Can be
     *          <code>null</code> if none.
     */
    public Map getOtherNamespaces() {
        return otherNamespaces;
    }
    
    /** Gets the map of other attributes declared.
     * @return  Map of other attributes (key is attributes QName).  Can be
     *          <code>null</code> if none.
     */
    public Map getOtherAttributes() {
        return otherAttributes;
    }
    
    /** Tests if document is a BPEL.
     * @return  <code>true</code> if document is a BPEL.
     */
    public boolean isBPEL() {
        return flagBPEL;
    }
    
    /** Tests if document is a WSDL.
     * @return  <code>true</code> if document is a WSDL.
     */
    public boolean isWSDL() {
        return flagWSDL;
    }
    
    /** Tests if document is a XSD.
     * @return  <code>true</code> if document is a XSD.
     */
    public boolean isXSD() {
        return flagXSD;
    }
    
    /** Checks an element tag to see if it belongs to a certain namespace URI.
     * @param   ownerNS     Owner namespace.
     * @param   tag         Tag of the specific element.
     * @param   uri         Namespace URI.
     * @param   localName   Local name.
     * @param   qName       Qualified name.
     * @return  <code>true</code> if it is; <code>false</code> otherwise.
     */
    public static boolean checkNSElement(String ownerNS, String tag, String uri, String localName, String qName) {
//        boolean ownerHasEndSlash = ownerNS.endsWith("/");
        String effName = null;
        
        if ((null == qName) || (qName.trim().length() == 0)) {
            if ((null == localName) || (localName.trim().length() == 0)) {
                throw new XMLParseVisitorException("Both qualified and local name are missing!");
            }
            effName = localName;
        } else {
            effName = com.sun.bpel.xml.NamespaceUtility.getLocalName(qName);
        } 
        
        String effNS = uri;
        if ((null == uri) || (uri.trim().length() == 0)) {
            effNS = ownerNS;
        }
        boolean is = (ownerNS.equals(effNS) && effName.equals(tag));
        //RIT commented following logic namespace should always match 
        //no hack if slash is missing then assume they are same
        /*
        if (!is && effName.endsWith(tag)
                && ((ownerHasEndSlash && ownerNS.equals(effNS + "/"))
                    || (!ownerHasEndSlash && effNS.equals(ownerNS + "/")))) {
            mLogger.warning(mMsg.getString(
                (ownerHasEndSlash ? INCORRECT_NAMESPACE : INCORRECT_NAMESPACE_EXTRA),
                ownerNS));
            is = true;
        }
        */
        return is;
    }
    
    /** Tests if the element is the BPEL element specified.
     * @param   tag         Tag of the specific BPEL element.
     * @param   uri         Namespace URI.
     * @param   localName   Local name.
     * @param   qName       Qualified name.
     * @return  <code>true</code> if it is; <code>false</code> otherwise.
     */
    public static boolean isBPELElement(String tag, String uri, String localName, String qName) {
        return checkNSElement(BPELDocument.BPEL_NAMESPACE, tag, uri, localName, qName);
    }
    
    /**
     * tests if the element is Sbyn BPEL Extn Element.
     *
     * @param tag a <code>String</code> value
     * @param uri a <code>String</code> value
     * @param localName a <code>String</code> value
     * @param qName a <code>String</code> value
     * @return a <code>boolean</code> value
     */
    public static boolean isSbynBPELExtnElement(String tag, String uri, String localName, String qName) {
        return checkNSElement(XMLElement.SBYNBPEL_EXTN_NAMESPACE, tag, uri, localName, qName);
    }

    /**
     * tests if the element is Sbyn BPEL runtime Extn Element.
     *
     * @param tag a <code>String</code> value
     * @param uri a <code>String</code> value
     * @param localName a <code>String</code> value
     * @param qName a <code>String</code> value
     * @return a <code>boolean</code> value
     */
    public static boolean isSbynBPELRuntimeExtnElement(String tag, String uri, String localName, String qName) {
        return checkNSElement(XMLElement.SBYNBPEL_RUNTIME_EXTN_NAMESPACE, tag, uri, localName, qName);
    }
    
    /**
     * tests if the element is Sun BPEL Extn Trace Element.
     *
     * @param tag a <code>String</code> value
     * @param uri a <code>String</code> value
     * @param localName a <code>String</code> value
     * @param qName a <code>String</code> value
     * @return a <code>boolean</code> value
     */
    public static boolean isSunBPELExtnTraceElement(String tag, String uri, String localName, String qName) {
        return checkNSElement(BPELProcess.SUN_BPEL_EXTN_TRACE_NAMESPACE, tag, uri, localName, qName);
    }

    /**
     * tests if the element is Sun BPEL Extn Trace Element.
     *
     * @param tag a <code>String</code> value
     * @param uri a <code>String</code> value
     * @param localName a <code>String</code> value
     * @param qName a <code>String</code> value
     * @return a <code>boolean</code> value
     */
    public static boolean isSunBPELExtnDataHandler(String tag, String uri, String localName, String qName) {
        return checkNSElement(BPELProcess.SUN_BPEL_EXTN_DATA_HANDLING, tag, uri, localName, qName);
    }

    /** Tests if the element is the WSDL element specified.
     * @param   tag         Tag of the specific WSDL element.
     * @param   uri         Namespace URI.
     * @param   localName   Local name.
     * @param   qName       Qualified name.
     * @return  <code>true</code> if it is; <code>false</code> otherwise.
     */
    public static boolean isWSDLElement(String tag, String uri, String localName, String qName) {
        return checkNSElement(WSDLDocument.WSDL_NAMESPACE, tag, uri, localName, qName);
    }
    
    /** Tests if the element is the XML Schema element specified.
     * @param   tag         Tag of the specific WSDL element.
     * @param   uri         Namespace URI.
     * @param   localName   Local name.
     * @param   qName       Qualified name.
     * @return  <code>true</code> if it is; <code>false</code> otherwise.
     */
    public static boolean isXSDElement(String tag, String uri, String localName, String qName) {
        return checkNSElement(XMLSchema.URI, tag, uri, localName, qName);
    }
    
    /** Tests if the elment is the WSDL Service Link Type element specified.
     * @param   tag         Tag of the specific WSDL element.
     * @param   uri         Namespace URI.
     * @param   localName   Local name.
     * @param   qName       Qualified name.
     * @return  <code>true</code> if it is; <code>false</code> otherwise.
     */
    public static boolean isWSDLServiceLinkTypeElement(String tag, String uri, String localName, String qName) {
        return checkNSElement(WSDLDocument.WSDL_SLNK_NAMESPACE, tag, uri, localName, qName);
    }
    
    /** Tests if the element is the WSDL Service Reference element specified.
     * @param   tag         Tag of the specific WSDL element.
     * @param   uri         Namespace URI.
     * @param   localName   Local name.
     * @param   qName       Qualified name.
     * @return  <code>true</code> if it is; <code>false</code> otherwise.
     */
    public static boolean isWSDLServiceReferenceElement(String tag, String uri, String localName, String qName) {
        return checkNSElement(WSDLDocument.WSDL_SREF_NAMESPACE, tag, uri, localName, qName);
    }
    
    /** Ignore exceptions if any from SAX XML Reader feature sets.
     * @param   xmlReader   XML Reader
     * @param   feature     Feature to set
     * @param   boolVal     Boolean value to use
     */
    private static void setSAXXMLReaderFeature(XMLReader xr, String feature, boolean boolVal) {
        try {
            xr.setFeature(feature, boolVal);
        } catch (SAXException se) {
            mLogger.warning("SAX XML Reader feature \"" + feature + "\" not set to \"" + boolVal + "\"!");
        }
    }
    
    /** Getter for the XML reader.
     * @return  XML reader.
     */
    private static XMLReader getXmlReader() {
        XMLReader xmlReader = null;
        try {
            SAXParserFactory factory = SAXParserFactory.newInstance();
            SAXParser saxParser = factory.newSAXParser();
            xmlReader = saxParser.getXMLReader();

            // SAX2 core features
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/namespaces", true);
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/namespace-prefixes", true);
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/validation", false);
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/external-general-entities", false);
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/external-parameter-entities", false);
        } catch (Throwable trw) {
            throw new XMLParseVisitorException(
                "Cannot instantiate XML Reader", trw);
        }
        return xmlReader;
    }
    
    /** Implements <code>ContentHandler</code> for document parsing.
     */
    public class DocumentXmlParser extends DefaultHandler {
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if ((flagBPEL = isBPELElement(BPELProcess.TAG, uri, localName, qName))
                    || (flagWSDL = isWSDLElement("definitions", uri, localName, qName))
                    || (flagXSD = isXSDElement(XMLSchema.TAG, uri, localName, qName))) {
                for (int i = 0, n = attributes.getLength(); i < n; i++) {
                    String attrQName = attributes.getQName(i);
                    String val = attributes.getValue(i);
                    if (ATTR.TARGET_NAMESPACE.equals(attrQName)) {
                        targetNamespace = val;
                    } else if (WellKnownAttr.XMLNS.equals(attrQName)) {
                        defaultNamespace = val;
                    } else if (attrQName.startsWith(WellKnownAttr.XMLNS_COLON)) {
                        if (null == otherNamespaces) {
                            otherNamespaces = new HashMap();
                        }
                        String nsPrefix = com.sun.bpel.xml.NamespaceUtility.getLocalName(attrQName);
                        otherNamespaces.put(nsPrefix, val);
                    } else {
                        if (null == otherAttributes) {
                            otherAttributes = new HashMap();
                        }
                        otherAttributes.put(attrQName, val);
                    }
                }
                
                // Throw an exception to quickly end this
                throw new SAXException(FOUND);
            }
        }
    }
    
    /** Gets a XML Document parser.
     * @return  XML Document parser.
     */
    public ContentHandler getXmlParser() {
        return new DocumentXmlParser();
    }
    
    /** Finds the namespaces of a document.
     * @param   file    A file containing the document.
     * @return  Resulting <code>Namespaces</code> object or <code>null</code> if none found.
     * @throws  Exception   When there are problems.
     */
    public static Namespaces find(File file) throws Exception {
        return find(new BufferedReader(new InputStreamReader(new FileInputStream(file), "ISO-8859-1")));
    }
    
    /** Finds the namespaces of a document.
     * @param   url     A URL pointing to a location containing the document.
     * @return  Resulting <code>Namespaces</code> object or <code>null</code> if none found.
     * @throws  Exception   When there are problems.
     */
    public static Namespaces find(URL url) throws Exception {
        return find(new BufferedReader(new InputStreamReader(url.openStream(), "ISO-8859-1")));
    }
    
    /** Finds the namespaces of a document.
     * @param   reader      A <code>Reader</code> object to the document.
     * @return  Resulting <code>Namespaces</code> object or <code>null</code> if none found.
     */
    public static Namespaces find(Reader reader) {
        Namespaces nspaces = new Namespaces();
        try {
            XMLReader xmlReader = getXmlReader();
            xmlReader.setContentHandler(nspaces.getXmlParser());
            xmlReader.parse(new InputSource(reader));
            // If no "***FOUND IT***" exception thrown means none were found
            nspaces = null;
        } catch (Throwable trw) {
            String msg = trw.getMessage();
            if ((msg != null) && (msg.indexOf(FOUND) != -1)) {
                // Just the quick exit so return
            } else {
                nspaces = null;
                throw new XMLParseVisitorException(
                    "Cannot find namespaces", trw);
            }
        }
        return nspaces;
    }
}
