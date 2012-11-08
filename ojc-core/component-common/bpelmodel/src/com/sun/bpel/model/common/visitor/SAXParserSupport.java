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
 * @(#)SAXParserSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.common.visitor;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import java.util.logging.Logger;

import org.xml.sax.Attributes;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.LexicalHandler;

import com.sun.bpel.model.common.MessageManager;
import com.sun.bpel.model.wsdlmodel.impl.XMLCommentImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLCharacterData;
import com.sun.bpel.xml.common.model.XMLComment;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.visitor.VisitorSupport;
import com.sun.bpel.xml.uri.URI;


/**
 * Supports SAX parsing of XML documents.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SAXParserSupport extends BPELVisitorSupport
        implements VisitorSupport {
    
    /** The logger. */
    private static final Logger mLogger = Logger.getLogger(SAXParserSupport.class.getName());
    
    /** MessageManager for localized strings. */    
    private static MessageManager mMsg = MessageManager.getManager(SAXParserSupport.class);
    
    /** JAXP Parser Factory System Property key */
    /*
    private static final String
        JAXP_PARSER_FACTORY_KEY = "javax.xml.parsers.SAXParserFactory";
    */

    /** Netbeans private Parser Factory System Property key */
    /*
    private static final String
        NB_PARSER_FACTORY_KEY = "netbeans.xml.FastSAXParserFactoryImpl";
    */

    /** Xerces SAX Parser Factory value */
    /*
    private static final String
        XERCES_PARSER_FACTORY_VALUE = "org.apache.xerces.jaxp.SAXParserFactoryImpl";
    */
    
    static final String JAXP_SCHEMA_LANGUAGE =
        "http://java.sun.com/xml/jaxp/properties/schemaLanguage";
    static final String W3C_XML_SCHEMA =
        "http://www.w3.org/2001/XMLSchema";
    static final String JAXP_SCHEMA_SOURCE =
        "http://java.sun.com/xml/jaxp/properties/schemaSource";
    
    static final String bpelXSDUrl = "/com/sun/bpel/model/visitor/wsbpel_main.xsd";
    
    static final String wsdlXSDUrl = "/com/sun/bpel/model/visitor/wsdl.xsd";
    
    static final String bpelExtXSDUrl = "/com/sun/bpel/model/visitor/sbynext.xsd";
    
    static final String bpelRuntimeXSDUrl = "/com/sun/bpel/model/visitor/sbynRuntime.xsd";
    
    /** Holds stack of XML parsers */
    protected Stack parsers = new Stack();
    
    /** Holds the only lexical handler (for comments) */
    protected LexicalHandler xmlLexicalHandler = null;
    
    /** Holds the XML reader */
    protected XMLReader xmlReader;

    /** Holds the locator for the current event. */
    protected Locator locator;
    
    /** Holds the map of imported documents processed already--used to avoid cyclic paths */
    protected Map processedDocs = new HashMap();
    
    /** Holds whether import/include directives should be followed deeply.
     * @since   5.1.0
     */
    protected Boolean deepImport;
    
    /** Holds the mount point URI of imported files.
     * @since   5.1.0
     */
    protected URI mountPoint;
    
    /** Holds the base project for imports.
     * @since   5.1.0
     */

    /** Creates a new instance of SAXParserSupport */
    public SAXParserSupport() {
        super();
    }
    
    /** Ignore exceptions if any from SAX XML Reader feature sets.
     * @param   xmlReader   XML Reader
     * @param   feature     Feature to set
     * @param   boolVal     Boolean value to use
     */
    private void setSAXXMLReaderFeature(XMLReader xmlReader, String feature, boolean boolVal) {
        try {
            xmlReader.setFeature(feature, boolVal);
        } catch (SAXException se) {
            mLogger.warning("SAX XML Reader feature \"" + feature + "\" not set to \"" + boolVal + "\"!");
        }
    }
    
    /** Getter for the XML reader.
     * @return  XML reader.
     */
    public XMLReader getXmlReader() {
    	return xmlReader;
    }
    
	
    /** Getter for the XML reader.
     * @return  XML reader.
     */
    public XMLReader createXmlReader(boolean validate) {
        if (null == xmlReader) {
            try {
                
                javax.xml.parsers.SAXParserFactory factory = javax.xml.parsers.SAXParserFactory.newInstance();
                javax.xml.parsers.SAXParser saxParser = null;
                
                if(validate) {
                //Set namespaceAware to true to get a parser that corresponds to
                // the default SAX2 namespace feature setting.  This is necessary
                // because the default value from JAXP 1.0 was defined to be false.
                	factory.setNamespaceAware(true);
                	factory.setValidating(true);
                
                	saxParser = factory.newSAXParser();
                
	                // Set the schema language if necessary
	                try {
	                    saxParser.setProperty(JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);
	                    InputStream[] ins = new InputStream[1];
	                    
	                    ins[0] = this.getClass().getResourceAsStream(bpelXSDUrl);
	                    /*
	                    ins[0] = this.getClass().getResourceAsStream(wsdlXSDUrl);
	                    ins[1] = this.getClass().getResourceAsStream(bpelXSDUrl);
	                    ins[2] = this.getClass().getResourceAsStream(bpelExtXSDUrl);
						ins[3] = this.getClass().getResourceAsStream(bpelRuntimeXSDUrl);
	                    */						
	                    saxParser.setProperty(JAXP_SCHEMA_SOURCE, ins);
	                    
	                } catch (SAXNotRecognizedException ex) {
	                    // This can happen if the parser does not support JAXP 1.2
	                	throw new XMLParseVisitorException(
	                            "SAX2 schema validation is not supported", ex);
	                }
                } else {
                	saxParser = factory.newSAXParser();
                }
                
                xmlReader = saxParser.getXMLReader();
                xmlReader.setEntityResolver(new Resolver());
                
                // SAX2 core features
                setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/namespaces", true);
                setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/namespace-prefixes", true);
//                setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/validation", true);
//                setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/external-general-entities", false);
//                setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/external-parameter-entities", false);
//                
                xmlReader.setProperty("http://xml.org/sax/properties/lexical-handler", getXmlLexicalHandler());
                
//              
            } catch (SAXNotRecognizedException snre) {
                throw new XMLParseVisitorException(
                    "SAX2 LexicalHandler not supported", snre);
            } catch (SAXNotSupportedException snse) {
                throw new XMLParseVisitorException(
                    "SAX2 LexicalHandler not supported", snse);
            } catch (Throwable trw) {
                throw new XMLParseVisitorException(
                    "SAXParserSupport.getXmlReader()", trw);
            }
        }
        return xmlReader;
    }
    
    /** Pushes a XML Parser onto the stack and uses it.
     * @param   parser     Parser to immediately use.
     * @param   xmlElement  XML element for this parser.
     * @return  XML element for this parser (same as input; a convenience).
     */
    public XMLElement pushXmlParser(XmlParser parser, XMLElement xmlElement) {
        parsers.push(new ParserEnv(parser, xmlElement));
        parser.setCurrentXMLNode(xmlElement);
        getXmlReader().setContentHandler(parser);
        //getXmlReader().setErrorHandler(parser);
        return xmlElement;
    }
    
    /** Pops off a XML Parser from the stack and uses the now topmost one.
     * @return  XML element correlating to the parser.
     * @throws  SAXException    When SAX problems occur.
     */
    public XMLElement popXmlParser() throws SAXException {
        XMLElement xmlElem = null;
        if (!parsers.isEmpty()) {
        	parsers.pop();
        }
        if (!parsers.isEmpty()) {
            ParserEnv pEnv = (ParserEnv) parsers.peek();
            XmlParser p = pEnv.getParser();
            xmlElem = pEnv.getXmlElement();
            p.setCurrentXMLNode(xmlElem);
            getXmlReader().setContentHandler(p);
           //getXmlReader().setErrorHandler(p);
            
        }
        return xmlElem;
    }
    
    /** Peeks at the XML parser envrionment stack.
     * @return  The top XML element being parsed.
     */
    protected XMLElement peekXmlElement() {
        XMLElement xmlElem = null;
        if (!parsers.isEmpty()) {
            ParserEnv pEnv = (ParserEnv) parsers.peek();
            xmlElem = pEnv.getXmlElement();
        }
        return xmlElem;
    }
    
    /** Getter for the Lexical handler for the SAX parser.
     * @return  Lexical handler
     */
    protected LexicalHandler getXmlLexicalHandler() {
        if (null == xmlLexicalHandler) {
            xmlLexicalHandler = new XmlLexer();
        }
        return xmlLexicalHandler;
    }

    /** Gets the location of the current event.
     * @return  locator for current event.
     */
    public Locator getLocator() {
        return locator;
    }
    
    /** Sets the location of the current event.
     * @param   locator     Locator for the current event.
     */
    public void setLocator(Locator locator) {
        this.locator = locator;
    }
    
    /** Implements the Lexical handler for the SAX parser
     */
    protected class XmlLexer implements LexicalHandler {
        
        // SAX Lexical Handler interface
        
        /** Called when a XML comment is encountered
         * @param   ch      Character array for the comment.
         * @param   offset  Offset to the array where the comment starts.
         * @param   length  Length of the comment
         */
        public void comment(char[] ch, int offset, int length) {
            XMLComment tComment = new XMLCommentImpl();
            tComment.setValue(new String(ch, offset, length));
            peekXmlElement().addChild(tComment);
        }
        
        /** Called when the end of a CDATA section is detected.
         */
        public void endCDATA() {
        }
        
        /** Called when the end of a DTD definition is detected.
         */
        public void endDTD() {
        }
        
        /** Called when the end of a entity is detected.
         * @param   name    Name of the entity.
         */
        public void endEntity(String name) {
        }
        
        /** Called when the start of a DTD definition is detected.
         * @param   name        Name of the DTD.
         * @param   publicId    Public ID of the DTD.
         * @param   systemId    System ID of the DTD.
         */
        public void startDTD(String name, String publicId,
        String systemId) {
        }
        
        /** Called when the start of a CDATA section is detected.
         */
        public void startCDATA() {
            XMLElement curElem = peekXmlElement();
            if (curElem instanceof XMLCharacterData) {
                ((XMLCharacterData) curElem).setCDATAForm(true);
            }
        }
        
        /** Called when the start of a entity is detected.
         * @param   name    Name of the entity.
         */
        public void startEntity(String name) {
        }
    }
    
    /** Implements a XML parser environment.
     */
    protected class ParserEnv {
        
        /** Holds the XML parser. */
        private XmlParser parser;
        
        /** Holds the XML element being parsed. */
        private XMLElement xmlElement;
        
        /** Constructs a XML parser environment object.
         * @param   p   XML parser.
         * @param   e   XML element being parsed.
         */
        public ParserEnv(XmlParser p, XMLElement e) {
            parser = p;
            xmlElement = e;
        }
        
        /** Getter for the XML parser.
         * @return  XML parser.
         */
        public XmlParser getParser() {
            return parser;
        }
        
        /** Getter for the XML element.
         * @return  XML element.
         */
        public XMLElement getXmlElement() {
            return xmlElement;
        }
    }
    
    /** Sets attributes of an element.
     * @param   element     XML element.
     * @param   attrs       Attributes from parser.
     */
    public static void setAttributes(XMLElement element, Attributes attrs) {
        setAttributes(element.getXmlAttributes(), element, attrs);
    }
    
    
    /** Sets attributes of an element.
     * @param   xmlAttrs    Specific element XML attributes to set.
     * @param   element     XML element.
     * @param   attrs       Attributes from parser.
     * @throws XMLParseVisitorException for invalid attribute values
     */
    public static void setAttributes(XMLAttribute[] xmlAttrs,
                                     XMLElement element, Attributes attrs)
        throws XMLParseVisitorException {
        for (int i = 0, n = attrs.getLength(); i < n; i++) {
            String qName = attrs.getQName(i);
            String val = attrs.getValue(i);
            boolean found = false;
            for (int j = 0; ((xmlAttrs != null)
                             && (j < xmlAttrs.length)); j++) {
                if (qName.endsWith(xmlAttrs[j].getLocalName())) {
//                    if (!xmlAttrs[j].isInEnumValues(val)) {
//                        throw new XMLParseVisitorException(
//                            mMsg.getString("THRW_Invalid_Attribute_Value",
//                                xmlAttrs[i].getLocalName(),
//                                '<' + element.getQualifiedName() + '>',
//                                '"' + val + '"'));
//                    }
                    element.setAttribute(j, qName, val);
                    found = true;
                    break;
                }
            }
            if (!found) {
                element.setOtherAttributes(qName, val);
            }
        }
        
        //RA This check is done at validation time, no need to do at
        //parse time
//        // check to make sure that required attributes are present
//        if (xmlAttrs != null) {
//            for (int i = 0; i < xmlAttrs.length; i++) {
//                if (!xmlAttrs[i].isOptional()
//                        && (null == xmlAttrs[i].getValue())) {
////                    throw new XMLParseVisitorException(
////                        mMsg.getString(
////                            "THRW_Missing_Required_Attribute_2",
////                            xmlAttrs[i].getLocalName(),
////                            '<' + element.getQualifiedName() + '>'));
//                	//get the document and line number
//                	String baseUri = element.getOwnerDocument().getBaseURI();
//                	Locator locator = element.getLocator();
//                	int lineNumber = -1;
//					if(locator != null) {
//						lineNumber = locator.getLineNumber();
//						
//					}
//                      mLogger.warning(
//                        mMsg.getString(
//                            "THRW_Missing_Required_Attribute_2",
//                            xmlAttrs[i].getLocalName(),
//                            '<' + element.getQualifiedName() + '>'+ " line: "+ lineNumber +" file:" + baseUri));
//                }
//            }
//        }
    }
    
    /**
     * Gets a processed import document.
     * @pararm  ns      Namespace of the document.
     * @param   uriLoc  URI location of the document.
     * @return  The processed document or <code>null</code> if not yet processed.
     */
    public Object getProcessedImportDocument(String ns, String uriLoc) {
        return processedDocs.get(makeProcessedImportDocumentKey(ns, uriLoc));
    }
    
    /**
     * Registers a processed import document.
     * @param   ns      Namespace of document.
     * @param   uriLoc  URI location of document.
     * @param   doc     Imported document.
     */
    public void registerProcessedImportDocument(String ns, String uriLoc, Object doc) {
        processedDocs.put(makeProcessedImportDocumentKey(ns, uriLoc), doc);
    }
    
    /**
     * Clears the processed import document registry.
     */
    public void clearProcessedImportDocumentRegistry() {
        processedDocs.clear();
    }
    
    /**
     * Gets the processed import document registry.
     * @return  Map representing the import document registry.
     */
    public Map getProcessedImportDocumentRegistry() {
        return processedDocs;
    }
    
    /**
     * Sets the processed import document registry.
     * @param   map     Map representing the import document registry.
     */
    public void setProcessedImportDocumentRegistry(Map map) {
        processedDocs = map;
    }
    
    /**
     * Forms the processed import document key.
     * @param   ns      Namespace of document.
     * @param   uriLoc  URI location of the document.
     */
    protected String makeProcessedImportDocumentKey(String ns, String uriLoc) {
        return "{" + ns + "}" + uriLoc;
    }
    
    class Resolver implements EntityResolver {
        public InputSource resolveEntity (String publicId, String systemId)
        {
          //System.out.println("******************************************EntityResolver");
          //System.out.println("publicId "+ publicId + "  systemId "+ systemId);
          try {
              if(systemId != null) {
                  if(systemId.endsWith("sbynRuntime.xsd")) {
                      InputStream in = this.getClass().getResourceAsStream(bpelRuntimeXSDUrl);
                      return new InputSource(in);
                  } else if(systemId.endsWith("sbynext.xsd")) {
                      InputStream in = this.getClass().getResourceAsStream(bpelExtXSDUrl);
                      return new InputSource(in);

                  } else if(systemId.endsWith("wsdl.xsd")) {
                      InputStream in = this.getClass().getResourceAsStream(wsdlXSDUrl);
                      return new InputSource(in);
                  }
              }
          } catch(Exception ex) {
              ex.printStackTrace();
          }
          
          return null;
        }
     }


}
