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
 * @(#)XmlParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.common.visitor;

import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;

/** Base implementation of a XML parser.
 */
public class XmlParser extends DefaultHandler 
    implements ContentHandler, DTDHandler, EntityResolver, ErrorHandler {
    
    /** Holds current XML element being processed */
    private XMLNode mCurXmlElement = null;
    
    /** Holds the parser support for this XML parser. */
    protected SAXParserSupport parserSupport;
    
    public SAXParseException saxParseException=null;
    
    /** Constructs a XML parser.
     */
    public XmlParser() {
        super();
    }
    
    /** Constructs a XML parser.
     * @param   sup     Parser support.
     */
    public XmlParser(SAXParserSupport sup) {
        super();
        parserSupport = sup;
    }
    
    /** Captures the location of the current event.
     * @param   locator     Locator object.
     */
    public void setDocumentLocator(Locator locator) {
        if ((parserSupport != null) && (null == parserSupport.getLocator())) {
            parserSupport.setLocator(locator);
        }
    }
    
     
    public void error(SAXParseException exception) throws SAXException	       {
    } 
    
    public void fatalError(SAXParseException exception) throws SAXException {                        
    }	
    
    public void warning(SAXParseException exception) throws SAXException {
    }	
    
    public final void setCurrentXMLNode(XMLNode curXmlElement) {
        mCurXmlElement = curXmlElement;
    }
    
    public final XMLNode getCurrentXMLNode() {
        return mCurXmlElement;
    }

}
