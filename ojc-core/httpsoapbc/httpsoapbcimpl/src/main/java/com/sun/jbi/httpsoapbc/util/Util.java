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
 * @(#)Util.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.events.XMLEvent;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Node;

public class Util {
    private static final TransformerPool cTransformerPool =
            new TransformerPool();
    
    /** Creates a new instance of Util */
    public Util() {
    }
    
    public static Node messageAsDom(NormalizedMessage normal) throws TransformerFactoryConfigurationError, TransformerException {
        DOMResult result = new DOMResult();
        Source src = normal.getContent();
        
        if (src != null) {
            if (src instanceof DOMSource) {
                return ((DOMSource)src).getNode();
            }
            Transformer transformer = cTransformerPool.retrieve();
            try {
                transformer.transform(src, result);
            } finally {
                cTransformerPool.relinquish(transformer);
            }
        }
        
        return result.getNode();
    }

    public static String toXml(Node node, String encoding, boolean omitXMLDeclaration) throws Exception {
        String ret = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Transformer trans = cTransformerPool.retrieve();
        try {
            trans.setOutputProperty(OutputKeys.ENCODING, encoding);
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            trans.setOutputProperty(OutputKeys.METHOD, "xml");
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
            trans.transform(new DOMSource(node), new StreamResult(baos));
            ret = baos.toString(encoding);
        } finally {
            cTransformerPool.relinquish(trans);
        }

        return ret;
    }
    
    public static Node textAsDom(String text) throws TransformerConfigurationException {
        if (text == null || "".equals(text)) {
            return null;
            
        }
        StringReader reader = new StringReader(text);
        DOMResult result = new DOMResult();
        Source src = new StreamSource(reader);
        
        Transformer transformer = cTransformerPool.retrieve();
        try {
            transformer.transform(src, result);
        } catch (TransformerException e) {
            result = null;
        } finally {
            cTransformerPool.relinquish(transformer);
        }
        
        return (result != null ? result.getNode() : null);
    }
    
    public static Node textAsDomWithXMLCheck(String text) throws TransformerConfigurationException, XMLStreamException, FactoryConfigurationError {
	if (text == null || "".equals(text)) {
	    return null;

	}
	// there is xml declaration
	if (text.startsWith("<?xml")) {
	    // do nothing this is a xml doc
	} else {
	    String text1 = "<dummy>" + text.trim() + "</dummy>";

	    if (checkForTextInput(text1)) {
		return null;
	    }
	}

	StringReader reader = new StringReader(text);
	DOMResult result = new DOMResult();
	Source src = new StreamSource(reader);

	Transformer transformer = cTransformerPool.retrieve();
	try {
	    transformer.transform(src, result);
	} catch (TransformerException e) {
	    result = null;
	} finally {
	    cTransformerPool.relinquish(transformer);
	}

	return (result != null ? result.getNode() : null);
    }

    private static boolean checkForTextInput(String text) throws XMLStreamException, FactoryConfigurationError {

	StringReader reader = new StringReader(text);

	XMLEventReader xmlReader = XMLInputFactory.newInstance().createXMLEventReader(reader);

	boolean b1, b2, b3, b4, b5;
	XMLEvent e = xmlReader.nextEvent();

	b1 = e.isStartDocument();//start doc

	e = xmlReader.nextEvent();//start dummy
	b2 = e.isStartElement();
	e = xmlReader.nextEvent();//text
	b3 = e.isCharacters();
	e = xmlReader.nextEvent();//end dummy
	b4 = e.isEndElement();
	e = xmlReader.nextEvent();//end document
	b5 = e.isEndDocument();

	if (b1 && b2 && b3 && b4 && b5) {
	    return true;
	}

	return false;

    }
    
}
