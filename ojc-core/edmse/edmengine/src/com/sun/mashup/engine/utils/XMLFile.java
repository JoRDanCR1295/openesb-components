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

package com.sun.mashup.engine.utils;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;

/**
 * XMLUtil to parse and get nodeslist.
 *
 * @author karthikeyan s
 */

//TODO cleanup this class later on
public class XMLFile {
    
    private Element root;
    
    private File xmlFile;
    private static Logger logger = Logger.getLogger(XMLFile.class.getName());
    private static final Messages mMessages = Messages.getMessages(XMLFile.class);
    
    /** Creates a new instance of XMLUtil */
    public XMLFile(File file) {
        xmlFile = file;
        parse();
    }
    
    public NodeList getElementByTagName(String tagName) {		
        return root.getElementsByTagName(tagName);
    }
    
    
       /**  
     * Extracts attribute, if any, with given name from given DOM element.
     *
     * @param element DOM element in which to locate attribute
     * @param attrName name of attribute to extract
     * @param nullIfEmptyString indicates whether to return null if attribute is not found
     *        or returns as empty string
     * @return attribute value as String. If parsing the element for attrName results in
     *         an empty String (or if either element or attrName are null), then return
     *         null if nullIfEmptyString is true, else return an empty String.
     */
    public  String getAttributeFrom(Element element, String attrName, boolean nullIfEmptyString) {        
        
        if (element == null || attrName == null) {
            return (nullIfEmptyString) ? null : "";
        }

        String val = element.getAttribute(attrName);
        if ("".equals(val) && nullIfEmptyString) {
            val = null;
        }
        return val;
    }
   
    private void parse() {        
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document document = builder.parse(xmlFile);
            root = document.getDocumentElement();
        } catch (IOException ex) {
            //ignore
        } catch (ParserConfigurationException ex) {
            //ignore
        } catch (SAXException ex) {
            //ignore
        }
    }    
}