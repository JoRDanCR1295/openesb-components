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
 * @(#)SAXWriterSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.visitor;

import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.common.MessageManager;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.common.model.XMLText;

import java.io.Writer;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Map.Entry;
import java.util.logging.Logger;
import java.util.logging.Level;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;



/**
 * Supports SAX2 writing of writing documents.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SAXWriterSupport implements VisitorSupport {
    
    /** The logger. */
    private static final Messages MESSAGES = 
            Messages.getMessages(SAXWriterSupport.class);
    private static final Logger LOGGER = 
            Messages.getLogger(SAXWriterSupport.class);
    
    /** MessageManager for localized strings. */    
    //private static MessageManager mMsg = 
    //MessageManager.getManager(SAXWriterSupport.class);
    
    /** Holds value of property writer. */
    private Writer writer;
    
    /** Holds the SAX2Writer property */
    private SAX2Writer xmlWriter;
    
    /** Holds the current XML element stack */
    private Stack curXmlElement = new Stack();
    
    /** Creates a new instance of SAXWriterSupport.
     */
    public SAXWriterSupport() {
    }
    
    /** Getter for property writer.
     * @return Value of property writer.
     *
     */
    public Writer getWriter() {
        return writer;
    }
    
    /** Setter for property writer.
     * @param writer New value of property writer.
     *
     */
    public void setWriter(Writer writer) {
        this.writer = writer;
        setXmlWriter(new SAX2Writer(false, writer));
    }
    
    /** Getter for property xmlWriter.
     * @return Value of property xmlWriter.
     *
     */
    public SAX2Writer getXmlWriter() {
        return xmlWriter;
    }
    
    /** Setter for property xmlWriter.
     * @param xmlWriter New value of property xmlWriter.
     *
     */
    protected void setXmlWriter(SAX2Writer xmlWriter) {
        this.xmlWriter = xmlWriter;
    }
    
    /** Add this attribute to the list of attributes for an element.
     * @param   xmlAttrs    XML attribute array.
     * @param   attrs       Ordered attribute list.
     * @throws  SAXException    if there are XML syntax errors.
     */
    protected void addToAttributeList(List xmlAttrs, AttributesImpl attrs)
        throws SAXException {
        if (xmlAttrs != null) {
            for (int i = 0, n = xmlAttrs.size(); i < n; i++) {
                XMLAttribute attr = (XMLAttribute) xmlAttrs.get(i);
                if (!attr.isOptional() && ((attr.getValue() == null)
                        || (attr.getValue().length() == 0))) {
                    LOGGER.log(Level.FINE,
                            MESSAGES.getString(
                            "SAXWriterSupport.THRW_Missing_Required_Attribute",
                            new Object[]{attr.getLocalName()} ));
//                    throw new SAXException(
//                        mMsg.getString("THRW_Missing_Required_Attribute",
//                                        attr.getLocalName()));
                } else if (attr.getValue() != null) {
                    attrs.addAttribute("", attr.getLocalName(),
                                       attr.getQualifiedName(),
                                       MESSAGES.getString(
                                                    "SAXWriterSupport.CDATA"),
                                                                attr.getValue());
                }
            }
        }
    }
    
    /** Converts all the key/value pairs other attributes to XML Attributes.
     * @param   m       The <tt>Map</tt> object to convert.
     * @param   attr    The <tt>AttributesImpl</tt> object to append.
     *                  If <tt>null</tt> and there are <tt>HashMap</tt> entries,
     *                  a new <tt>AttributesImpl</tt> will be created and 
     *                  returned.
     * @return  The corresponding XML Attributes.
     */
    protected AttributesImpl addOtherToAttributeList(Map m,
                                                     AttributesImpl attr) {
        AttributesImpl retAttr = attr;
        if ((m != null) && !m.isEmpty()) {
            Iterator iter = m.entrySet().iterator();
            while (iter.hasNext()) {
                Entry me = (Entry) iter.next();
                if (null == retAttr) {
                    retAttr = new AttributesImpl();
                }
                retAttr.addAttribute("", (String) me.getKey(),
                                     (String) me.getKey(),
                                     MESSAGES.getString(
                                                    "SAXWriterSupport.CDATA"),
                                        (String) me.getValue());
            }
        }
        return retAttr;
    }
    
    /** Converts all the XML namespace attributes to XML Attributes.
     * @param   m       The <tt>Map</tt> object to convert.
     * @param   attr    The <tt>AttributesImpl</tt> object to append.
     *                  If <tt>null</tt> and there are <tt>HashMap</tt> entries,
     *                  a new <tt>AttributesImpl</tt> will be created and
     *                  returned.
     * @return  The corresponding XML Attributes.
     */
    protected AttributesImpl addNamespaceToAttributeList(Map m,
                                                         AttributesImpl attr) {
        AttributesImpl retAttr = attr;
        if ((m != null) && !m.isEmpty()) {
            Iterator iter = m.entrySet().iterator();
            while (iter.hasNext()) {
                Entry me = (Entry) iter.next();
                if (null == retAttr) {
                    retAttr = new AttributesImpl();
                }
                String key = (String) me.getKey();
                if (XMLElement.WellKnownAttr.XMLNS.equals(key)) {
                    // as-is
                } else {
                    key = XMLElement.WellKnownAttr.XMLNS_COLON + key;
                }
                retAttr.addAttribute("", key, key, MESSAGES.getString(
                                                    "SAXWriterSupport.CDATA"),
                                                        (String) me.getValue());
            }
        }
        return retAttr;
    }

    /** Determines if an element is being started or ended.
     * @param   e   The element.
     * @return  <tt>true</tt> if element is being started.
     */
    public boolean isElementStart(XMLElement e) {
        boolean start = false;
        if (curXmlElement.isEmpty() || !curXmlElement.peek().equals(e)) {
            curXmlElement.push(e);
            start = true;
        } else {
            curXmlElement.pop();
            start = false;
        }
        return start;
    }
    
    /** Determines if an element is being the current element in stack
     * @param   e   The element.
     * @return  <tt>true</tt> if element is being current element.
     */
    public boolean isCurrentElement(XMLElement e) {
        boolean isCurrentElement = false;
        if (!curXmlElement.isEmpty() && curXmlElement.peek().equals(e)) {
        	isCurrentElement = true;
        } 
        return isCurrentElement;
    }
    
    /** Writes an element (start, ..., end).
     * @param   e   Element.
     */
    public void writeElement(XMLElement e) {
        try {
            if (isElementStart(e)) {
                AttributesImpl attrs = new AttributesImpl();
                addToAttributeList(e.getOrderedAttributes(), attrs);
                addOtherToAttributeList(e.getRawPresentationMap(false), attrs);
                addOtherToAttributeList(e.getOtherAttributes(), attrs);
                addNamespaceToAttributeList(e.getNamespaces(), attrs);
                getXmlWriter().startElement("", e.getLocalName(),
                                            e.getQualifiedName(), attrs);
            } else {
                getXmlWriter().endElement("", e.getLocalName(),
                                          e.getQualifiedName());
            }
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                MESSAGES.getString(
                    "SAXWriterSupport.CANT_WRITE_ELEM"), trw);
        }
    }
    
    /** Writes an empty element.
     * @param   e   Element.
     */
    public void writeEmptyElement(XMLElement e) {
        try {
            AttributesImpl attrs = new AttributesImpl();
            addToAttributeList(e.getOrderedAttributes(), attrs);
            addOtherToAttributeList(e.getRawPresentationMap(false), attrs);
            addOtherToAttributeList(e.getOtherAttributes(), attrs);
            addNamespaceToAttributeList(e.getNamespaces(), attrs);
            getXmlWriter().emptyElement("", e.getLocalName(),
                                        e.getQualifiedName(), attrs);
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                MESSAGES.getString(
                    "SAXWriterSupport.CANT_WRITE_EMPTY_ELEM"), trw);
        }
    }
    
    /** Writes the text section of an element.
     * @param   t   Text section.
     */
    public void writeText(XMLText t) {
        try {
            getXmlWriter().characters(t.getValue().toCharArray(), 0,
                                      t.getValue().length());
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                    MESSAGES.getString(
                    "SAXWriterSupport.CANT_WRITE_TEST"), trw);
        }
    }
    
}
