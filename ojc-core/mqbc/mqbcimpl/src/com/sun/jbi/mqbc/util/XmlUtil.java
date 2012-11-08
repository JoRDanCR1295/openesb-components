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
 */

/*
 * @(#)XmlUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.text.MessageFormat;
import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import com.sun.jbi.mqbc.I18n;
import com.sun.jbi.mqbc.extensions.XmlSchemaDataTypes;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * This is the utility class to handle XML data.
 *
 * @author Noel.Ang@sun.com
 */
public class XmlUtil {
    
    // Pattern used to format calendar into XML Schema dateTime lexical representation:
    //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
    private static final String dateTimeFormatPattern =
            "{0,date,yyyy'-'MM'-'dd'T'HH':'mm':'ss.SSSZ}";
    
    // Pattern used to parse XML Schema dateTime values, for creating
    // Calendar objects.
    private static final Pattern dateTimeFormatRegexPattern =
            Pattern.compile("(-?\\d{4,})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})" 
                    + "(.\\d+)?((?:(\\+|-)(\\d{2}):(\\d{2}))|Z)");
    
    private static final TransformerFactory transformerFactory;
    private static final DocumentBuilderFactory documentBuilderFactory;
    static {
        transformerFactory = TransformerFactory.newInstance();
        documentBuilderFactory = DocumentBuilderFactory.newInstance();
    }
    
    private static Document createDocument(boolean namespaceAware,
                                           InputSource source
    )
            throws
            Exception {

        DocumentBuilder builder;
        synchronized (documentBuilderFactory) {
            documentBuilderFactory.setNamespaceAware(namespaceAware);
            builder = documentBuilderFactory.newDocumentBuilder();
        }
        return builder.parse(source);
    }
    
    /**
     * Creates a Document object with String based XML data
     *
     * @param namespaceAware  namespaceAware flag
     * @param xml             XML data
     * @return                Document object
     * @exception             exception upon error
     */
    public static Document createDocumentFromXML(boolean namespaceAware,
                                                 String xml)
                                                 throws SAXException, IOException, Exception {
        return createDocument(namespaceAware, new InputSource(new StringReader(xml)));
    }

    /**
     * Gets the text attribute of a DOM node
     *
     * @param node  Description of the Parameter
     * @return      The text value
     */
    public static String getText(Node node) {
        StringBuffer sb = new StringBuffer();
        NodeList children = node.getChildNodes();
        for (int i = 0, length = children.getLength(); i < length; i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.TEXT_NODE) {
                sb.append(child.getNodeValue());
            }
        }
        return sb.toString();
    }

    /**
     * Serializes DOM node to an array of bytes.
     *
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        byte array representation of the DOM node
     * @exception            		exception upon error
     */
    public static byte[] transformToBytes(Node node, 
                                          String encoding,
                                          boolean omitXMLDeclaration) 
                                          throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
 
        Transformer trans = transformerFactory.newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, "xml");
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        
        return baos.toByteArray();
    }
    
    /**
     * Serializes DOM node to String.
     *
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        String representation of the DOM node
     * @exception            		exception upon error
     */
    public static String transformToString(Node node, 
                                           String encoding, 
                                           boolean omitXMLDeclaration) 
                                           throws Exception {
        String xmlData = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        
        Transformer trans = transformerFactory.newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, "xml");
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        xmlData = baos.toString(encoding);
        
        return xmlData;
    }
    
    /**
     * Serializes DOM node to an array of bytes.
     *
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param method	                method to write the result
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        byte array representation of the DOM node
     * @exception            		exception upon error
     */
    public static byte[] transformToBytes(Node node, 
                                          String encoding,
                                          boolean omitXMLDeclaration,
                                          String method) 
                                          throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
 
        Transformer trans = transformerFactory.newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, method);
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        
        return baos.toByteArray();
    }
    
    /**
     * Serializes DOM node to an array of bytes.
     *
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param method	                method to write the result
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        String representation of the DOM node
     * @exception            		exception upon error
     */
    public static String transformToString(Node node, 
                                           String encoding, 
                                           boolean omitXMLDeclaration,
                                           String method) 
                                           throws Exception {
        String xmlData = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        
        Transformer trans = transformerFactory.newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, method);
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        xmlData = baos.toString(encoding);
        
        return xmlData;
    }
    
    public static DOMResult transformToDOMResult(Source source) throws Exception {
    	DOMResult result = new DOMResult();
    	
        Transformer trans = transformerFactory.newTransformer();
        trans.transform(source, result);
        
        return result;
    }

    public static String convertToDateTime(Calendar calendar) {
        if (calendar == null) {
            throw new NullPointerException("calendar");
        }
        
        MessageFormat format = new MessageFormat(dateTimeFormatPattern);
        String dateTimeString = format.format(new Object[] { calendar.getTime() });
        
        // MessageFormat qua DateFormat outputs RFC 822 time zone format,
        // for example "-0800".
        // XML Schema dateTime format requires a colon character between
        // hour and minute values, so it must be added manually,
        // for example "-08:00".
        if (!dateTimeString.endsWith("Z")) {
            StringBuffer buffer = new StringBuffer(dateTimeString);
            buffer.insert(buffer.length() - 2, ':');
            dateTimeString = buffer.toString();
        }
        return dateTimeString;
    }
    
    public static Calendar convertToCalendar(String xmlDateTime)
            throws DateTimeFormatException {
        if (xmlDateTime == null) {
            throw new NullPointerException("xmlDateTime");
        }
        
        Matcher matcher = dateTimeFormatRegexPattern.matcher(xmlDateTime);
        if (!matcher.matches()) {
            throw new DateTimeFormatException(I18n.msg(
                    "9100: Cannot parse value ''{0}'' as a dateTime.",
                    xmlDateTime));
        } else {
            int year = Integer.valueOf(matcher.group(1));
            int month = Integer.valueOf(matcher.group(2)) - 1;
            int monthday = Integer.valueOf(matcher.group(3));
            int hour = Integer.valueOf(matcher.group(4));
            int minute = Integer.valueOf(matcher.group(5));
            int second = Integer.valueOf(matcher.group(6));
            
            String fraction = matcher.group(7);
            int fractionSecond = (fraction != null && !"".equals(fraction)
                    ? Integer.valueOf(fraction.substring(1))
                    : 0);
            
            String utcOffset = matcher.group(8);
            int offset = 0;
            if (!utcOffset.equals("Z")) {
                int signum = (matcher.group(9).equals("-") ? -1 : 1);
                int hours = Integer.valueOf(matcher.group(10));
                int minutes = Integer.valueOf(matcher.group(11));
                offset = ((hours * 3600) + (minutes * 60)) * 1000;
                if (signum == -1) {
                    offset = 0 - offset;
                }
            }
            
            Calendar calendar = Calendar.getInstance();
            calendar.set(Calendar.YEAR, year);
            calendar.set(Calendar.MONTH, month);
            calendar.set(Calendar.DAY_OF_MONTH, monthday);
            calendar.set(Calendar.HOUR_OF_DAY, hour);
            calendar.set(Calendar.MINUTE, minute);
            calendar.set(Calendar.SECOND, second);
            calendar.set(Calendar.MILLISECOND, fractionSecond);
            //calendar.set(Calendar.ZONE_OFFSET, offset);
            calendar.getTimeInMillis(); // force recompute
            return calendar;
        }
    }

    /**
     * Translate character content to a Java object.
     *  
     * @param targetType Specifies the Java type to translate the data to.
     * @param sourceType Specifies the XML schema data type of the untranslated value.
     * @param value The value to translate.
     * 
     * @return An object whose type is assignable to targetType; never null.
     * @throws ConversionException if any error occurs.
     */
    public static Object parseValue(Class targetType, QName sourceType, String value)
            throws ConversionException {
        
        if (targetType == null) {
            throw new NullPointerException("targetType");
        }
        
        if (sourceType == null) {
            throw new NullPointerException("sourceType");
        }
        
        if (value == null) {
            throw new NullPointerException("value");
        }
        
        Object parsed;

        if (targetType == byte[].class) {
            if (XmlSchemaDataTypes.BASE64.qname.equals(sourceType)) {
                parsed = Base64.decode(value);
            } else if (XmlSchemaDataTypes.HEXBINARY.qname.equals(sourceType)) {
                parsed = HexBinary.decodeString(value);
            } else {
                throw new ConversionException(I18n.msg(
                        "9101: Unsupported conversion from type ''{0}'' to type ''{1}''",
                        sourceType.toString(),
                        targetType.getName()));
            }
        } else if (targetType == int.class) {
            try {
                parsed = Integer.valueOf(value.trim());
            } catch (NumberFormatException e) {
                throw new ConversionException(e);
            }
        } else if (targetType == Calendar.class) {
            try {
                parsed = XmlUtil.convertToCalendar(value.trim());
            } catch (DateTimeFormatException e) {
                throw new ConversionException(e);
            }
        } else if (targetType == String.class) {
            parsed = value;
        } else {
            throw new ConversionException(I18n.msg(
                    "9101: Unsupported conversion from type ''{0}'' to type ''{1}''",
                    sourceType.toString(),
                    targetType.getName()));
        }
        
        return parsed;
    }
}
