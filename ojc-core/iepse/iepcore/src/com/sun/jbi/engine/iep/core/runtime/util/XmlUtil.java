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
 * @(#)XmlUtil.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;

import java.io.*;
import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.xml.sax.InputSource;

import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.share.SharedConstants;
import java.util.logging.Level;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
 * XmlUtil.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class XmlUtil implements SharedConstants {
    private static final Messages mMessages = Messages.getMessages(XmlUtil.class);
    
    /**
     * Description of the Method
     *
     * @param namespaceAware  Description of the Parameter
     * @return                Description of the Return Value
     * @exception Exception   Description of the Exception
     */
    public static Document createDocument(boolean namespaceAware)
            throws Exception {
        
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        
        factory.setNamespaceAware(namespaceAware);
        
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.newDocument();
        
        return document;
    }
    
    /**
     * Description of the Method
     *
     * @param namespaceAware  Description of the Parameter
     * @param source          Description of the Parameter
     * @return                Description of the Return Value
     * @exception Exception   Description of the Exception
     */
    public static Document createDocument(boolean namespaceAware,
            InputSource source)
            throws Exception {
        
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        
        factory.setNamespaceAware(namespaceAware);
        //factory.setValidating();
        
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(source);
        
        document.normalize();
        
        return document;
    }
    
    /**
     * Description of the Method
     *
     * @param namespaceAware  Description of the Parameter
     * @param xml             Description of the Parameter
     * @return                Description of the Return Value
     * @exception Exception   Description of the Exception
     */
    public static Document createDocumentFromXML(boolean namespaceAware,
            String xml)
            throws Exception {
        return createDocument(namespaceAware,
                new InputSource(new StringReader(xml)));
    }
    
    private static final TransformerPool cTransformerPool = new TransformerPool();
    
    public static Document createDocumentFromSource(Source src)
            throws TransformerFactoryConfigurationError, TransformerException {
        if (src instanceof DOMSource) {
            return (Document)((DOMSource) src).getNode();
        }
        DOMResult result = new DOMResult();
        if (src != null) {
            Transformer transformer = cTransformerPool.retrieve();
            try {
                transformer.transform(src, result);
            } finally {
                cTransformerPool.relinquish(transformer);
            }
        }
        
        return (Document)result.getNode();
    }
    
    /**
     * Gets the text attribute of the DOMUtil class
     *
     * @param node  Description of the Parameter
     * @return      The text value
     */
    public static String getText(Node node) {
        StringBuffer buf = new StringBuffer();
        NodeList children = node.getChildNodes();
        for (int i = 0, I = children.getLength(); i < I; i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.TEXT_NODE) {
                buf.append(child.getNodeValue());
            }
        }
        return buf.toString();
    }
    
    private static String getColumnRowNodeText(Node columnRowNode, String columnType) {
        StringBuffer buf = new StringBuffer();
        if(SharedConstants.SQL_TYPE_CLOB.equals(columnType)) {
            //if it is clob the type it represents
            //is xsd:anyType so we can get a complex type
            //with multiple elments. so we need a place holder
            //element under which we store all the contents
            //of this clob field. We decided to use
            //the clob element name as the wrapper element.
            //so when reading this clob data, we should strip
            //the root element and use all of its chlidren.
            //we need root element so that we can use xml parser
            // to read it back correctly.
            //ex input has  purchasOrderAttachment field of type clob
            //and clob is represented as xsd:anyType
            //now actually data is sent as <purchaseOrder/>
            //then we got <purchasOrderAttachment>
            //                <purchaseOrder/>
            //                <purchaseOrder/>
            //            </purchasOrderAttachment>
            //so here columnRowNode points to <purchasOrderAttachment>
            //
            String data = XmlUtil.toXml(columnRowNode, "UTF-8", true);
            if(data != null) {
                buf.append(data);
            } else {
                //we can not get xml from node so return text
                return getText(columnRowNode);
            }
        } else {
            return getText(columnRowNode);
        }
        return buf.toString();
    }
    
    public static String getText(Object value, String type) {

    	if (value == null) {
    		return "";
    	}

    	if (value instanceof java.util.Date && SQL_TYPE_TIMESTAMP.equals(type)) {
    		try {
    			GregorianCalendar cal = new GregorianCalendar();
    			cal.setTimeInMillis(((Timestamp)value).getTime());
    			XMLGregorianCalendar xmlCal = DatatypeFactory.newInstance().newXMLGregorianCalendar(cal);
    			return xmlCal.toString();
    		} catch (DatatypeConfigurationException e) {
    			throw new RuntimeException(e);
    		}
    	}

    	if (value instanceof java.util.Date && SQL_TYPE_DATE.equals(type)) {
    		try {
    			GregorianCalendar cal = new GregorianCalendar();
    			cal.setTimeInMillis(((Date)value).getTime());
    			XMLGregorianCalendar xmlCal = DatatypeFactory.newInstance().newXMLGregorianCalendar(cal);
    			XMLGregorianCalendar xmlCal2 = DatatypeFactory.newInstance().newXMLGregorianCalendar();
    			xmlCal2.setDay(xmlCal.getDay());
    			xmlCal2.setMonth(xmlCal.getMonth());
    			xmlCal2.setYear(xmlCal.getYear());
    			xmlCal2.setTimezone(xmlCal.getTimezone());
    			return xmlCal2.toString();
    		} catch (DatatypeConfigurationException e) {
    			throw new RuntimeException(e);
    		}
    	}

    	if (value instanceof java.util.Date && SQL_TYPE_TIME.equals(type)) {
    		try {
    			GregorianCalendar cal = new GregorianCalendar();
    			cal.setTimeInMillis(((Time)value).getTime());
    			XMLGregorianCalendar xmlCal = DatatypeFactory.newInstance().newXMLGregorianCalendar(cal);
    			XMLGregorianCalendar xmlCal2 = DatatypeFactory.newInstance().newXMLGregorianCalendar();
    			xmlCal2.setHour(xmlCal.getHour());
    			xmlCal2.setMinute(xmlCal.getMinute());
    			xmlCal2.setSecond(xmlCal.getSecond());
    			xmlCal2.setFractionalSecond(xmlCal.getFractionalSecond());
    			xmlCal2.setTimezone(xmlCal.getTimezone());            	
    			return xmlCal2.toString();
    		} catch (DatatypeConfigurationException e) {
    			throw new RuntimeException(e);
    		}
    	}
    	return value.toString();
    }
    
    public static Object getObject(String value, String type) {
    	
    	if (type.equals(SQL_TYPE_TIMESTAMP)) {
    		try {
    			XMLGregorianCalendar xmlCal = DatatypeFactory.newInstance().newXMLGregorianCalendar(value);
    			if (xmlCal.getXMLSchemaType() != DatatypeConstants.DATETIME){
    				mMessages.log(Level.SEVERE, "XmlUtil.Invalid_timestamp", value);
    				throw new RuntimeException(mMessages.getString("XmlUtil.Invalid_timestamp", value));
    			}
    			GregorianCalendar g = xmlCal.toGregorianCalendar();
    			return new Timestamp(g.getTimeInMillis());
    		} catch (DatatypeConfigurationException e) {
    			mMessages.log(Level.SEVERE, "XmlUtil.Invalid_timestamp", value);
    			throw new RuntimeException(mMessages.getString("XmlUtil.Invalid_timestamp", value));
    		}
    	}

    	if (type.equals(SQL_TYPE_DATE)) {
    		try {
    			XMLGregorianCalendar xmlCal = DatatypeFactory.newInstance().newXMLGregorianCalendar(value);
    			if (xmlCal.getXMLSchemaType() != DatatypeConstants.DATE){
    				mMessages.log(Level.SEVERE, "XmlUtil.Invalid_date", value);
    				throw new RuntimeException(mMessages.getString("XmlUtil.Invalid_date", value));
    			}				
    			GregorianCalendar g = xmlCal.toGregorianCalendar();
    			Date ts = new Date(g.getTimeInMillis());
    			return ts;
    		} catch (DatatypeConfigurationException e) {
    			mMessages.log(Level.SEVERE, "XmlUtil.Invalid_date", value);
    			throw new RuntimeException(mMessages.getString("XmlUtil.Invalid_date", value));
    		}
    	}

    	if (type.equals(SQL_TYPE_TIME)) {
    		try {
    			XMLGregorianCalendar xmlCal = DatatypeFactory.newInstance().newXMLGregorianCalendar(value);
    			if (xmlCal.getXMLSchemaType() != DatatypeConstants.TIME){
    				mMessages.log(Level.SEVERE, "XmlUtil.Invalid_time", value);
    				throw new RuntimeException(mMessages.getString("XmlUtil.Invalid_time", value));
    			}				
    			GregorianCalendar g = xmlCal.toGregorianCalendar();
    			Time ts = new Time(g.getTimeInMillis());
    			return ts;
    		} catch (DatatypeConfigurationException e) {
    			mMessages.log(Level.SEVERE, "XmlUtil.Invalid_time", value);
    			throw new RuntimeException(mMessages.getString("XmlUtil.Invalid_time", value));
    		}
    	}
    	
    	return value;
    }
    
    /**
     * Description of the Method
     *
     * @param node  Description of the Parameter
     * @return      Description of the Return Value
     */
    // UTF-8
    public static String toXml(Node node, String encoding, boolean omitXMLDeclaration) {
        String ret = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            Transformer trans = TransformerFactory.newInstance().newTransformer();
            trans.setOutputProperty(OutputKeys.ENCODING, encoding);
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            trans.setOutputProperty(OutputKeys.METHOD, "xml");
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
            trans.transform(new DOMSource(node), new StreamResult(baos));
            ret = baos.toString(encoding);
            if (mMessages.isLoggable(Level.FINEST)) {
                mMessages.log(Level.FINEST, "XmlUtil.Result", ret);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "XmlUtil.Fail_to_turn_xml_Node_into_text", e);
        }
        return ret;
    }
    
    private static String convertNodeToClob(Node node) {
        
        StringBuffer buffer = new StringBuffer();
        
        NodeList l = node.getChildNodes();
        if(l.getLength()==1 && l.item(0).getNodeType() == Node.TEXT_NODE) {
            return l.item(0).getNodeValue();
        } else {
        for(int j = 0 ; j < l.getLength(); j++){
            Node n = l.item(j);
            
            buffer.append(toXml(n,"UTF-8",true));
        }
        return buffer.toString();   
        }
    }
    
    /**
     * input is either of form:
     * <${operation}Batch_MsgObj>
     *   <${operation}_MsgObj>
     *     <$col-name>....</$col-name>
     *     ... more col
     *   </${operation}_MsgObj>
     *   ... more evt
     * </${operation}Batch_MsgObj>
     *
     * or of form:
     *
     * <${operation}_MsgObj>
     *   <$col-name>....</$col-name>
     *   ... more col
     * </${operation}_MsgObj>
     */
    public static List<Object[]> messageToList(String operation, String xml, Schema schema) throws Exception {
        Document doc = createDocumentFromXML(false, xml);
        Element rootNode = doc.getDocumentElement();
        return messageToList(operation, rootNode, schema);
    }
    
    public static List<Object[]> messageToList(String operation, Element rootNode, Schema schema) throws Exception {
        List<Object[]> rowList = new ArrayList<Object[]>();
        String[] colNames = schema.getColumnNames();
        String[] colTypes = schema.getColumnTypes();
        int colCnt = colTypes.length;
        Object[] row = null;
        String tagName = rootNode.getTagName();
        if (tagName.endsWith(operation + "Batch_MsgObj")) {
            NodeList objNodeList = rootNode.getChildNodes();
            for (int i = 0, I = objNodeList.getLength(); i < I; i++) {
                Node objNode = objNodeList.item(i);
                if (objNode.getNodeType() != Node.ELEMENT_NODE ||
                        !objNode.getLocalName().equalsIgnoreCase(operation + "_MsgObj")) {
                    continue;
                }
                row = new Object[colCnt];
                NodeList colNodeList = objNode.getChildNodes();
                int k = 0;
                for (int j = 0, J = colNodeList.getLength(); j < J; j++) {
                    Node colNode = colNodeList.item(j);
                    if (colNode.getNodeType() != Node.ELEMENT_NODE ||
                            !colNode.getLocalName().equalsIgnoreCase(colNames[k])) {
                        continue;
                    }
                    row[k] = getObject(getColumnRowNodeText(colNode, colTypes[k]), colTypes[k]);
                    /*
                    if(colTypes[k].equals(SQL_TYPE_CLOB)) {
                        row[k] = convertNodeToClob(colNode);
                    } else {
                        row[k] = getObject(getText(colNode), colTypes[k]);
                    }*/
                    k++;
                }
                if (k < colCnt - 1) {
                    throw new Exception(mMessages.getString("XmlUtil.XML_has_fewer_entries_than_schema_expects", new Object[]{k, colCnt}));
                }
                rowList.add(row);
            }
        } else if (tagName.endsWith(operation + "_MsgObj")) {
            row = new Object[colCnt];
            NodeList colNodeList = rootNode.getChildNodes();
            for (int i = 0, I = colNodeList.getLength(), k = 0; i < I; i++) {
                Node colNode = colNodeList.item(i);
                if (colNode.getNodeType() != Node.ELEMENT_NODE ||
                        !colNode.getLocalName().equalsIgnoreCase(colNames[k])) {
                    continue;
                }
                
                row[k] = getObject(getColumnRowNodeText(colNode, colTypes[k]), colTypes[k]);
                
//                if(colTypes[k].equals(SQL_TYPE_CLOB)) {
//                        row[k] = convertNodeToClob(colNode);
//                    } else {
//                    row[k] = getObject(getText(colNode), colTypes[k]);
//                    }
                k++;
            }
            rowList.add(row);
        }
        return rowList;
    }
    
    public static List<Object[]> responseToList(String operation, Element rootNode, String[] attrNames, String[] attrTypes) throws Exception {
        List<Object[]> rowList = new ArrayList<Object[]>();
        int colCnt = attrNames.length;
        Object[] row = null;
        String tagName = rootNode.getTagName();
        if (tagName.endsWith(operation + "_ResponseObj")) {
            NodeList objNodeList = rootNode.getChildNodes();
            for (int i = 0, I = objNodeList.getLength(); i < I; i++) {
                Node objNode = objNodeList.item(i);
                if (objNode.getNodeType() != Node.ELEMENT_NODE ||
                        !objNode.getLocalName().equalsIgnoreCase(operation + "_ResponseItem")) {
                    continue;
                }
                row = new Object[colCnt];
                NodeList colNodeList = objNode.getChildNodes();
                int k = 0;
                for (int j = 0, J = colNodeList.getLength(); j < J; j++) {
                    Node colNode = colNodeList.item(j);
                    if (colNode.getNodeType() != Node.ELEMENT_NODE ||
                            !colNode.getLocalName().equalsIgnoreCase(attrNames[k])) {
                        continue;
                    }
                    row[k] = getObject(getColumnRowNodeText(colNode, attrTypes[k]), attrTypes[k]);
                    /*
                    if(colTypes[k].equals(SQL_TYPE_CLOB)) {
                        row[k] = convertNodeToClob(colNode);
                    } else {
                        row[k] = getObject(getText(colNode), colTypes[k]);
                    }*/
                    k++;
                }
                if (k < colCnt - 1) {
                    throw new Exception(mMessages.getString("XmlUtil.XML_has_fewer_entries_than_schema_expects", new Object[]{k, colCnt}));
                }
                rowList.add(row);
            }
        } 
        return rowList;
    }
    
    public static void constructColumnNodeContent(Document doc,
            Element columnNode,
            String columnName,
            String columnType,
            String columnRowData) 
    {
        //if column type is CLOB it is represented as xsd:anyType
        //so when converting a CLOB data to xml node
        //we need to make sure we strip the dummy wrapper
        //root element which was added when the original
        //data was stored in this CLOB
        if (SharedConstants.SQL_TYPE_CLOB.equals(columnType)) {
            String data = columnRowData;
            if (data != null) {
                try {
                    Document document = XmlUtil.createDocumentFromXML(false, data);
                    if (document != null) {
                        Element element = document.getDocumentElement();
                        if (element != null) {

                            //now we need to add all of imported
                            //node's children
                            NodeList children = element.getChildNodes();
                            for (int i = 0; i < children.getLength(); i++) {
                                Node importedChildNode = doc.importNode(children.item(i), true);
                                columnNode.appendChild(importedChildNode);
                            }

                            //we also should copy the attributes of
                            //our stored element in clob 
                            NamedNodeMap map = element.getAttributes();
                            for (int i = 0; i < map.getLength(); i++) {
                                Node node = map.item(i);
                                Attr attr = element.getAttributeNodeNS(node.getNamespaceURI(), node.getLocalName());
                                if (attr != null) {
                                    Attr newAttr = (Attr) doc.importNode(attr, true);
                                    columnNode.setAttributeNodeNS(newAttr);
                                }


                            }
                        }
                    }
                } catch (Throwable th) {
                    //if we can not parse then it is ok since
                    //CLOB data may not be a xml structure.
                    mMessages.log(Level.INFO,
                            mMessages.getString("XmlUtil.Fail_to_parse_clob_data_as_xml",
                            new Object[]{columnRowData, columnName}),
                            th);

                    //so take clob data as string, this is also a valid case
                    Text t = doc.createTextNode(columnRowData);
                    columnNode.appendChild(t);
                }
            }
        } else {
            Text t = doc.createTextNode(columnRowData);
            columnNode.appendChild(t);
        }
    }    
    
}
