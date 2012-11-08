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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.common.util;

import com.zaz.ssapi.protocol.common.model.CustomizeOutputInfo;
import com.zaz.ssapi.protocol.common.model.CustomizeOutputField;

import com.zaz.ssapi.protocol.common.model.RowColumnDef;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.List;
import java.util.Map;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

/**
 *
 * @author tianlize
 */
public class EmulatorXSDGenerator {

    private static Document doc = null;
    private static Element root = null;
    private static Element requestElement = null;
    private static Element responseElement = null;

    public EmulatorXSDGenerator() {
    }

    public static void generateXSDFile(List<String> paramList, CustomizeOutputInfo customizeOutputInfo, File dir, String name) {
        Map<Integer, List<CustomizeOutputField>> outputInfo = customizeOutputInfo.getOutputInfo();
        createDocument(paramList, name, outputInfo);
        writeXML(doc, dir, name + ".xsd");
    }

    public static void writeXML(Document document, File dir, String fileName) {
        try {
            TransformerFactory tf = TransformerFactory.newInstance();
            tf.setAttribute("indent-number", new Integer(4));
            Transformer transformer = tf.newTransformer();
            DOMSource source = new DOMSource(document);
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            PrintWriter pw = new PrintWriter(new FileOutputStream(dir.getPath() + File.separator + "src" + File.separator + fileName));
            StreamResult result = new StreamResult(pw);
            transformer.transform(source, result);
        } catch (TransformerConfigurationException ex) {
            Logger.getLogger(EmulatorXSDGenerator.class.getName()).log(Level.SEVERE, null, ex);
        } catch (TransformerException ex) {
            Logger.getLogger(EmulatorXSDGenerator.class.getName()).log(Level.SEVERE, null, ex);
        } catch (FileNotFoundException ex) {
            Logger.getLogger(EmulatorXSDGenerator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private static void createDocument(List<String> paramList, String name, Map<Integer, List<CustomizeOutputField>> outputInfo) {
        initDocument(name);
        createRequestElement(paramList, outputInfo);
        createResponseElement(outputInfo);
//        createGlobleElements();
    }

    private static void createRequestElement(List<String> paramList, Map<Integer, List<CustomizeOutputField>> outputInfo) {
        requestElement = createElement("Request", null, null, null);
        Element reqElement = createComplexType(null);
        Element sequence = createSequence();
        if (paramList != null && paramList.size() > 0) {
            for (int k = 0; k < paramList.size(); k++) {
                String param = paramList.get(k);
                Element paramElement = createElement(param, "xsd:string", null, null);
                sequence.appendChild(paramElement);
            }
        }

//        int mapSize = outputInfo.keySet().size();
//        if (mapSize > 0) {
//            for (int i = 0; i < mapSize; i++) {
//                List<CustomizeOutputField> customizeOutputFields = outputInfo.get(i);
//                createScreenRequestElement(sequence, i, customizeOutputFields);
//            }
//        }
        reqElement.appendChild(sequence);
        requestElement.appendChild(reqElement);
        root.appendChild(requestElement);
    }

    private static void createResponseElement(Map<Integer, List<CustomizeOutputField>> outputInfo) {
        responseElement = createElement("Response", null, null, null);
        Element responseType = createComplexType(null);
        Element sequence = createSequence();

        Element responseStatus = createElement("status", "xsd:string", null, null);
        sequence.appendChild(responseStatus);

        int mapSize = outputInfo.keySet().size();
        if (mapSize > 0) {
            for (int i = 0; i < mapSize; i++) {
                List<CustomizeOutputField> customizeOutputFields = outputInfo.get(i);
                createResponseScreenElement(sequence, i, customizeOutputFields);
            }
        }
        responseType.appendChild(sequence);
        responseElement.appendChild(responseType);
        root.appendChild(responseElement);
    }

    private static void createResponseScreenElement(Element sequence, int screenIndex, List<CustomizeOutputField> fields) {
        Element screenElement = createElement("screen" + screenIndex, null, null, null);
        Element complexType = createComplexType(null);
        Element sequence2 = createSequence();
        if (fields.size() > 0) {
            for (int j = 0; j < fields.size(); j++) {
                createResponseElement(sequence2, fields.get(j));
            }
        }
        complexType.appendChild(sequence2);
        screenElement.appendChild(complexType);
        sequence.appendChild(screenElement);
    }

    private static void createResponseElement(Element sequence, CustomizeOutputField field) {
        Element resElement = null;
        if (field.getFieldType() == CustomizeOutputField.SINGLE_VALUE || field.getFieldType() == CustomizeOutputField.CHILD_NODE_VALUE) {
            resElement = createElement(field.getXsdName(), "xsd:string", null, null);
        } else if (field.getFieldType() == CustomizeOutputField.MULTI_VALUE) {
            resElement = createElement(field.getXsdName(), "xsd:string", null, "unbounded");
        } else if (field.getFieldType() == CustomizeOutputField.LIST_TABLE_VALUE) {
            resElement = createListTableResponseElement(field);
        } else {
            resElement = createTableResponseElement(field);
        }
        sequence.appendChild(resElement);
    }

    private static Element createListTableResponseElement(CustomizeOutputField field) {
        Element resElement = createElement(field.getXsdName(), null, null, null);
        Element complexType = createComplexType(null);
        Element sequence = createSequence();

        Element owner = createElement("owner", "xsd:string", null, null);
        sequence.appendChild(owner);

        Element fields = createElement("fields", null, null, "unbounded");
        Element complexType2 = createComplexType(null);
        Element sequence2 = createSequence();
        List<RowColumnDef> rowColumnDefs = field.getRowCols();
        if (rowColumnDefs.size() > 0) {
            for (int i = 0; i < rowColumnDefs.size(); i++) {
                RowColumnDef rowColumnDef = rowColumnDefs.get(i);
                sequence2.appendChild(createElement(rowColumnDef.getXsdName(), "xsd:string", null, null));
            }
        }
        complexType2.appendChild(sequence2);
        fields.appendChild(complexType2);
        
        sequence.appendChild(fields);
        complexType.appendChild(sequence);
        resElement.appendChild(complexType);
        return resElement;
    }

    private static Element createTableResponseElement(CustomizeOutputField field) {
        Element resElement = createElement(field.getXsdName(), null, null, null);
        Element complexType = createComplexType(null);
        Element sequence = createSequence();
        List<RowColumnDef> rowColumnDefs = field.getRowCols();
        if (rowColumnDefs.size() > 0) {
            for (int i = 0; i < rowColumnDefs.size(); i++) {
                RowColumnDef rowColumnDef = rowColumnDefs.get(i);
                sequence.appendChild(createElement(rowColumnDef.getXsdName(), "xsd:string", null, null));
            }
        }
        complexType.appendChild(sequence);
        resElement.appendChild(complexType);
        return resElement;
    }

    private static void initDocument(String name) {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = null;
        try {
            db = dbf.newDocumentBuilder();
        } catch (ParserConfigurationException pce) {
            pce.printStackTrace();
            System.exit(1);
        }

        doc = db.newDocument();
        root = doc.createElement("xsd:schema");
        root.setAttribute("xmlns:xsd", "http://www.w3.org/2001/XMLSchema");
        root.setAttribute("targetNamespace", "http://xml.netbeans.org/schema/" + name);
        root.setAttribute("xmlns:tns", "http://xml.netbeans.org/schema/" + name);
        root.setAttribute("elementFormDefault", "qualified");
        doc.appendChild(root);
    }

    private static Element createElement(String name, String type, String fixedValue, String maxOccur) {
        Element element = doc.createElement("xsd:element");
        element.setAttribute("name", name);
        if (null != type) {
            element.setAttribute("type", type);
        }
        if (null != fixedValue) {
            element.setAttribute("fixed", fixedValue);
        }
        if (null != maxOccur) {
            element.setAttribute("maxOccurs", maxOccur);
        }
        return element;
    }

    private static Element createComplexType(String complexTypeName) {
        Element element = doc.createElement("xsd:complexType");
        if (complexTypeName != null) {
            element.setAttribute("name", complexTypeName);
        }
        return element;
    }

    private static Element createSequence() {
        return doc.createElement("xsd:sequence");
    }

    public static String formatAsXMLParam(String param) {
        String ret = "";
        for (int i = 0; i < param.length(); i++) {
            String s = param.substring(i, i + 1);
            if (Pattern.matches("[0-9a-zA-Z]", s)) {
                ret += s;
            }
        }
        return ret.toLowerCase();
    }
}
