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

package com.sun.jbi.cam.manager.framework.common;

import java.io.FileInputStream;
import java.io.StringReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Attr; 
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * @author Sun MicrosystemInc.
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class Util {

    public static Element getXMLDocumentRoot(ZipFile file,String entryName) throws Exception{
        ZipEntry ze = file.getEntry(entryName);
        InputSource is = new InputSource(file.getInputStream(ze));
        DocumentBuilder db = DocumentBuilderFactory.newInstance().newDocumentBuilder() ;
        Document xmlDoc = db.parse(is);
        Element root = xmlDoc.getDocumentElement();
        return root;
    }

    public static Map getNameSpaces(Node root) {
        Map<String,String> map = new HashMap<String,String>();
        NamedNodeMap attributes = root.getAttributes();
        if(attributes.getLength() == 0) {
            return map;
        }
        for (int index = 0; index < attributes.getLength(); index++) {
            Attr attrib = (Attr)attributes.item(index);
            String attibuteName = attrib.getNodeName();
            int nameSpaceDelimiter = attibuteName.indexOf(":");
            if(nameSpaceDelimiter != -1) {
                map.put(attibuteName.substring(nameSpaceDelimiter+1, attibuteName.length()),
                                attrib.getValue());
            }
        }
        return map;
    }
    
    public static String getNodeTextValue(Node node) {
        NodeList nl = node.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node lNode = nl.item(i);
            if(lNode.getNodeType() == Node.TEXT_NODE) {
                return lNode.getNodeValue();
            }
            
        }
        return "";
    }
    
    public static String resolveNameSpace(Map nameSpace,String qName) {
        int nameSpaceDelimiter = qName.indexOf(":");
        if(nameSpaceDelimiter == -1) {
            return qName; // qname does not have name space
        }
        String ns = qName.substring(0, nameSpaceDelimiter);
        return nameSpace.get(ns)+"^"+qName.substring(nameSpaceDelimiter+1,
                        qName.length());
    }

     public static Element getXMLDocumentRoot(String xmlString) throws Exception{
        InputSource is = new InputSource(new StringReader(xmlString));
        DocumentBuilder db = DocumentBuilderFactory.newInstance().newDocumentBuilder() ;
        Document xmlDoc = db.parse(is);
        Element root = xmlDoc.getDocumentElement();
        return root;
    }
     
     
     public static MBeanServer getMBeanServer() {
        MBeanServer server = null;
        List servers = MBeanServerFactory.findMBeanServer(null);
        if(servers.size() > 0) {
            server = (MBeanServer)servers.get(0);
        }
        return server;
    }

}
