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
 * @(#)$Id: XpathTestUtil.java,v 1.4 2010/02/15 19:25:04 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.xpath.test;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.net.URI;
import java.net.URL;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.XmlUtil;

public class XpathTestUtil {
	
	
    public static Element loadInputMessageElement(String fileName) throws Exception {
        URL url = XpathTestUtil.class.getResource(fileName);
        URI uri = url.toURI();

        File file = new File(uri);

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document doc = factory.newDocumentBuilder().parse(file);

        return doc.getDocumentElement();
    }
    
    public static Element loadElement(String fileName) throws Exception {
        URL url = XpathTestUtil.class.getResource(fileName);
        URI uri = url.toURI();

        File file = new File(uri);

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document doc = factory.newDocumentBuilder().parse(file);

        return doc.getDocumentElement();
    }
    
    public static Element loadString(String xmlStr) throws Exception {
        Document doc = XmlUtil.createDocumentFromXML(true, xmlStr);
        return doc.getDocumentElement();
    }

    public static Element getElement(DOMSource source) throws Exception {
        Node node = source.getNode();
        if (node instanceof Document) {
            return ((Document) node).getDocumentElement();
        } else {
            return (Element) node;
        }
    }	

    public static Message loadMesasge(String wsdlFileName, String ns, String msgName) throws Exception {
        Definition definition = loadDefinition(wsdlFileName);
        Message msg = definition.getMessage(new QName (ns, msgName));
         return msg;
    }

    public static Definition loadDefinition(String wsdlName) throws Exception {

        Definition definition;

        URL url = XpathTestUtil.class.getResource(wsdlName);
        
        File f = new File (url.toURI());
        
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader();
        Definition def = null;

        try {
            FileInputStream fis = new FileInputStream(f);

            byte[] contents = new byte[fis.available()];
            fis.read(contents, 0, contents.length);
            fis.close();
            InputSource in = new InputSource(new ByteArrayInputStream(contents));
            in.setEncoding("UTF-8");
            Document doc = DocumentBuilderFactory. newInstance().newDocumentBuilder().parse(in);
            def = reader.readWSDL(url.toURI().toString());
            def.setDocumentBaseURI(f.getParentFile().toURI().toString());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw new WSDLException("WLM-6104", I18n.loc(
                    "WLM-6104: Unable to read WSDL defintion {0}", f.getAbsolutePath()), e);
        }
        return def;        
    }

}
