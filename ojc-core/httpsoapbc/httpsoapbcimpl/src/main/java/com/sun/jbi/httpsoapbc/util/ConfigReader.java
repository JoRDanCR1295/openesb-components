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
 * @(#)ConfigReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.io.IOException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.SAXException;
import org.xml.sax.InputSource;

/**
 *
 */
public class ConfigReader {
    
    /** Creates a new instance of ConfigReader */
    public ConfigReader() {
    }
    
    public static Configuration parse(InputSource is) throws IOException, FactoryConfigurationError, ParserConfigurationException, SAXException {
        Configuration conf = new Configuration();
        DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
        
        DocumentBuilder builder = fact.newDocumentBuilder();
        
        Document doc = builder.parse(is);
        
        Element root = doc.getDocumentElement();
        
        NodeList nl = root.getElementsByTagName(Configuration.PORT_MAP);
        for (int i = 0; nl != null && i < nl.getLength(); i ++) {
            Node n = nl.item(i);
            Element e = (Element) n;
            
            String s0 = e.getAttribute(Configuration.TARGETNAMESPACE);
            String s1 = e.getAttribute(Configuration.SERVICE);
            String s2 = e.getAttribute(Configuration.END_POINT);
            String s3 = e.getAttribute(Configuration.DIRECTION);
            String s4 = e.getAttribute(Configuration.WSDL_FILE);
            Configuration.PortMap pm = conf.newPortMap(s0, s1, s2, s3, s4);
            conf.addPortMap(pm);
        }
        return conf;
    }
    
    public static void main (String argv[]) {
        try {
            ConfigReader.parse(new InputSource(System.getProperty("ALASKA_ROOT") + "/jbi/test/bps/src/bpelasa/portmap.xml"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
