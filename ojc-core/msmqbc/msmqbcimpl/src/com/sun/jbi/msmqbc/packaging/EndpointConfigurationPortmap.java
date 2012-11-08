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
 * @(#)EndpointConfigurationPortmap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.packaging;

import java.util.List;
import java.util.ArrayList;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import org.xml.sax.SAXException;
import org.xml.sax.InputSource;

import com.sun.jbi.msmqbc.Endpoint.EndpointType;

/**
 * EndpointConfigurationPortmap is a class to parse the endpoints.xml file. This file is part of the
 * packaging of a Service Assembly
 * 
 * @author Sun Microsystems
 */
public class EndpointConfigurationPortmap implements EndpointConfiguration {

    private static final String ENDPOINTS_FILE_NAME = "endpoints.xml";

    public static final String TARGETNAMESPACE = "targetNamespace";

    public static final String SERVICE = "service";

    public static final String END_POINT = "endPoint";

    public static final String DIRECTION = "direction";

    public static final String WSDL_FILE = "wsdlFile";

    public static final String PORT_MAP = "portmap";

    public static final String INBOUND = "inbound";

    public static final String OUTBOUND = "outbound";

    // use arraylist for now
    private List services;

    public EndpointConfigurationPortmap(String rootDir) throws IOException, FactoryConfigurationError,
            ParserConfigurationException, SAXException {

        services = new ArrayList();

        File f = new File(rootDir, ENDPOINTS_FILE_NAME);

        if (!f.exists()) {
            String msg = "unable to locate endpoints.xml for root dir with id :" + rootDir;
            throw new IOException(msg);
        }

        FileReader fr = new FileReader(f);
        InputSource is = new InputSource(fr);
        DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();

        DocumentBuilder builder = fact.newDocumentBuilder();

        Document doc = builder.parse(is);

        Element root = doc.getDocumentElement();

        NodeList nl = root.getElementsByTagName(EndpointConfigurationPortmap.PORT_MAP);
        for (int i = 0; nl != null && i < nl.getLength(); i++) {
            Node n = nl.item(i);
            Element e = (Element) n;

            String s0 = e.getAttribute(EndpointConfigurationPortmap.TARGETNAMESPACE);
            String s1 = e.getAttribute(EndpointConfigurationPortmap.SERVICE);
            String s2 = e.getAttribute(EndpointConfigurationPortmap.END_POINT);
            String s3 = e.getAttribute(EndpointConfigurationPortmap.DIRECTION);
            String s4 = e.getAttribute(EndpointConfigurationPortmap.WSDL_FILE);
            String interfaceName = null; // No interface in endpoints.xml!
            if (s3.equals(INBOUND)) {
                addEndpoint(interfaceName, s1, s2, EndpointType.INBOUND);
            } else if (s3.equals(OUTBOUND)) {
                addEndpoint(interfaceName, s1, s2, EndpointType.OUTBOUND);
            }
        }
    }

    public void addEndpoint(EndpointData p) {
        services.add(p);
    }

    public void addEndpoint(String interfaceName, String service, String endPoint, int direction) {
        services.add(new EndpointDataImpl(interfaceName, service, endPoint, direction));
    }

    public List endpoints() {
        return services;
    }

}
