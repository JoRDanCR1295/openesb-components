/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.packaging;

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

import com.sun.jbi.dcombc.Endpoint.EndpointType;

/**
 * EndpointConfigurationPortmap is a class to parse the endpoints.xml file. This file is part of the
 * packaging of a Service Assembly
 * 
 * @author Chandrakanth Belde
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
