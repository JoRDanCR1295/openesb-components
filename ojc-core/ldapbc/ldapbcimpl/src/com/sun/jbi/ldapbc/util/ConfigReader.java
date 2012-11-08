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

package com.sun.jbi.ldapbc.util;

import java.io.IOException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import com.sun.jbi.ldapbc.util.Configuration.PortMap;


/**
 *ConfigReader
 */
public class ConfigReader {
    /** Creates a new instance of ConfigReader */
    public ConfigReader() {
    }

    /**
     *
     * @param is
     * @return
     * @throws IOException
     * @throws FactoryConfigurationError
     * @throws ParserConfigurationException
     * @throws SAXException
     */
    public static Configuration parse(final InputSource is)
        throws IOException, FactoryConfigurationError,
            ParserConfigurationException, SAXException {
        final Configuration conf = new Configuration();
        final DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
        final DocumentBuilder builder = fact.newDocumentBuilder();
        final Document doc = builder.parse(is);
        final Element root = doc.getDocumentElement();
        final NodeList nl = root.getElementsByTagName(Configuration.PORT_MAP);

        for (int i = 0; (nl != null) && (i < nl.getLength()); i++) {
            final Node n = nl.item(i);
            final Element e = (Element) n;
            final String s0 = e.getAttribute(Configuration.TARGETNAMESPACE);
            final String s1 = e.getAttribute(Configuration.SERVICE);
            final String s2 = e.getAttribute(Configuration.END_POINT);
            final String s3 = e.getAttribute(Configuration.DIRECTION);
            final String s4 = e.getAttribute(Configuration.WSDL_FILE);
            final PortMap pm = conf.newPortMap(s0, s1, s2, s3, s4);
            conf.addPortMap(pm);
        }

        return conf;
    }

    /**
     *
     * @param argv
     */
    public static void main(final String[] argv) {
        try {
            final Configuration c = ConfigReader.parse(new InputSource(
                        "C:/Alaska/root/jbi/test/bps/src/bpelasa/portmap.xml"));
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }
}
