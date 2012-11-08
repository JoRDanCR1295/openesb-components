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
 * @(#)PortMapReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;


/**
 * PortMapReader class helper class to read portmap.xml
 *
 * @author Sun Microsystems
 */
public class PortMapReader {
    private static final String PORTMAP_TAG = "portmap"; // NO I18N
    private static final String SERVICENAME_TAG = "service"; // NO I18N
    private static final String ENDPOINT_TAG = "endPoint"; // NO I18N
    private static final String ROLE_TAG = "role"; // NO I18N
    private static final String ROLENAME_TAG = "roleName"; // NO I18N
    private static final String PARTNERLINK_TAG = "partnerLink"; // NO I18N
    private static final String PARTNERLINKTYPE_TAG = "partnerLinkType"; // NO I18N

    /**
     * Creates a new instance of PortMapReader
     */
    protected PortMapReader() {
    }

    /**
     * parse portmap.xml file
     *
     * @param portmapfile File object for portmap.xml
     *
     * @return Iterator iterator of portmap entries
     *
     * @throws SAXException SAX parser exception
     * @throws IOException file IO exception
     * @throws ParserConfigurationException xml parser configuration exception
     */
    public static Iterator parse(java.io.File portmapfile)
        throws SAXException, IOException, ParserConfigurationException {
        LinkedList entries = null;

        XmlResourceProviderPool pool = (XmlResourceProviderPool)
                BPELSERegistry.getInstance().lookup(
                        XmlResourceProviderPool.class.getName());
        XmlResourceProvider rsrc = pool.acquireXmlResourceProvider();
        DocumentBuilder db = rsrc.getDocumentBuilder();
        Document doc = null;
        try { doc = db.parse(portmapfile); }
        finally { pool.releaseXmlResourceProvider(rsrc); }

        Element elem = doc.getDocumentElement();
        NodeList portmaps = elem.getElementsByTagName(PORTMAP_TAG);

        for (int i = 0; i < portmaps.getLength(); i++) {
            NamedNodeMap values = portmaps.item(i).getAttributes();

            QName service = getQName(values.getNamedItem(SERVICENAME_TAG)
                                           .getNodeValue());
            QName endpoint = getQName(values.getNamedItem(ENDPOINT_TAG)
                                            .getNodeValue());
            String role = values.getNamedItem(ROLE_TAG).getNodeValue();
            String roleName = values.getNamedItem(ROLENAME_TAG).getNodeValue();
            QName partnerlink = getQName(values.getNamedItem(PARTNERLINK_TAG)
                                               .getNodeValue());
            QName partnerlinktype = getQName(values.getNamedItem(PARTNERLINKTYPE_TAG)
                                               .getNodeValue());

            if (entries == null) {
                entries = new LinkedList();
            }

            PortMapEntry entry = new PortMapEntry(service, endpoint, role,
                    partnerlink, partnerlinktype, roleName);
            entries.add(entry);
        }

        return (entries != null) ? entries.iterator() : null;
    }

    private static QName getQName(String qname) {
        return QName.valueOf(qname);
    }
}
