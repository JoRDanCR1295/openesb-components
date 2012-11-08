/**
 *   rss-binding-component - RSS Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.uddi.component;

import com.gestalt.jbi.nmr.NmrWrapperUtils;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Namespace;

import org.jdom.output.DOMOutputter;
import org.jdom.output.XMLOutputter;

import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;


/**
 * Takes the UDDI inquiry results and builds the jbi wrapper 1.1 message.
 */
public class UDDINormalizer {
    private Logger log = Logger.getLogger(UDDINormalizer.class.getName());

    public void createConsumerContent(QName type, List<String> servicesList,
        NmrWrapperUtils wrapper) {
        try {
            wrapper.addComplexType(getEndpoints(servicesList));
        } catch (Exception e) {
            log.warning(e.toString());
        }
    }

    /**
     * Takes the syndicated feed and builds a string representation of the entries.
     * @param services
     * @return String
     */
    private org.w3c.dom.Element getEndpoints(List<String> services)
        throws JDOMException {
        DOMOutputter domOutputter = new DOMOutputter();
        XMLOutputter xmlOutputter = new XMLOutputter();

        Namespace namespace = Namespace.getNamespace(
                "http://xml.netbeans.org/schema/1.0/ws/addressing/extensions");
        Element endpointListElement = new Element("EndpointReferenceList",
                namespace);
        Document document = new Document(endpointListElement);

        for (String accessUrl : services) {
            Element endpointElement = new Element("EndpointReference", namespace);
            Element addressElement = new Element("Address", namespace);

            addressElement.setText(accessUrl);
            endpointElement.addContent(addressElement);
            endpointListElement.addContent(endpointElement);
        }

        log.fine("Normalizer getEntries returning: " +
            xmlOutputter.outputString(endpointListElement));

        return domOutputter.output(document).getDocumentElement();
    }
}
