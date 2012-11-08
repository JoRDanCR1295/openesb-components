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
 * @(#)HL7SFTSegmentProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7bc.extservice.support.hl7v25;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.List;

import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.jbi.messaging.NormalizedMessage;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.w3c.dom.NodeList;

import com.sun.jbi.hl7bc.extservice.exception.HL7ApplicationException;
import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.I18n;

/**
 * This class update the Normalized message with SFT segment
 * 
 * @author Raghunadh
 */
public class HL7SFTSegmentProcessor implements HL7Constants {

    private static final Logger mLog = Logger.getLogger(HL7SFTSegmentProcessor.class.getName());

    public static boolean canSFTBeEnabled(HL7ProtocolProperties protocolProperties) {
        String versionID = protocolProperties.getVersionID();
        if (HL7v25.equalsIgnoreCase(versionID) || HL7v251.equalsIgnoreCase(versionID) ||
				HL7v26.equalsIgnoreCase(versionID)) {
            return true;
        }
        return false;
    }

    /**
     * Process SFT Segment
     * 
     * @param protocolProperties HL7ProtocolProperties
     * @param normalizedMsg NormalizedMessage
     * @exception exception upon error
     */

    public static void processSFTSegment(HL7ProtocolProperties protocolProperties, NormalizedMessage normalizedMsg)
            throws HL7ApplicationException, Exception {

        boolean sftEnabled = protocolProperties.getSFTEnabled().booleanValue();
        boolean sftProcessingEnabled = canSFTBeEnabled(protocolProperties) && sftEnabled;
        if (!sftProcessingEnabled) {
            mLog.log(Level.SEVERE, I18n.msg("E0314: Failed to process the SFT segment fields for the HL7 message either sftEnabled is set to {1} or invalid version {0}",
                    protocolProperties.getVersionID(), (new Boolean(sftEnabled)).toString()));
            throw new HL7ApplicationException(I18n.msg("E0314: Failed to process the SFT segment fields for the HL7 message either sftEnabled is set to {1} or invalid version {0}",
						protocolProperties.getVersionID(), (new Boolean(sftEnabled)).toString()));
        }
        if (!populateSFTRequiredFields(protocolProperties, normalizedMsg)) {
            mLog.log(Level.SEVERE, I18n.msg("E0315: Failed to populate the SFT segment fields for the HL7 message"));
            throw new HL7ApplicationException(I18n.msg("E0315: Failed to populate the SFT segment fields for the HL7 message"));

        }

    }

    private static boolean populateSFTRequiredFields(HL7ProtocolProperties protocolProperties,
                                                     NormalizedMessage normalizedMsg) throws Exception {

        Source source = normalizedMsg.getContent();
        Document doc = getDocument(source);
        boolean success = true;
        Node rootNode = ((DOMSource) source).getNode();
        Node SFTNode = rootNode.getFirstChild().getFirstChild().getFirstChild().getFirstChild().getNextSibling();
        if (!SFTNode.getLocalName().equals(SFT)) {
            Document newDoc = generateSFTNode(doc, protocolProperties);
            normalizedMsg.setContent(new DOMSource(newDoc));
        }

        return success;
    }

    /**
     * get Document object
     * 
     * @param src Source
     * @return normalizedDoc Document
     * @exception exception upon error
     */

    private static Document getDocument(Source src) throws Exception {

        DOMResult result = new DOMResult();
        if (src != null) {
            TransformerFactory fact = TransformerFactory.newInstance();
            Transformer transformer = fact.newTransformer();
            transformer.transform(src, result);
        }
        Node node = result.getNode();
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }

        return normalizedDoc;

    }

    /**
     * Update Document object with SFT Node
     * 
     * @param doc Documnet
     * @param protocol HL7ProtocolProperties
     * @return doc Document
     * @exception exception upon error
     */

    private static Document generateSFTNode(Document doc, HL7ProtocolProperties protocolProperties) {
        Node root = doc.getFirstChild().getFirstChild().getFirstChild();
        String namespaceURI = root.getNamespaceURI();
        Element SFT = doc.createElementNS(namespaceURI, "someNS:SFT");

        Element SFT1 = doc.createElementNS(namespaceURI, "SFT.1");
        Element XON1 = doc.createElementNS(namespaceURI, "XON.1");
        Element SFT2 = doc.createElementNS(namespaceURI, "SFT.2");
        Element SFT3 = doc.createElementNS(namespaceURI, "SFT.3");
        Element SFT4 = doc.createElementNS(namespaceURI, "SFT.4");
        Element SFT5 = doc.createElementNS(namespaceURI, "SFT.5");
        Element SFT6 = doc.createElementNS(namespaceURI, "SFT.6");
        Element TS1 = doc.createElementNS(namespaceURI, "TS.1");
        Text textSFT1 = doc.createTextNode(protocolProperties.getSoftwareVendorOrganization());
        XON1.appendChild(textSFT1);
        SFT1.appendChild(XON1);
        Text textSFT2 = doc.createTextNode(protocolProperties.getSoftwareCertifiedVersionOrReleaseNumber());
        SFT2.appendChild(textSFT2);
        Text textSFT3 = doc.createTextNode(protocolProperties.getSoftwareProductName());
        SFT3.appendChild(textSFT3);
        Text textSFT4 = doc.createTextNode(protocolProperties.getSoftwareBinaryID());
        SFT4.appendChild(textSFT4);
        Text textSFT5 = doc.createTextNode(protocolProperties.getSoftwareProductInformation());
        SFT5.appendChild(textSFT5);
        Text textSFT6 = doc.createTextNode(protocolProperties.getSoftwareInstallDate());
        TS1.appendChild(textSFT6);
        SFT6.appendChild(TS1);
        SFT.appendChild(SFT1);
        SFT.appendChild(SFT2);
        SFT.appendChild(SFT3);
        SFT.appendChild(SFT4);
        SFT.appendChild(SFT5);
        SFT.appendChild(SFT6);
        NodeList list = doc.getFirstChild().getFirstChild().getFirstChild().getChildNodes();
        for (int i = 0; i < list.getLength(); i++) {
            Node node = list.item(i);
            if (node.getLocalName().equals("MSH")) {
                Node nextNode = node.getNextSibling();
                node.getParentNode().insertBefore((Node) SFT, nextNode);
                break;
            }

        }

        return doc;
    }

}