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
package com.sun.jbi.engine.mashup;

import java.io.File;
import java.util.Hashtable;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.wsdl.Definition;
import com.sun.jbi.internationalization.Messages;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

public class MashuplMapReader {
    private static final String EDMMAP_TAG = "edm";
    private static final String TYPE_TAG = "type";
    private static final String FILE_TAG = "file";
    private static final String PARTNERLINK_TAG = "partnerLink";
    private static final String PORTTYPE_TAG = "portType";
    private static final String OPERATION_TAG = "operation";
    private static final String REPLY_FILE_TAG = "replyFile";
    private static final String OUT_PARTNERLINK_TAG = "outPartnerLink";
    private static final String OUT_PORTTYPE_TAG = "outPortType";
    private static final String OUT_OPERATION_TAG = "outOperation";
    
    private static final Logger logger = Logger
			.getLogger(MashuplMapReader.class.getName());
    private static final Messages mMessages = Messages.getMessages(MashuplMapReader.class);
    
    protected MashuplMapReader() {
    }
    

    public static MashupMapEntryTable parse(File etlmapfile, MashupMapEntryTable etlMapEntryTable, String serviceUnitName,
                                          File deployDir, Hashtable wsdlMap)
        throws org.xml.sax.SAXException, 
               java.io.IOException,
               javax.xml.parsers.ParserConfigurationException 
    {
        assert etlmapfile != null;
        assert etlMapEntryTable != null;

        Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(etlmapfile);
        Element elem = doc.getDocumentElement();
        NodeList portmaps = elem.getElementsByTagName(EDMMAP_TAG);
        for (int i = 0; i < portmaps.getLength(); i++) {
            NamedNodeMap values = portmaps.item(i).getAttributes();
            String type = values.getNamedItem(TYPE_TAG).getNodeValue();
            QName partnerLink = getQName(values.getNamedItem(PARTNERLINK_TAG).getNodeValue());
            QName portType = getQName(values.getNamedItem(PORTTYPE_TAG).getNodeValue());
            QName operation = getQName(values.getNamedItem(OPERATION_TAG).getNodeValue());
            String file = values.getNamedItem(FILE_TAG).getNodeValue();
            String fullFilePath = deployDir.getAbsolutePath() + File.separator + file;
            QName outPartnerLink = null;
            QName outPortType = null;
            QName outOperation = null;
            String replyFile = null;
            String fullReplyFilePath = null;
            MashupMapEntry entry = null;
            Definition wsdl = (Definition) wsdlMap.get(portType);
            if (MashupMapEntry.REQUEST_REPLY_SERVICE.equals(type)) {
                entry = MashupMapEntry.newRequestReplyService(serviceUnitName, partnerLink, portType, operation, fullFilePath, wsdl);
                etlMapEntryTable.addEntry(entry);
                //logger.info(mMessages.getString("EDMSE-I0333.adding_Entry") + entry.toString());
                
                continue;
            }
            if (MashupMapEntry.FILTER_ONE_WAY.equals(type)) {
                outPartnerLink = getQName(values.getNamedItem(OUT_PARTNERLINK_TAG).getNodeValue());
                outPortType = getQName(values.getNamedItem(OUT_PORTTYPE_TAG).getNodeValue());
                outOperation = getQName(values.getNamedItem(OUT_OPERATION_TAG).getNodeValue());
                entry = MashupMapEntry.newFilterOneWay(serviceUnitName, partnerLink, portType, operation, fullFilePath, 
                    outPartnerLink, outPortType, outOperation, wsdl);
                etlMapEntryTable.addEntry(entry);
                logger.info(mMessages.getString("EDMSE-I0334.adding_FilterOneWayEntry")+ entry.toString());
                continue;
            }
            if (MashupMapEntry.FILTER_REQUEST_REPLY.equals(type)) {
                outPartnerLink = getQName(values.getNamedItem(OUT_PARTNERLINK_TAG).getNodeValue());
                outPortType = getQName(values.getNamedItem(OUT_PORTTYPE_TAG).getNodeValue());
                outOperation = getQName(values.getNamedItem(OUT_OPERATION_TAG).getNodeValue());
                replyFile = values.getNamedItem(REPLY_FILE_TAG).getNodeValue();
                fullReplyFilePath = deployDir.getAbsolutePath() + File.separator + replyFile;
                entry = MashupMapEntry.newFilterRequestReply(serviceUnitName, partnerLink, portType, operation, fullFilePath, 
                    outPartnerLink, outPortType, outOperation, fullReplyFilePath, wsdl);
                etlMapEntryTable.addEntry(entry);
                logger.info(mMessages.getString("EDMSE-I0335.filterRequest_Reply")+ entry.toString());
                continue;
            }
        }        
        return etlMapEntryTable;
    }
    
    private static QName getQName(String qname) {
        return QName.valueOf(qname);
    }
}
