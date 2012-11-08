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
 * @(#)SQLMapReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.io.File;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import com.sun.jbi.internationalization.Messages;


/**
 * Used by SQLSE.
 * Helper class that reads a sqlmap file and generates SQLMapEntryTable containing
 * SQLMapEntry values.
 */
public class SQLMapReader {
    private static final Messages mMessages = Messages.getMessages(SQLMapReader.class);
    private static final Logger mLogger = Messages.getLogger(SQLMapReader.class);
    private static final String SQLMAP_TAG = "sql";
    private static final String TYPE_TAG = "type";
    private static final String SQL_FILE_TAG = "sqlfile";
    private static final String WSDL_FILE_TAG = "wsdlfile";
    private static final String PARTNERLINK_TAG = "partnerLink";
    private static final String PORTTYPE_TAG = "portType";
    private static final String OPERATION_TAG = "operation";
    private static final String REPLY_FILE_TAG = "replyFile";
    private static final String OUT_PARTNERLINK_TAG = "outPartnerLink";
    private static final String OUT_PORTTYPE_TAG = "outPortType";
    private static final String OUT_OPERATION_TAG = "outOperation";

    protected SQLMapReader() {
    }

    /**
     *
     * @param sqlmapfile
     * @param sqlMapEntryTable
     * @param serviceUnitName
     * @param deployDir
     * @param wsdlMap
     * @return
     * @throws org.xml.sax.SAXException
     * @throws java.io.IOException
     * @throws javax.xml.parsers.ParserConfigurationException
     */
    protected static SQLMapEntryTable parse(final File sqlmapfile,
        final SQLMapEntryTable sqlMapEntryTable, final String serviceUnitName,
        final File deployDir, final Hashtable wsdlMap)
        throws org.xml.sax.SAXException, java.io.IOException,
            javax.xml.parsers.ParserConfigurationException {
        assert sqlmapfile != null;
        assert sqlMapEntryTable != null;

        final Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder()
                                             .parse(sqlmapfile);
        final Element elem = doc.getDocumentElement();
        final NodeList portmaps = elem.getElementsByTagName(SQLMapReader.SQLMAP_TAG);

        for (int i = 0; i < portmaps.getLength(); i++) {
            final NamedNodeMap values = portmaps.item(i).getAttributes();
            final String type = values.getNamedItem(SQLMapReader.TYPE_TAG).getNodeValue();
            final QName partnerLink = SQLMapReader.getQName(values.getNamedItem(SQLMapReader.PARTNERLINK_TAG)
                                               .getNodeValue());
            final QName portType = SQLMapReader.getQName(values.getNamedItem(SQLMapReader.PORTTYPE_TAG)
                                            .getNodeValue());
            final QName operation = SQLMapReader.getQName(values.getNamedItem(SQLMapReader.OPERATION_TAG)
                                             .getNodeValue());
            final String sqlfile = values.getNamedItem(SQLMapReader.SQL_FILE_TAG).getNodeValue();
            final String wsdlfile = values.getNamedItem(SQLMapReader.WSDL_FILE_TAG).getNodeValue();

            //String fullWSDLFilePath = deployDir.getAbsolutePath() + File.separator + wsdlfile;
            QName outPartnerLink = null;
            QName outPortType = null;
            QName outOperation = null;
            String replyFile = null;
            String fullReplyFilePath = null;
            SQLMapEntry entry = null;
            final Definition wsdl = (Definition) wsdlMap.get(portType);

            if (SQLMapEntry.REQUEST_REPLY_SERVICE.equals(type)) {
                entry = SQLMapEntry.newRequestReplyService(serviceUnitName,
                        partnerLink, portType, operation, sqlfile, wsdlfile,
                        wsdl);
                sqlMapEntryTable.addEntry(entry);

                continue;
            }

            if (SQLMapEntry.FILTER_ONE_WAY.equals(type)) {
                outPartnerLink = SQLMapReader.getQName(values.getNamedItem(
                            SQLMapReader.OUT_PARTNERLINK_TAG).getNodeValue());
                outPortType = SQLMapReader.getQName(values.getNamedItem(SQLMapReader.OUT_PORTTYPE_TAG)
                                             .getNodeValue());
                outOperation = SQLMapReader.getQName(values.getNamedItem(SQLMapReader.OUT_OPERATION_TAG)
                                              .getNodeValue());
                entry = SQLMapEntry.newFilterOneWay(serviceUnitName,
                        partnerLink, portType, operation, sqlfile, wsdlfile,
                        outPartnerLink, outPortType, outOperation, wsdl);
                sqlMapEntryTable.addEntry(entry);

                continue;
            }

            if (SQLMapEntry.FILTER_REQUEST_REPLY.equals(type)) {
                outPartnerLink = SQLMapReader.getQName(values.getNamedItem(
                            SQLMapReader.OUT_PARTNERLINK_TAG).getNodeValue());
                outPortType = SQLMapReader.getQName(values.getNamedItem(SQLMapReader.OUT_PORTTYPE_TAG)
                                             .getNodeValue());
                outOperation = SQLMapReader.getQName(values.getNamedItem(SQLMapReader.OUT_OPERATION_TAG)
                                              .getNodeValue());
                replyFile = values.getNamedItem(SQLMapReader.REPLY_FILE_TAG).getNodeValue();
                fullReplyFilePath = deployDir.getAbsolutePath() +
                    File.separator + replyFile;
                entry = SQLMapEntry.newFilterRequestReply(serviceUnitName,
                        partnerLink, portType, operation, sqlfile, wsdlfile,
                        outPartnerLink, outPortType, outOperation,
                        fullReplyFilePath, wsdl);
                sqlMapEntryTable.addEntry(entry);

                continue;
            }
        }

        mLogger.log(Level.FINE, mMessages.getString("SQLSE_C00150.SQLMR_PARSE_SUCCESS"),
            sqlmapfile.getAbsolutePath());

        return sqlMapEntryTable;
    }

    private static QName getQName(final String qname) {
        return QName.valueOf(qname);
    }
}
