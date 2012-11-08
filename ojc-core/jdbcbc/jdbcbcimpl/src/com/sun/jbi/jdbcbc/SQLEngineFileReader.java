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
 * @(#)SQLEngineFileReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jdbcbc.util.ExtensionFilter;


/**
 * Used by SQLSE.
 * Reads a sqlse engine file and generates SQLEngineFileEntry[].
 */
public class SQLEngineFileReader {
    private static final Messages mMessages = Messages.getMessages(SQLEngineFileReader.class);
    private static final Logger mLogger = Messages.getLogger(SQLEngineFileReader.class);
    private static final String SQLDEF_TAG = "sqldef";

    /**
     * Parses the given file to generate a SQLEngineFileEntry[].
     * @param sqlengineFile
     * @return SQLEngineFileEntry[]
     * @throws org.xml.sax.SAXException
     * @throws java.io.IOException
     * @throws javax.xml.parsers.ParserConfigurationException
     */
    static SQLEngineFileEntry[] parse(final File asaDir, final File sqlengineFile)
        throws org.xml.sax.SAXException, java.io.IOException,
            javax.xml.parsers.ParserConfigurationException {
        assert sqlengineFile != null;

        final Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder()
                                             .parse(sqlengineFile);
        final Element elem = doc.getDocumentElement();
        final NodeList sqldefMaps = elem.getElementsByTagName(SQLEngineFileReader.SQLDEF_TAG);
        final SQLEngineFileEntry[] entryArr = new SQLEngineFileEntry[sqldefMaps.getLength()];

        for (int i = 0; i < sqldefMaps.getLength(); i++) {
            final Node childNode = sqldefMaps.item(i);
            final SQLEngineFileEntry engineFileEntry = new SQLEngineFileEntry();

            if (childNode.getNodeType() == Node.ELEMENT_NODE) {
                final Element childElem = (Element) childNode;
                final NodeList sqlFile = childElem.getElementsByTagName("sqlfile");
                final NodeList connDef = childElem.getElementsByTagName(
                        "connectiondef");

                if (sqlFile != null) {
                    final NamedNodeMap val = sqlFile.item(0).getAttributes();
                    String sqlFileName = val.getNamedItem("name").getNodeValue();
                    final File sqlFileObj = SQLEngineFileReader.getFileByName(asaDir, sqlFileName);

                    if (sqlFileName != null) {
                        final FileReader fr = new FileReader(sqlFileObj);
                        final BufferedReader reader = new BufferedReader(fr);
                        final StringBuffer sqlText = new StringBuffer();
                        String line = null;

                        while ((line = reader.readLine()) != null) {
                            if (sqlText.length() != 0) {
                                sqlText.append("\n");
                            }

                            sqlText.append(line);
                        }

                        engineFileEntry.setSQLText(sqlText.toString());
                        sqlFileName = sqlFileName.substring(sqlFileName.lastIndexOf(
                                    File.separator) + File.separator.length(),
                                sqlFileName.length());
                        engineFileEntry.setSQLFileName(sqlFileName);

                        SQLEngineFileReader.mLogger.log(Level.FINE, "SQLFR_READ_SQL",
                            sqlText.toString());
                        reader.close();
                        fr.close();
                    }
                }

                if (connDef != null) {
                    final NamedNodeMap val = connDef.item(0).getAttributes();
                    final String driverClass = val.getNamedItem("driverClass")
                                            .getNodeValue();
                    final String dbURL = val.getNamedItem("dbURL").getNodeValue();
                    final String databaseName = val.getNamedItem("databaseName")
                                             .getNodeValue();
                    final String user = val.getNamedItem("user").getNodeValue();
                    String password = val.getNamedItem("password").getNodeValue();
                    /*String pwd = null;
                    if(com.sun.jbi.jdbcbc.security.Base64Impl.getInstance().decode(password)!=null)
                         pwd = com.sun.jbi.jdbcbc.security.Base64Impl.getInstance().decode(password);
                    else
                        pwd = password;*/
                    final String jndi = val.getNamedItem("jndi_name").getNodeValue();
                    String transactionRequired = "";
                    if(val.getNamedItem("transactionRequired") != null) {
                            transactionRequired = val.getNamedItem("transactionRequired").getNodeValue();
                    }
                    engineFileEntry.setDriverClass(driverClass);
                    engineFileEntry.setDatabaseUrl(dbURL);
                    engineFileEntry.setDatabaseName(databaseName);
                    engineFileEntry.setDatabaseUserName(user);
                    engineFileEntry.setDatabasePassword(password);
					engineFileEntry.setJNDI(jndi);
					engineFileEntry.setTransactionRequired(transactionRequired);
                }
            }

            entryArr[i] = engineFileEntry;
        }

        SQLEngineFileReader.mLogger.log(Level.FINE, mMessages.getString("SQLSE_C00150.SQLMR_PARSE_SUCCESS"),entryArr);

        return entryArr;
    }

    /**
     * Helper method to find a specific SQLEngineFileEntry given a SQLFineEngineFileEntry[]
     * @param fea
     * @param sqlFileName
     * @return SQLEngineFileEntry
     */
    static SQLEngineFileEntry findSQLEngineFileEntry(
        final SQLEngineFileEntry[] fea, final String sqlFileName) {
        SQLEngineFileEntry sqlfeEntry = null;

        for (SQLEngineFileEntry element : fea) {
            if (element.getSqlFileName().equalsIgnoreCase(sqlFileName)) {
                sqlfeEntry = element;
            }
        }

        return sqlfeEntry;
    }

    protected static List<File> getFilesRecursively(final File dir, final FileFilter filter) {
        final List<File> ret = new ArrayList<File>();

        if (!dir.isDirectory()) {
            return ret;
        }

        final File[] fileNdirs = dir.listFiles(filter);

        for (File element : fileNdirs) {
            if (element.isDirectory()) {
                ret.addAll(SQLEngineFileReader.getFilesRecursively(element, filter));
            } else {
                ret.add(element);
            }
        }

        return ret;
    }

    protected static List getFilesRecursively(final File dir, final String[] extensions) {
        FileFilter filter = null;

        if (extensions[0].equals(".sql")) {
            filter = new ExtensionFilter(extensions);
        }

        return SQLEngineFileReader.getFilesRecursively(dir, filter);
    }

    protected static File getFileByName(final File dir, final String fileName) {
        final String extension = fileName.substring(fileName.lastIndexOf("."),
                fileName.length());
        final List files = SQLEngineFileReader.getFilesRecursively(dir, new String[] { extension });

        if (files != null) {
            final Iterator iter = files.iterator();

            while (iter.hasNext()) {
                final File sqlFile = (File) iter.next();

                if ((sqlFile != null) &&
                        sqlFile.getName().equalsIgnoreCase(fileName)) {
                    return sqlFile;
                }
            }
        }

        return null;
    }
}
