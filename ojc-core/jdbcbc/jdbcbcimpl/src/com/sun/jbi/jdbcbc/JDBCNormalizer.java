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
 * @(#)JDBCNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.CallableStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jdbcbc.extensions.JDBCConstants;
import com.sun.jbi.jdbcbc.extensions.JDBCOperationInput;
import com.sun.jbi.jdbcbc.extensions.JDBCOperationOutput;
import com.sun.jbi.jdbcbc.model.metadata.DBMetaData;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.jdbcbc.util.XMLCharUtil;
import com.sun.jbi.jdbcbc.model.metadata.DBMetaData;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCNormalizer {
    private static final Messages mMessages = Messages.getMessages(JDBCNormalizer.class);
    private static final Logger mLogger = Messages.getLogger(JDBCNormalizer.class);
    private static DocumentBuilder mBuilder = null;
    private WrapperBuilder wrapperBuilder;
    private ArrayList outParamIndex = new ArrayList();
    private HashMap<Integer,String> outParamTypes = new HashMap<Integer,String>();
    private HashMap<Integer,String> outParamNames = new HashMap<Integer,String>();
    private String mRecordPrefix = null; //113494 


    /** Creates a new instance of SoapNormalizer 
     * @throws javax.jbi.messaging.MessagingException 
     */
    public JDBCNormalizer() throws MessagingException {
        try {
            wrapperBuilder = HelperFactory.createBuilder();
        } catch (final WrapperProcessingException ex) {
            throw new MessagingException(JDBCNormalizer.mMessages.getString(
                    "SQLSE_E00701.JDBCN_Failed_Create") + ex.getMessage(), ex);
        }
    }

    /**
     *
     * @param rowsUpdated
     * @param exchange
     * @param meta
     * @return
     * @throws MessagingException
     */
    public NormalizedMessage normalize(final int rowsUpdated,
        final MessageExchange exchange, final OperationMetaData meta)
        throws MessagingException {
        final NormalizedMessage normalMsg = exchange.createMessage();

        try {
            Document normalDoc = JDBCNormalizer.newDocument();
            String returnPartName = null;
            final JDBCOperationOutput jdbcOpOutput = meta.getJDBCOperationOutput();

            if (jdbcOpOutput != null) {
                returnPartName = jdbcOpOutput.getReturnPartName();

                if (returnPartName == null) {
                    final String msgEx = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                        "missing " + JDBCOperationOutput.ATTR_RETURN_PART_NAME +
                        " attribute in " +
                        JDBCConstants.QNAME_OPERATION_OUTPUT;
                    throw new MessagingException(msgEx);
                }
            } else {
                final String msgEx = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                    "missing " + JDBCConstants.QNAME_OPERATION_OUTPUT;
                throw new MessagingException(msgEx);
            }

            if (HelperFactory.WRAPPER_ENABLED) {
                String operationOutputName = null;
                Message msg = null;
                final Output output = meta.getOperation().getOutput();

                if (output != null) {
                    operationOutputName = output.getName();
                    msg = output.getMessage();

                    if (msg != null) {
                        wrapperBuilder.initialize(normalDoc, msg,
                            operationOutputName);

                        final Part part = msg.getPart(returnPartName);

                        if (part != null) {
                            final Element returnPartElement = normalDoc.createElement(returnPartName);
                            final Text text = normalDoc.createTextNode("" +
                                    rowsUpdated);

                            final QName type = part.getTypeName();
                            {
                                if (type != null) {
                                    returnPartElement.appendChild(text);
                                    wrapperBuilder.addPart(part.getName(), returnPartElement);
                                } else {
                                    final QName element = part.getElementName();
                                    final Element elementRoot = normalDoc.createElementNS(element.getNamespaceURI(), element.getLocalPart());
                                    returnPartElement.appendChild(elementRoot);

                                    elementRoot.appendChild(text);
                                    wrapperBuilder.addPart(part.getName(), elementRoot);
                                }
                                normalDoc = wrapperBuilder.getResult();
                            }
                        } else {
                            final String msgEx = JDBCNormalizer.mMessages.getString(
                                    "SQLSE_E00703.JDBCN_Failed_NM_Part") + returnPartName +
                                "in message " + msg.getQName();
                            throw new MessagingException(msgEx);
                        }
                    } else {
                        final String msgEx = JDBCNormalizer.mMessages.getString(
                                "SQLSE_E00704.JDBCN_Failed_NM_WS_OPER") +
                            meta.getOperation().getName() +
                            " is missing message in its <output>";
                        throw new MessagingException(msgEx);
                    }
                } else {
                    final String msgEx = JDBCNormalizer.mMessages.getString(
                            "SQLSE_E00704.JDBCN_Failed_NM_WS_OPER") +
                        meta.getOperation().getName() +
                        " is missing <output> ";
                    throw new MessagingException(msgEx);
                }
            } else {
                final String operationName = meta.getBindingOperation().getName();
                final Element normalRoot = normalDoc.createElement(operationName);
                normalDoc.appendChild(normalRoot);

                if (returnPartName != null) {
                    final Element returnElement = normalDoc.createElement(returnPartName);
                    final Text text = normalDoc.createTextNode("" + rowsUpdated);
                    returnElement.appendChild(text);
                    normalRoot.appendChild(returnElement);
                }
            }

            if (JDBCNormalizer.mLogger.isLoggable(Level.INFO)) {
                JDBCNormalizer.mLogger.log(Level.INFO, "normalized message", normalDoc);
            }

            normalMsg.setContent(new DOMSource(normalDoc));
        } catch (final ParserConfigurationException tex) {
            final String msg = JDBCNormalizer.mMessages.getString("SQLSE_E00705.JDBCN_Failed_NM_DOM");
            throw new MessagingException(msg, tex);
        } catch (final WrapperProcessingException ex) {
            final String exMsg = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                ex.getMessage();
            throw new MessagingException(exMsg, ex);
        }

        return normalMsg;
    }

    /**
     *
     * @param rs
     * @param exchange
     * @param meta
     * @param epb
     * @param pkName
     * @return
     * @throws MessagingException
     * @throws SQLException
     * @throws ParserConfigurationException
     * @throws TransformerConfigurationException
     * @throws TransformerException
     */
    public NormalizedMessage normalizeSelectInbound(final ResultSet rs,
            final MessageExchange exchange, final OperationMetaData meta, final EndpointBean epb,
            final String pkName)
            throws MessagingException, SQLException, ParserConfigurationException,
                TransformerConfigurationException, TransformerException {
            final NormalizedMessage normalMsg = exchange.createMessage();

            //113494  start
            mRecordPrefix = meta.getJDBCSql().getTableName();
            // see if the table name is null (SQL SE)
            // then take the query name as the prefix
            if (mRecordPrefix == null) {
                mRecordPrefix = meta.getJDBCOperationOutput().getName();
                mRecordPrefix = mRecordPrefix.substring(0, mRecordPrefix.indexOf("Response"));
            }
            //113494  end

            try {
                Document normalDoc = JDBCNormalizer.newDocument();
                String returnPartName = null;
                String NS = "";
    			int numberOfRecords = meta.getJDBCOperationInput()
                                          .getNumberOfRecords();


                QName elementQName = null;
                final JDBCOperationInput jdbcOpInput = meta.getJDBCOperationInput();

                if (jdbcOpInput == null) {
                    final String msgEx = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                        "missing " + JDBCConstants.QNAME_OPERATION_OUTPUT;
                    throw new MessagingException(msgEx);
                }

                if (HelperFactory.WRAPPER_ENABLED) {
                    String operationInputName = null;
                    Message msg = null;
                    final Input input = meta.getOperation().getInput();

                    if (input != null) {
                        operationInputName = input.getName();
                        msg = input.getMessage();

                        /* Adding for inbound */
                        final Map parts = msg.getParts();
                        final Iterator it = parts.values().iterator();

                        while (it.hasNext()) {
                            final Part part = (Part) it.next();
                            elementQName = part.getElementName();

                            if (elementQName == null) {
                                final String msgEx = JDBCNormalizer.mMessages.getString(
                                        "SQLSE_E00703.JDBCN_Failed_NM_Part") + part.getName() +
                                    "should have element attribute defined.";
                                throw new MessagingException(msgEx);
                            }

                            // } //while
                            returnPartName = elementQName.getLocalPart();

                            if (returnPartName == null) {
                                final String msgEx = JDBCNormalizer.mMessages.getString(
                                        "SQLSE_E00702.JDBCN_Failed_NM") + "missing "// +
                                    // JDBCOperationInput.ATTR_RETURN_PART_NAME
                                    // + " attribute in "
                                     + JDBCConstants.QNAME_OPERATION_INPUT;
                                throw new MessagingException(msgEx);
                            }

                            /* End of inbound */
                            if (msg != null) {
                                wrapperBuilder.initialize(normalDoc, msg,
                                    operationInputName);

                                // Part part = msg.getPart(returnPartName);
                                if (part != null) {
                                    final Element returnPartElement = normalDoc.createElement(returnPartName);
                                    final QName type = part.getTypeName();
                                    {
                                        if (type != null) {
                                            // get resultset metadata rsmd and add it to the element
                                            final ResultSetMetaData rsmd = rs.getMetaData();
                                            if (numberOfRecords == -1) {
                                                while (rs.next()) {
                                            	final Element record = normalDoc.createElement(mRecordPrefix + "record"); //113494 
                                                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                        final String colName = rsmd.getColumnName(j);
                                                        final String colValue = rs.getString(j);
                                                        final Element e = normalDoc.createElement(XMLCharUtil.makeValidNCName(colName));
                                                        e.appendChild(normalDoc.createTextNode(colValue));
                                                    record.appendChild(e);
                                                }
                                                returnPartElement.appendChild(record);
                                            }
                                            } else {
                                                while (rs.next() && (numberOfRecords > 0)) {
                                            	final Element record = normalDoc.createElement(mRecordPrefix + "record"); //113494 
                                                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                        final String colName = rsmd.getColumnName(j);
                                                        String colValue = rs.getString(j);

                                                        if (rs.wasNull()) {
                                                            colValue = "";
                                                        }

                                                        final Element e = normalDoc.createElement(XMLCharUtil.makeValidNCName(colName));
                                                        e.appendChild(normalDoc.createTextNode(colValue));
                                                    record.appendChild(e);
                                                }
                                                returnPartElement.appendChild(record);
                                                numberOfRecords--;
                                            }
                                        }
                                            wrapperBuilder.addPart(part.getName(), returnPartElement);
                                        } else {
                                            final QName element = part.getElementName();
                                            NS = element.getNamespaceURI();

                                            final Element elementRoot = normalDoc.createElementNS(NS, element.getLocalPart());

                                            // returnPartElement.appendChild(elementRoot);
                                            // get resultset metadata rsmd and add it to the element
                                            final ResultSetMetaData rsmd = rs.getMetaData();
                                            final List<String> pKeyList = new ArrayList<String>();

                                            if (numberOfRecords == -1) {
                                                while (rs.next()) {
                                            	final Element record = normalDoc.createElementNS(NS,mRecordPrefix + "record"); //113494
                                                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                        final String colName = rsmd.getColumnName(j);
                                                        final String colValue = rs.getString(j);

                                                        if (colName.equalsIgnoreCase(pkName) || ("\"" + colName + "\"").equalsIgnoreCase(pkName)) {
                                                            pKeyList.add(colValue);
                                                        }

                                                        final Element e = normalDoc.createElementNS(NS, XMLCharUtil.makeValidNCName(colName));
                                                        e.appendChild(normalDoc.createTextNode(colValue));
                                                    record.appendChild(e);
                                                }
                                                elementRoot.appendChild(record);
                                            }
                                            } else {
                                                while (rs.next() && (numberOfRecords > 0)) {
                                            	final Element record = normalDoc.createElementNS(NS,mRecordPrefix + "record"); //113494
                                                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                        final String colName = rsmd.getColumnName(j);
                                                        String colValue = rs.getString(j);

                                                        if (colName.equalsIgnoreCase(pkName) || ("\"" + colName + "\"").equalsIgnoreCase(pkName)) {
                                                            pKeyList.add(colValue);
                                                        }

                                                        final Element e = normalDoc.createElementNS(NS, XMLCharUtil.makeValidNCName(colName));
                                                        e.appendChild(normalDoc.createTextNode(colValue));
                                                    record.appendChild(e);
                                                    }

                                                elementRoot.appendChild(record);
                                                    numberOfRecords--;
                                                }
                                            } //else
                                            epb.setProcessList(pKeyList);
                                            wrapperBuilder.addPart(part.getName(), elementRoot);
                                        }
                                        normalDoc = wrapperBuilder.getResult();
                                    }
                                } else {
                                    final String msgEx = JDBCNormalizer.mMessages.getString(
                                            "SQLSE_E00703.JDBCN_Failed_NM_Part") +
                                        returnPartName + "in message " +
                                        msg.getQName();
                                    throw new MessagingException(msgEx);
                                }
                            } else {
                                final String msgEx = JDBCNormalizer.mMessages.getString(
                                        "SQLSE_E00704.JDBCN_Failed_NM_WS_OPER") +
                                    meta.getOperation().getName() +
                                    " is missing message in its <output>";
                                throw new MessagingException(msgEx);
                            }
                        } // while
                    } // if(input)
                    else {
                        final String msgEx = JDBCNormalizer.mMessages.getString(
                                "SQLSE_E00704.JDBCN_Failed_NM_WS_OPER") +
                            meta.getOperation().getName() +
                            " is missing <output> ";
                        throw new MessagingException(msgEx);
                    }
                } else {
                    final String operationName = meta.getBindingOperation().getName();
                    final Element normalRoot = normalDoc.createElement(operationName);
                    normalDoc.appendChild(normalRoot);

                    if (returnPartName != null) {
                        final Element returnElement = normalDoc.createElement(returnPartName);

                        // get resultset metadata rsmd and add it to the element
                        final ResultSetMetaData rsmd = rs.getMetaData();
    				 if (numberOfRecords == -1) {
    						while (rs.next()) {
    			                        	final Element record = normalDoc.createElement(mRecordPrefix + "record"); //113494
    							for (int j = 1; j <= rsmd.getColumnCount(); j++) {
    								final String colName = rsmd.getColumnName(j);
    								final String colValue = rs.getString(j);
    								final Element e = normalDoc.createElement(XMLCharUtil.makeValidNCName(colName));
    								e.appendChild(normalDoc.createTextNode(colValue));
    				                                record.appendChild(e);
    							}
    			                            returnElement.appendChild(record);
    						}
    					  } else {
                            while (rs.next() && (numberOfRecords > 0)) {
                            	final Element record = normalDoc.createElement(mRecordPrefix + "record"); //113494
                                for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                    final String colName = rsmd.getColumnName(j);
                                    String colValue = rs.getString(j);
                                    if (rs.wasNull()) {
                                        colValue = "";
                                    }
                                    final Element e = normalDoc.createElement(XMLCharUtil.makeValidNCName(colName));
                                    e.appendChild(normalDoc.createTextNode(colValue));
                                    record.appendChild(e);
                                }
                                returnElement.appendChild(record);
                                numberOfRecords--;
                            }
                        }
                        normalRoot.appendChild(returnElement);
                    }
                }
                if (JDBCNormalizer.mLogger.isLoggable(Level.INFO)) {
                    JDBCNormalizer.mLogger.log(Level.INFO, "normalized message", normalDoc);
                }
                normalMsg.setContent(new DOMSource(normalDoc));
            } catch (final ParserConfigurationException tex) {
                final String msg = JDBCNormalizer.mMessages.getString("SQLSE_E00705.JDBCN_Failed_NM_DOM");
                throw new MessagingException(msg, tex);
            } catch (final WrapperProcessingException ex) {
                final String exMsg = JDBCNormalizer.mMessages.getString("SQLSE_E00703.JDBCN_Failed_NM_Part") +
                    ex.getMessage();
                throw new MessagingException(exMsg, ex);
            }

            return normalMsg;
        }

    /**
     *
     * @param rs
     * @param exchange
     * @param meta
     * @return
     * @throws MessagingException
     * @throws SQLException
     * @throws ParserConfigurationException
     * @throws TransformerConfigurationException
     * @throws TransformerException
     */
    public NormalizedMessage normalizeSelect(final ResultSet rs,
        final MessageExchange exchange, final OperationMetaData meta)
        throws MessagingException, SQLException, ParserConfigurationException,
            TransformerConfigurationException, TransformerException {
        final NormalizedMessage normalMsg = exchange.createMessage();

        try {
            Document normalDoc = JDBCNormalizer.newDocument();
            String returnPartName = null;
            String NS = "";
            int numberOfRecords = meta.getJDBCOperationInput()
                                      .getNumberOfRecords();
            final JDBCOperationOutput jdbcOpOutput = meta.getJDBCOperationOutput();
            //113494 start
            mRecordPrefix = meta.getJDBCSql().getTableName();
            // see if the table name is null (SQL SE)
            // then take the query name as the prefix
            if (mRecordPrefix == null) {
                mRecordPrefix = meta.getJDBCOperationOutput().getName();
                mRecordPrefix = mRecordPrefix.substring(0, mRecordPrefix.indexOf("Response"));
            }
            //113494 end

            if (jdbcOpOutput != null) {
                returnPartName = jdbcOpOutput.getReturnPartName();

                if (returnPartName == null) {
                    final String msgEx = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                        "missing " + JDBCOperationOutput.ATTR_RETURN_PART_NAME +
                        " attribute in " +
                        JDBCConstants.QNAME_OPERATION_OUTPUT;
                    throw new MessagingException(msgEx);
                }
            } else {
                final String msgEx = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                    "missing " + JDBCConstants.QNAME_OPERATION_OUTPUT;
                throw new MessagingException(msgEx);
            }

            if (HelperFactory.WRAPPER_ENABLED) {
                String operationOutputName = null;
                Message msg = null;
                final Output output = meta.getOperation().getOutput();

                if (output != null) {
                    operationOutputName = output.getName();
                    msg = output.getMessage();

                    if (msg != null) {
                        wrapperBuilder.initialize(normalDoc, msg,
                            operationOutputName);

                        final Part part = msg.getPart(returnPartName);

                        if (part != null) {
                            final Element returnPartElement = normalDoc.createElement(returnPartName);
                            final QName type = part.getTypeName();
                            {
                                if (type != null) {
                                    // get resultset metadata rsmd and add it to the
                                    // element
                                    final ResultSetMetaData rsmd = rs.getMetaData();

                                    if (numberOfRecords == -1) {
                                        while (rs.next()) {
                                            final Element record = normalDoc.createElement(mRecordPrefix + "_Record"); //113494
                                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                final String colName = rsmd.getColumnName(j);
                                                final String colValue = rs.getString(j);
                                                final Element e = normalDoc.createElement(XMLCharUtil.makeValidNCName(colName));
                                                e.appendChild(normalDoc.createTextNode(colValue));
                                                record.appendChild(e);
                                            }
                                            returnPartElement.appendChild(record);
                                        }
                                    } else {
                                        while (rs.next() && (numberOfRecords > 0)) {
                                            final Element record = normalDoc.createElement(mRecordPrefix + "_Record"); //113494
                                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                final String colName = rsmd.getColumnName(j);
                                                String colValue = rs.getString(j);

                                                if (rs.wasNull()) {
                                                    colValue = "";
                                                }

                                                final Element e = normalDoc.createElement(XMLCharUtil.makeValidNCName(colName));
                                                e.appendChild(normalDoc.createTextNode(colValue));
                                                record.appendChild(e);
                                            }
                                            returnPartElement.appendChild(record);
                                            numberOfRecords--;
                                        }
                                    }

                                    wrapperBuilder.addPart(part.getName(), returnPartElement);
                                } else {
                                    final QName element = part.getElementName();
                                    NS = element.getNamespaceURI();

                                    final Element elementRoot = normalDoc.createElementNS(NS, element.getLocalPart());

                                    // returnPartElement.appendChild(elementRoot);
                                    // get resultset metadata rsmd and add it to the element
                                    final ResultSetMetaData rsmd = rs.getMetaData();

                                    if (numberOfRecords == -1) {
                                        while (rs.next()) {
                                            final Element record = normalDoc.createElementNS(NS,mRecordPrefix + "_Record"); //113494
                                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                final String colName = rsmd.getColumnName(j);
                                                String colValue = rs.getString(j);

                                                if (rs.wasNull()) {
                                                    colValue = "";
                                                }

                                                final Element e = normalDoc.createElementNS(NS, XMLCharUtil.makeValidNCName(colName));
                                                e.appendChild(normalDoc.createTextNode(colValue));
                                                record.appendChild(e);
                                            }
                                            elementRoot.appendChild(record);
                                        }
                                    } else {
                                        while (rs.next() && (numberOfRecords > 0)) {
                                            final Element record = normalDoc.createElementNS(NS,mRecordPrefix + "_Record"); //113494
                                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                final String colName = rsmd.getColumnName(j);
                                                String colValue = rs.getString(j);

                                                if (rs.wasNull()) {
                                                    colValue = "";
                                                }

                                                final Element e = normalDoc.createElementNS(NS, XMLCharUtil.makeValidNCName(colName));
                                                e.appendChild(normalDoc.createTextNode(colValue));
                                                record.appendChild(e);
                                                
                                            }
                                            elementRoot.appendChild(record);
                                            numberOfRecords--;
                                        }
                                    }

                                    wrapperBuilder.addPart(part.getName(), elementRoot);
                                }
                                normalDoc = wrapperBuilder.getResult();
                            }
                        } else {
                            final String msgEx = JDBCNormalizer.mMessages.getString(
                                    "SQLSE_E00703.JDBCN_Failed_NM_Part") + returnPartName +
                                "in message " + msg.getQName();
                            throw new MessagingException(msgEx);
                        }
                    } else {
                        final String msgEx = JDBCNormalizer.mMessages.getString(
                                "SQLSE_E00704.JDBCN_Failed_NM_WS_OPER") +
                            meta.getOperation().getName() +
                            " is missing message in its <output>";
                        throw new MessagingException(msgEx);
                    }
                } else {
                    final String msgEx = JDBCNormalizer.mMessages.getString(
                            "SQLSE_E00704.JDBCN_Failed_NM_WS_OPER") +
                        meta.getOperation().getName() +
                        " is missing <output> ";
                    throw new MessagingException(msgEx);
                }
            } else {
                final String operationName = meta.getBindingOperation().getName();
                final Element normalRoot = normalDoc.createElement(operationName);
                normalDoc.appendChild(normalRoot);

                if (returnPartName != null) {
                    final Element returnElement = normalDoc.createElement(returnPartName);

                    // get resultset metadata rsmd and add it to the element
                    final ResultSetMetaData rsmd = rs.getMetaData();

                    if (numberOfRecords == -1) {
                    	
                        while (rs.next()) {
                            final Element record = normalDoc.createElement(mRecordPrefix + "_Record"); //113494
                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                final String colName = rsmd.getColumnName(j);
                                String colValue = rs.getString(j);

                                if (rs.wasNull()) {
                                    colValue = "";
                                }

                                final Element e = normalDoc.createElement(XMLCharUtil.makeValidNCName(colName));
                                e.appendChild(normalDoc.createTextNode(colValue));
                                record.appendChild(e);
                            }
                            returnElement.appendChild(record);
                        }
                    } else {
                        while (rs.next() && (numberOfRecords > 0)) {
                        	final Element record = normalDoc.createElement("record");
                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                final String colName = rsmd.getColumnName(j);
                                String colValue = rs.getString(j);

                                if (rs.wasNull()) {
                                    colValue = "";
                                }

                                final Element e = normalDoc.createElement(XMLCharUtil.makeValidNCName(colName));
                                e.appendChild(normalDoc.createTextNode(colValue));
                                record.appendChild(e);
                                
                            }
                            returnElement.appendChild(record);
                            numberOfRecords--;
                        }
                    }

                    normalRoot.appendChild(returnElement);
                }
            }

            if (JDBCNormalizer.mLogger.isLoggable(Level.INFO)) {
                JDBCNormalizer.mLogger.log(Level.INFO, "normalized message", normalDoc);
            }

            normalMsg.setContent(new DOMSource(normalDoc));
        } catch (final ParserConfigurationException tex) {
            final String msg = JDBCNormalizer.mMessages.getString("SQLSE_E00704.JDBCN_Failed_NM_WS_OPER");
            throw new MessagingException(msg, tex);
        } catch (final WrapperProcessingException ex) {
            final String exMsg = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                ex.getMessage();
            throw new MessagingException(exMsg, ex);
        }

        return normalMsg;
    }

    /**
    * @param cs 
     * @param exchange
    * @param meta
    * @return
    * @throws MessagingException
    * @throws SQLException
    * @throws ParserConfigurationException
    * @throws TransformerConfigurationException
    * @throws TransformerException
    */
   public NormalizedMessage normalizeProcedure(final CallableStatement cs,
       final MessageExchange exchange, final OperationMetaData meta)
       throws MessagingException, SQLException, ParserConfigurationException,
           TransformerConfigurationException, TransformerException {
       final NormalizedMessage normalMsg = exchange.createMessage();

       try {
           Document normalDoc = JDBCNormalizer.newDocument();
           String returnPartName = null;
           String NS = "";
           int numberOfRecords = meta.getJDBCOperationInput()
                                     .getNumberOfRecords();
           final JDBCOperationOutput jdbcOpOutput = meta.getJDBCOperationOutput();
           if (jdbcOpOutput != null) {
               returnPartName = jdbcOpOutput.getReturnPartName();

               if (returnPartName == null) {
                   final String msgEx = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                       "missing " + JDBCOperationOutput.ATTR_RETURN_PART_NAME +
                       " attribute in " +
                       JDBCConstants.QNAME_OPERATION_OUTPUT;
                   throw new MessagingException(msgEx);
               }
           } else {
               final String msgEx = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
                   "missing " + JDBCConstants.QNAME_OPERATION_OUTPUT;
               throw new MessagingException(msgEx);
           }

           if (HelperFactory.WRAPPER_ENABLED) {
               String operationOutputName = null;
               Message msg = null;
               final Output output = meta.getOperation().getOutput();

               if (output != null) {
                   operationOutputName = output.getName();
                   msg = output.getMessage();

                   if (msg != null) {
                       wrapperBuilder.initialize(normalDoc, msg,
                           operationOutputName);

                       final Part part = msg.getPart(returnPartName);

                       if (part != null) {
                           final Element returnPartElement = normalDoc.createElement(returnPartName);
                           final QName type = part.getTypeName();
                            {
                                if (type != null) {
                                    // get resultset metadata rsmd and add it to the
                                    // element
                                    for(int i =0; i < outParamIndex.size();i++) {
                                    	int paramIndex = (Integer)outParamIndex.get(i);
                                    	String paramType = outParamTypes.get(paramIndex);
                                    	String paramName = outParamNames.get(paramIndex);
                                    	if(paramType.equalsIgnoreCase("REF CURSOR") || paramType.equalsIgnoreCase("RESULTSET")){
                                        ResultSet rs = (ResultSet)cs.getObject(paramIndex);
                                        final ResultSetMetaData rsmd = rs.getMetaData();
                                        while (rs.next()) {
                                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                                                final String colName = rsmd.getColumnName(j);
                                                final String colValue = rs.getString(j);
                                                final Element e = normalDoc.createElement(colName);
                                                e.appendChild(normalDoc.createTextNode(colValue));
                                                returnPartElement.appendChild(e);
                                            }
                                        }
                                    } else{
                                    	Object paramValue =  cs.getObject(paramIndex);
                                    	final Element e = normalDoc.createElement(paramName);
                                        e.appendChild(normalDoc.createTextNode(paramValue.toString()));
                                        returnPartElement.appendChild(e);
                                    }
                                    	
                                    }
                                    wrapperBuilder.addPart(part.getName(), returnPartElement);
                                } else {
                                    final QName element = part.getElementName();
                                    NS = element.getNamespaceURI();

                                    final Element elementRoot = normalDoc.createElementNS(NS, element.getLocalPart());

                                    // returnPartElement.appendChild(elementRoot);
                                    // get resultset metadata rsmd and add it to the element
                                    for(int i =0; i < outParamIndex.size();i++) {
                                    	int paramIndex = (Integer)outParamIndex.get(i);
                                    	String paramType = outParamTypes.get(paramIndex);
                                    	String paramName = outParamNames.get(paramIndex);
                                    	if(paramType.equalsIgnoreCase("REF CURSOR") || paramType.equalsIgnoreCase("RESULTSET")){
	                                        ResultSet rs = (ResultSet)cs.getObject(paramIndex);
	                                        final ResultSetMetaData rsmd = rs.getMetaData();
	
	                                        while (rs.next()) {
	                                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
	                                                final String colName = rsmd.getColumnName(j);
	                                                String colValue = rs.getString(j);
	
	                                                if (rs.wasNull()) {
	                                                    colValue = "";
	                                                }
	
	                                                final Element e = normalDoc.createElementNS(NS, colName);
	                                                e.appendChild(normalDoc.createTextNode(colValue));
	                                                elementRoot.appendChild(e);
	                                            }
	                                        }
                                	}else{
                                    	Object paramValue =  cs.getObject(paramIndex);
                                    	final Element e = normalDoc.createElement(paramName);
                                        e.appendChild(normalDoc.createTextNode(paramValue.toString()));
                                        returnPartElement.appendChild(e);
                                	}
                                    }
                                    wrapperBuilder.addPart(part.getName(), elementRoot);
                                }
                                normalDoc = wrapperBuilder.getResult();
                            }
                       } else {
                           final String msgEx = JDBCNormalizer.mMessages.getString(
                                   "SQLSE_E00703.JDBCN_Failed_NM_Part") + returnPartName +
                               "in message " + msg.getQName();
                           throw new MessagingException(msgEx);
                       }
                   } else {
                       final String msgEx = JDBCNormalizer.mMessages.getString(
                               "SQLSE_E00704.JDBCN_Failed_NM_WS_OPER") +
                           meta.getOperation().getName() +
                           " is missing message in its <output>";
                       throw new MessagingException(msgEx);
                   }
               } else {
                   final String msgEx = JDBCNormalizer.mMessages.getString(
                           "SQLSE_E00704.JDBCN_Failed_NM_WS_OPER") +
                       meta.getOperation().getName() +
                       " is missing <output> ";
                   throw new MessagingException(msgEx);
               }
           } else {
               final String operationName = meta.getBindingOperation().getName();
               final Element normalRoot = normalDoc.createElement(operationName);
               normalDoc.appendChild(normalRoot);

               if (returnPartName != null) {
                   final Element returnElement = normalDoc.createElement(returnPartName);

                   // get resultset metadata rsmd and add it to the element
                   for(int i =0; i < outParamIndex.size();i++) {
                   	int paramIndex = (Integer)outParamIndex.get(i);
                   	String paramType = outParamTypes.get(paramIndex);
                   	String paramName = outParamNames.get(paramIndex);
                   	if(paramType.equalsIgnoreCase("REF CURSOR") || paramType.equalsIgnoreCase("RESULTSET")){
                       ResultSet rs = (ResultSet)cs.getObject(paramIndex);
                       final ResultSetMetaData rsmd = rs.getMetaData();
                   
                       while (rs.next()) {
                           for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                               final String colName = rsmd.getColumnName(j);
                               String colValue = rs.getString(j);

                               if (rs.wasNull()) {
                                   colValue = "";
                               }

                               final Element e = normalDoc.createElement(colName);
                               e.appendChild(normalDoc.createTextNode(colValue));
                               returnElement.appendChild(e);
                           }
                       }
                   }else{
                   	Object paramValue =  cs.getObject(paramIndex);
                	final Element e = normalDoc.createElement(paramName);
                    e.appendChild(normalDoc.createTextNode(paramValue.toString()));
                    returnElement.appendChild(e);
                   }
                } 
                   normalRoot.appendChild(returnElement);
               }
           }

           if (JDBCNormalizer.mLogger.isLoggable(Level.INFO)) {
               JDBCNormalizer.mLogger.log(Level.INFO, "normalized message", normalDoc);
           }

           normalMsg.setContent(new DOMSource(normalDoc));
       } catch (final ParserConfigurationException tex) {
           final String msg = JDBCNormalizer.mMessages.getString("SQLSE_E00704.JDBCN_Failed_NM_WS_OPER");
           throw new MessagingException(msg, tex);
       } catch (final WrapperProcessingException ex) {
           final String exMsg = JDBCNormalizer.mMessages.getString("SQLSE_E00702.JDBCN_Failed_NM") +
               ex.getMessage();
           throw new MessagingException(exMsg, ex);
       }

       return normalMsg;
   }	
    
    
    
    
    /**
     *
     * @return
     * @throws ParserConfigurationException
     */
    private static final Document newDocument() throws ParserConfigurationException {
        if (JDBCNormalizer.mBuilder == null) {
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            JDBCNormalizer.mBuilder = factory.newDocumentBuilder();
        }

        return JDBCNormalizer.mBuilder.newDocument();
    }
    
    protected void setOutParamIndex( ArrayList outParamIndex){
    	this.outParamIndex = outParamIndex;
    }
    
    protected void setOutParamTypes(HashMap outParamTypes ){
    	this.outParamTypes = outParamTypes;
    }
    
    protected  void setOutParamNames(HashMap outParamNames){
    	this.outParamNames = outParamNames;
    }
    
}
