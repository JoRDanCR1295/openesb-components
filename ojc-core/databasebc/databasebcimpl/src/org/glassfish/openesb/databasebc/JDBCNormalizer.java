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
package org.glassfish.openesb.databasebc;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.ParameterMetaData;
import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Connection;
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
import org.glassfish.openesb.databasebc.extensions.JDBCConstants;
import org.glassfish.openesb.databasebc.extensions.JDBCOperationInput;
import org.glassfish.openesb.databasebc.extensions.JDBCOperationOutput;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import org.glassfish.openesb.databasebc.extensions.SPOperationOutput;
import org.glassfish.openesb.databasebc.util.XMLCharUtil;
import org.glassfish.openesb.databasebc.util.DebugLog;

/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCNormalizer {

  private static final Messages mMessages = Messages.getMessages(
          JDBCNormalizer.class);
  private static final Logger mLogger = Messages.getLogger(
          JDBCNormalizer.class);
  private static DocumentBuilder mBuilder = null;
  private WrapperBuilder wrapperBuilder;
  private ArrayList outParamIndex = new ArrayList();
  private HashMap<Integer, String> outParamTypes =
          new HashMap<Integer, String>();
  private HashMap<Integer, String> outParamNames =
          new HashMap<Integer, String>();
  private String mRecordPrefix = null; //113494
  private boolean status;
  private Connection connection = null;
  private String insertQuery = null;
  public ArrayList mProcessedList = new ArrayList();
  public Map mInboundExchangeProcessRecordsMap = new HashMap();
  public int mRowCount = 0;
  private JDBCClusterManager mJDBCClusterManager;

  /** Creates a new instance of SoapNormalizer
   * @throws javax.jbi.messaging.MessagingException
   */
  public JDBCNormalizer() throws MessagingException {
    try {
      wrapperBuilder = HelperFactory.createBuilder();
    } catch (final WrapperProcessingException ex) {
      throw new MessagingException(JDBCNormalizer.mMessages.getString(
              "DBBC_E00701.JDBCN_Failed_Create") + ex.getMessage(), ex);
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
  /*
   * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
   */
  public NormalizedMessage normalize(final String outputValue,
                                     final MessageExchange exchange, final OperationMetaData meta)
          throws MessagingException {
    final NormalizedMessage normalMsg = exchange.createMessage();
    mRowCount = 0;

    try {
      Document normalDoc = JDBCNormalizer.newDocument();
      String returnPartName = null;
      final JDBCOperationOutput jdbcOpOutput =
              meta.getJDBCOperationOutput();

      if (jdbcOpOutput != null) {
        returnPartName = jdbcOpOutput.getReturnPartName();

        if (returnPartName == null) {
          final String msgEx = JDBCNormalizer.mMessages.getString(
                  "DBBC_E00702.JDBCN_Failed_NM") +
                  "missing " + JDBCOperationOutput.ATTR_RETURN_PART_NAME +
                  " attribute in " +
                  JDBCConstants.QNAME_OPERATION_OUTPUT;
          throw new MessagingException(msgEx);
        }
      } else {
        final String msgEx = JDBCNormalizer.mMessages.getString(
                "DBBC_E00702.JDBCN_Failed_NM") +
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
              final Element returnPartElement = normalDoc.createElement(
                      returnPartName);
              /*
               * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
               */
              final Text text = normalDoc.createTextNode("" +
                      outputValue);

              final QName type = part.getTypeName();
              {
                if (type != null) {
                  returnPartElement.appendChild(text);
                  wrapperBuilder.addPart(part.getName(),
                          returnPartElement);
                } else {
                  final QName element = part.getElementName();
                  final Element elementRoot = normalDoc.createElementNS(element.
                          getNamespaceURI(), element.getLocalPart());
                  returnPartElement.appendChild(elementRoot);

                  elementRoot.appendChild(text);
                  wrapperBuilder.addPart(part.getName(),
                          elementRoot);
                }
                normalDoc = wrapperBuilder.getResult();
              }
            } else {
              final String msgEx = JDBCNormalizer.mMessages.getString(
                      "DBBC_E00703.JDBCN_Failed_NM_Part") + returnPartName +
                      "in message " + msg.getQName();
              throw new MessagingException(msgEx);
            }
          } else {
            final String msgEx = JDBCNormalizer.mMessages.getString(
                    "DBBC_E00704.JDBCN_Failed_NM_WS_OPER") +
                    meta.getOperation().getName() +
                    " is missing message in its <output>";
            throw new MessagingException(msgEx);
          }
        } else {
          final String msgEx = JDBCNormalizer.mMessages.getString(
                  "DBBC_E00704.JDBCN_Failed_NM_WS_OPER") +
                  meta.getOperation().getName() +
                  " is missing <output> ";
          throw new MessagingException(msgEx);
        }
      } else {
        final String operationName =
                meta.getBindingOperation().getName();
        final Element normalRoot =
                normalDoc.createElement(operationName);
        normalDoc.appendChild(normalRoot);

        if (returnPartName != null) {
          final Element returnElement = normalDoc.createElement(
                  returnPartName);
          /*
           * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
           */
          final Text text = normalDoc.createTextNode("" + outputValue);
          returnElement.appendChild(text);
          normalRoot.appendChild(returnElement);
        }
      }

      if (JDBCNormalizer.mLogger.isLoggable(Level.INFO))
        JDBCNormalizer.mLogger.log(Level.INFO, "normalized message",
                normalDoc);

      normalMsg.setContent(new DOMSource(normalDoc));
    } catch (final ParserConfigurationException tex) {
      final String msg = JDBCNormalizer.mMessages.getString(
              "DBBC_E00705.JDBCN_Failed_NM_DOM");
      throw new MessagingException(msg, tex);
    } catch (final WrapperProcessingException ex) {
      final String exMsg = JDBCNormalizer.mMessages.getString(
              "DBBC_E00702.JDBCN_Failed_NM") + ex.getMessage();
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
                                                  final String pkName, final String dbName)
          throws MessagingException, SQLException, ParserConfigurationException,
          TransformerConfigurationException, TransformerException {
    final NormalizedMessage normalMsg = exchange.createMessage();

    //113494  start
    mRecordPrefix = meta.getJDBCSql().getTableName();
    // see if the table name is null (SQL SE)
    // then take the query name as the prefix
    if (mRecordPrefix == null) {
      mRecordPrefix = meta.getJDBCOperationOutput().getName();
      mRecordPrefix = mRecordPrefix.substring(0, mRecordPrefix.indexOf(
              "Response"));
    }
    //113494  end

    try {
      Document normalDoc = JDBCNormalizer.newDocument();
      String returnPartName = null;
      String NS = "";
      int numberOfRecords = meta.getJDBCOperationInput().
              getNumberOfRecords();


      QName elementQName = null;
      final JDBCOperationInput jdbcOpInput = meta.getJDBCOperationInput();

      if (jdbcOpInput == null) {
        final String msgEx = JDBCNormalizer.mMessages.getString(
                "DBBC_E00702.JDBCN_Failed_NM") +
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
                      "DBBC_E00703.JDBCN_Failed_NM_Part") + part.getName() +
                      "should have element attribute defined.";
              throw new MessagingException(msgEx);
            }

            returnPartName = elementQName.getLocalPart();

            if (returnPartName == null) {
              final String msgEx = JDBCNormalizer.mMessages.getString(
                      "DBBC_E00702.JDBCN_Failed_NM") + "missing "// +
                      // JDBCOperationInput.ATTR_RETURN_PART_NAME
                      // + " attribute in "
                      + JDBCConstants.QNAME_OPERATION_INPUT;
              throw new MessagingException(msgEx);
            }

            /* End of inbound */
            if (msg != null) {
              wrapperBuilder.initialize(normalDoc, msg,
                      operationInputName);

              if (part != null) {
                final Element returnPartElement = normalDoc.createElement(
                        returnPartName);
                final QName type = part.getTypeName();
                if (type != null) {
                  // get resultset metadata rsmd and add it to the element
                  final ResultSetMetaData rsmd = rs.getMetaData();
                  while (rs.next()) {
                    mRowCount++;
                    if (mLogger.isLoggable(
                            Level.FINE))
                      mLogger.log(Level.FINE,
                              "DBBC_R00706.JDBCN_ProcessNextRecord");
                    final Element record =
                            normalDoc.createElement(
                            mRecordPrefix + "_Record"); //113494
                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                      final String colName = rsmd.getColumnName(j);
                      String colValue = JDBCUtil.convertToString(j,
                              rs, rsmd, dbName);

                      final Element e = normalDoc.createElement(
                              XMLCharUtil.makeValidNCName(
                              colName));
                      if (rs.wasNull()) {
                        colValue = "";
                        e.setAttribute("isNull",
                                "true");
                      } else
                        e.setAttribute("isNull",
                                "false");
                      e.appendChild(normalDoc.createTextNode(
                              colValue));
                      record.appendChild(e);
                      if (mLogger.isLoggable(
                              Level.FINEST))
                        mLogger.log(Level.FINEST,
                                "Col Name == " + colName + " and Col Val == " + colValue);
                    }
                    returnPartElement.appendChild(
                            record);
                    if (numberOfRecords > 0) {
                      numberOfRecords--;
                      if (numberOfRecords == 0)
                        break;
                    }
                  }
                  wrapperBuilder.addPart(part.getName(),
                          returnPartElement);
                } else {
                  final QName element = part.getElementName();
                  NS = element.getNamespaceURI();

                  final Element elementRoot = normalDoc.createElementNS(NS,
                          element.getLocalPart());

                  // returnPartElement.appendChild(elementRoot);
                  // get resultset metadata rsmd and add it to the element
                  final ResultSetMetaData rsmd = rs.getMetaData();
                  final List<String> pKeyList =
                          new ArrayList<String>();

                  while (rs.next()) {
                    mRowCount++;
                    if (mLogger.isLoggable(
                            Level.FINE))
                      mLogger.log(Level.FINE,
                              "DBBC_R00706.JDBCN_ProcessNextRecord");
                    Element record =
                            normalDoc.createElementNS(NS,
                            mRecordPrefix + "_Record"); //113494
                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                      final String colName = rsmd.getColumnName(j);
                      String colValue = JDBCUtil.convertToString(j,
                              rs, rsmd, dbName);

                      if (colName.equalsIgnoreCase(
                              pkName) || ("\"" + colName + "\"").
                              equalsIgnoreCase(
                              pkName))
                        if (epb.isClustered()) {
                          boolean inserted =
                                  false;
                          mJDBCClusterManager.setPKValue(
                                  colValue);
                          inserted =
                                  mJDBCClusterManager.
                                  isRecordInsertedByCurrentInstance();
                          if (!inserted)
                            record = null;
                          else
                            pKeyList.add(
                                    colValue);
                        } else {
                          boolean processed =
                                  isRecordProcessed(
                                  colValue);
                          if (!processed)
                            pKeyList.add(
                                    colValue);
                          else
                            record = null;
                        }
                      if (record != null) {
                        final Element e =
                                normalDoc.createElementNS(
                                NS, XMLCharUtil.makeValidNCName(
                                colName));
                        if (rs.wasNull()) {
                          colValue = "";
                          e.setAttribute(
                                  "isNull",
                                  "true");
                        } else
                          e.setAttribute(
                                  "isNull",
                                  "false");
                        e.appendChild(normalDoc.createTextNode(
                                colValue));
                        record.appendChild(e);
                        if (mLogger.isLoggable(
                                Level.FINEST))
                          mLogger.log(
                                  Level.FINEST,
                                  "Col Name == " + colName + " and Col Val == " + colValue);
                      } else
                        break;
                    }
                    if (record != null) {
                      elementRoot.appendChild(
                              record);
                      if (numberOfRecords > 0) {
                        numberOfRecords--;
                        if (numberOfRecords == 0)
                          break;
                      }
                    }
                  } // while
                  epb.setProcessList(pKeyList);
                  if (pKeyList.size() != 0)
                    mInboundExchangeProcessRecordsMap.put(exchange.
                            getExchangeId(),
                            pKeyList);
                  wrapperBuilder.addPart(part.getName(),
                          elementRoot);
                }
                normalDoc = wrapperBuilder.getResult();
              } else {
                final String msgEx = JDBCNormalizer.mMessages.getString(
                        "DBBC_E00703.JDBCN_Failed_NM_Part") +
                        returnPartName + "in message " +
                        msg.getQName();
                throw new MessagingException(msgEx);
              }
            } else {
              final String msgEx = JDBCNormalizer.mMessages.getString(
                      "DBBC_E00704.JDBCN_Failed_NM_WS_OPER") +
                      meta.getOperation().getName() +
                      " is missing message in its <output>";
              throw new MessagingException(msgEx);
            }
          } // while
        } // if(input)
        else {
          final String msgEx = JDBCNormalizer.mMessages.getString(
                  "DBBC_E00704.JDBCN_Failed_NM_WS_OPER") +
                  meta.getOperation().getName() +
                  " is missing <output> ";
          throw new MessagingException(msgEx);
        }
      } else {
        final String operationName =
                meta.getBindingOperation().getName();
        final Element normalRoot =
                normalDoc.createElement(operationName);
        normalDoc.appendChild(normalRoot);

        if (returnPartName != null) {
          final Element returnElement = normalDoc.createElement(
                  returnPartName);

          // get resultset metadata rsmd and add it to the element
          final ResultSetMetaData rsmd = rs.getMetaData();
          while (rs.next()) {
            mRowCount++;
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.FINE,
                      "DBBC_R00706.JDBCN_ProcessNextRecord");
            final Element record = normalDoc.createElement(
                    mRecordPrefix + "_Record"); //113494
            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
              final String colName = rsmd.getColumnName(j);
              String colValue =
                      JDBCUtil.convertToString(j, rs, rsmd,
                      dbName);

              final Element e = normalDoc.createElement(
                      XMLCharUtil.makeValidNCName(colName));

              if (rs.wasNull()) {
                colValue = "";
                e.setAttribute("isNull", "true");
              } else
                e.setAttribute("isNull", "false");
              e.appendChild(normalDoc.createTextNode(colValue));
              record.appendChild(e);
              if (mLogger.isLoggable(Level.FINEST))
                mLogger.log(Level.FINEST,
                        "Col Name == " + colName + " and Col Val == " + colValue);
            }
            returnElement.appendChild(record);
            if (numberOfRecords > 0) {
              numberOfRecords--;
              if (numberOfRecords == 0)
                break;
            }
          }
          normalRoot.appendChild(returnElement);
        }
      }

      JDBCNormalizer.mLogger.log(Level.INFO, "normalized message",
              normalDoc);
      if (JDBCNormalizer.mLogger.isLoggable(Level.FINE))
        DebugLog.debugLog(mLogger, Level.INFO,
                "***normalizeSelectInbound***", normalDoc);
      normalMsg.setContent(new DOMSource(normalDoc));
    } catch (final ParserConfigurationException tex) {
      final String msg = JDBCNormalizer.mMessages.getString(
              "DBBC_E00705.JDBCN_Failed_NM_DOM");
      throw new MessagingException(msg, tex);
    } catch (final WrapperProcessingException ex) {
      final String exMsg = JDBCNormalizer.mMessages.getString(
              "DBBC_E00703.JDBCN_Failed_NM_Part") + ex.getMessage();
      throw new MessagingException(exMsg, ex);
    } catch (final IOException ex) {
      final String exMsg = JDBCNormalizer.mMessages.getString(
              "DBBC_R00707.JDBCN_Failed_Base64_Encode");
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
                                           final MessageExchange exchange, final OperationMetaData meta, String driverName)
          throws MessagingException, SQLException, ParserConfigurationException,
          TransformerConfigurationException, TransformerException {
    final NormalizedMessage normalMsg = exchange.createMessage();

    try {
      Document normalDoc = JDBCNormalizer.newDocument();
      String returnPartName = null;
      String NS = "";
      int numberOfRecords = meta.getJDBCOperationInput().
              getNumberOfRecords();
      final JDBCOperationOutput jdbcOpOutput =
              meta.getJDBCOperationOutput();
      //113494 start
      mRecordPrefix = meta.getJDBCSql().getTableName();
      // see if the table name is null (SQL SE)
      // then take the query name as the prefix
      if (mRecordPrefix == null) {
        mRecordPrefix = meta.getJDBCOperationOutput().getName();
        mRecordPrefix = mRecordPrefix.substring(0,
                mRecordPrefix.indexOf("Response"));
      }
      //113494 end

      if (jdbcOpOutput != null) {
        returnPartName = jdbcOpOutput.getReturnPartName();

        if (returnPartName == null) {
          final String msgEx = JDBCNormalizer.mMessages.getString(
                  "DBBC_E00702.JDBCN_Failed_NM") +
                  "missing " + JDBCOperationOutput.ATTR_RETURN_PART_NAME +
                  " attribute in " +
                  JDBCConstants.QNAME_OPERATION_OUTPUT;
          throw new MessagingException(msgEx);
        }
      } else {
        final String msgEx = JDBCNormalizer.mMessages.getString(
                "DBBC_E00702.JDBCN_Failed_NM") +
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
              final Element returnPartElement = normalDoc.createElement(
                      returnPartName);
              final QName type = part.getTypeName();
              {
                if (type != null) {
                  // get resultset metadata rsmd and add it to the
                  // element
                  final ResultSetMetaData rsmd = rs.getMetaData();
                  String[] validColNames = new String[rsmd.getColumnCount()];
                  validColNames = validateColumnNames(rsmd,
                          validColNames);

                  while (rs.next()) {
                    mRowCount++;
                    if (mLogger.isLoggable(Level.FINE))
                      mLogger.log(Level.FINE,
                              "DBBC_R00706.JDBCN_ProcessNextRecord");
                    final Element record = normalDoc.createElement(
                            mRecordPrefix + "_Record"); //113494
                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                      final String colName =
                              validColNames[j - 1];
                      String colValue = JDBCUtil.convertToString(j, rs,
                              rsmd, driverName);
                      final Element e =
                              normalDoc.createElement(XMLCharUtil.
                              makeValidNCName(colName));
                      if (rs.wasNull()) {
                        colValue = "";
                        e.setAttribute("isNull",
                                "true");
                      } else
                        e.setAttribute("isNull",
                                "false");
                      e.appendChild(normalDoc.createTextNode(colValue));
                      record.appendChild(e);
                      if (mLogger.isLoggable(
                              Level.FINEST))
                        mLogger.log(Level.FINEST,
                                "Col Name == " + colName + " and Col Val == " + colValue);
                    }
                    returnPartElement.appendChild(record);
                    if (numberOfRecords > 0) {
                      numberOfRecords--;
                      if (numberOfRecords == 0)
                        break;
                    }
                  }

                  wrapperBuilder.addPart(part.getName(),
                          returnPartElement);
                } else {
                  final QName element = part.getElementName();
                  NS = element.getNamespaceURI();

                  final Element elementRoot = normalDoc.createElementNS(NS,
                          element.getLocalPart());

                  // returnPartElement.appendChild(elementRoot);
                  // get resultset metadata rsmd and add it to the element
                  final ResultSetMetaData rsmd = rs.getMetaData();
                  String[] validColNames = new String[rsmd.getColumnCount()];
                  validColNames = validateColumnNames(rsmd,
                          validColNames);

                  while (rs.next()) {
                    mRowCount++;
                    if (mLogger.isLoggable(Level.FINE))
                      mLogger.log(Level.FINE,
                              "DBBC_R00706.JDBCN_ProcessNextRecord");
                    final Element record = normalDoc.createElementNS(NS,
                            mRecordPrefix + "_Record"); //113494
                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                      final String colName =
                              validColNames[j - 1];
                      String colValue = JDBCUtil.convertToString(j, rs,
                              rsmd, driverName);


                      final Element e =
                              normalDoc.createElementNS(NS,
                              XMLCharUtil.makeValidNCName(colName));
                      if (rs.wasNull()) {
                        colValue = "";
                        e.setAttribute("isNull",
                                "true");
                      } else
                        e.setAttribute("isNull",
                                "false");
                      e.appendChild(normalDoc.createTextNode(colValue));
                      record.appendChild(e);
                      if (mLogger.isLoggable(
                              Level.FINEST))
                        mLogger.log(Level.FINEST,
                                "Col Name == " + colName + " and Col Val == " + colValue);
                    }
                    elementRoot.appendChild(record);
                    if (numberOfRecords > 0) {
                      numberOfRecords--;
                      if (numberOfRecords == 0)
                        break;
                    }
                  }

                  wrapperBuilder.addPart(part.getName(),
                          elementRoot);
                }
                normalDoc = wrapperBuilder.getResult();
              }
            } else {
              final String msgEx = JDBCNormalizer.mMessages.getString(
                      "DBBC_E00703.JDBCN_Failed_NM_Part") + returnPartName +
                      "in message " + msg.getQName();
              throw new MessagingException(msgEx);
            }
          } else {
            final String msgEx = JDBCNormalizer.mMessages.getString(
                    "DBBC_E00704.JDBCN_Failed_NM_WS_OPER") +
                    meta.getOperation().getName() +
                    " is missing message in its <output>";
            throw new MessagingException(msgEx);
          }
        } else {
          final String msgEx = JDBCNormalizer.mMessages.getString(
                  "DBBC_E00704.JDBCN_Failed_NM_WS_OPER") +
                  meta.getOperation().getName() +
                  " is missing <output> ";
          throw new MessagingException(msgEx);
        }
      } else {
        final String operationName =
                meta.getBindingOperation().getName();
        final Element normalRoot =
                normalDoc.createElement(operationName);
        normalDoc.appendChild(normalRoot);

        if (returnPartName != null) {
          final Element returnElement = normalDoc.createElement(
                  returnPartName);

          // get resultset metadata rsmd and add it to the element
          final ResultSetMetaData rsmd = rs.getMetaData();
          String[] validColNames = new String[rsmd.getColumnCount()];
          validColNames = validateColumnNames(rsmd, validColNames);

          while (rs.next()) {
            mRowCount++;
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.FINE,
                      "DBBC_R00706.JDBCN_ProcessNextRecord");
            final Element record = normalDoc.createElement(
                    mRecordPrefix + "_Record"); //113494
            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
              final String colName = validColNames[j - 1];
              String colValue =
                      JDBCUtil.convertToString(j, rs, rsmd,
                      driverName);

              final Element e = normalDoc.createElement(
                      XMLCharUtil.makeValidNCName(colName));
              if (rs.wasNull()) {
                colValue = "";
                e.setAttribute("isNull", "true");
              } else
                e.setAttribute("isNull", "false");
              e.appendChild(normalDoc.createTextNode(colValue));
              record.appendChild(e);
              if (mLogger.isLoggable(Level.FINEST))
                mLogger.log(Level.FINEST,
                        "Col Name == " + colName + " and Col Val == " + colValue);
            }
            returnElement.appendChild(record);
            if (numberOfRecords > 0) {
              numberOfRecords--;
              if (numberOfRecords == 0)
                break;
            }
          }

          normalRoot.appendChild(returnElement);
        }
      }

      if (JDBCNormalizer.mLogger.isLoggable(Level.INFO))
        JDBCNormalizer.mLogger.log(Level.INFO, "normalized message",
                normalDoc);

      normalMsg.setContent(new DOMSource(normalDoc));
    } catch (final ParserConfigurationException tex) {
      final String msg = JDBCNormalizer.mMessages.getString(
              "DBBC_E00704.JDBCN_Failed_NM_WS_OPER");
      throw new MessagingException(msg, tex);
    } catch (final WrapperProcessingException ex) {
      final String exMsg = JDBCNormalizer.mMessages.getString(
              "DBBC_E00702.JDBCN_Failed_NM") + ex.getMessage();
      throw new MessagingException(exMsg, ex);
    } catch (final IOException ex) {
      final String exMsg = JDBCNormalizer.mMessages.getString(
              "DBBC_R00707.JDBCN_Failed_Base64_Encode");
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
                                              final MessageExchange exchange, final OperationMetaData meta, final String dbName)
          throws MessagingException, SQLException, ParserConfigurationException,
          TransformerConfigurationException, TransformerException {
    final NormalizedMessage normalMsg = exchange.createMessage();

    try {
      Document normalDoc = JDBCNormalizer.newDocument();
      String returnPartName = null;
      String NS = "";
      final SPOperationOutput jdbcOpOutput =
              meta.getJDBCSPOperationOutput();
      if (jdbcOpOutput != null) {
        returnPartName = jdbcOpOutput.getReturnPartName();

        if (returnPartName == null) {
          final String msgEx = JDBCNormalizer.mMessages.getString(
                  "DBBC_E00702.JDBCN_Failed_NM") +
                  "missing " + JDBCOperationOutput.ATTR_RETURN_PART_NAME +
                  " attribute in " +
                  JDBCConstants.QNAME_OPERATION_OUTPUT;
          throw new MessagingException(msgEx);
        }
      } else {
        final String msgEx = JDBCNormalizer.mMessages.getString(
                "DBBC_E00702.JDBCN_Failed_NM") +
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
              final Element returnPartElement = normalDoc.createElement(
                      returnPartName);
              final QName type = part.getTypeName();
              {
                if (type != null) {
                  // get resultset metadata rsmd and add it to the
                  // element
                  for (int i = 0; i < outParamIndex.size(); i++) {
                    int paramIndex =
                            (Integer) outParamIndex.get(i);
                    String paramType = outParamTypes.get(
                            paramIndex);
                    String paramName = outParamNames.get(
                            paramIndex);
                    ResultSet rs = null;
                    try {
                      if (paramType.equalsIgnoreCase(
                              "REF CURSOR") || paramType.equalsIgnoreCase(
                              "RESULTSET")) {
                        if (dbName.trim().contains(
                                "sql server") || dbName.trim().contains(
                                "adaptive server"))
                          rs = (ResultSet) cs.getResultSet();
                        else
                          rs =
                                  (ResultSet) cs.getObject(paramIndex);
                        if (rs != null) {
                          final ResultSetMetaData rsmd =
                                  rs.getMetaData();
                          while (rs.next()) {
                            mRowCount++;
                            if (mLogger.isLoggable(
                                    Level.FINE))
                              mLogger.log(
                                      Level.FINE,
                                      "DBBC_R00706.JDBCN_ProcessNextRecord");
                            Element paramElem =
                                    normalDoc.createElement(
                                    paramName + "_0_Resultset");
                            returnPartElement.appendChild(
                                    paramElem);

                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                              final String colName =
                                      rsmd.getColumnName(
                                      j);
                              String colValue =
                                      JDBCUtil.convertToString(
                                      j, rs, rsmd,
                                      dbName);
                              final Element e =
                                      normalDoc.createElement(
                                      colName);

                              if (rs.wasNull()) {
                                colValue = "";
                                e.setAttribute(
                                        "isNull",
                                        "true");
                              } else
                                e.setAttribute(
                                        "isNull",
                                        "false");
                              e.appendChild(normalDoc.createTextNode(
                                      colValue));
                              paramElem.appendChild(
                                      e);
                              if (mLogger.isLoggable(
                                      Level.FINEST))
                                mLogger.log(
                                        Level.FINEST,
                                        "Col Name == " + colName + " and Col Val == " + colValue);
                            }
                          }
                        }
                      } else {
                        Object paramValue = null;
                        if (paramType.equalsIgnoreCase(
                                "CLOB")) {
                          java.sql.Clob clob =
                                  (java.sql.Clob) cs.getObject(paramIndex);
                          if (clob != null)
                            paramValue = clob.getSubString(1,
                                    (int) clob.length());
                        } else if (paramType.equalsIgnoreCase("BLOB")) {
                          java.sql.Blob blob =
                                  (java.sql.Blob) cs.getObject(paramIndex);
                          if (blob != null)
                            paramValue = new String(
                                    blob.getBytes(1,
                                    (int) blob.length()));
                        } else
                          paramValue = cs.getObject(
                                  paramIndex);
                        final Element e = normalDoc.createElement(paramName);
                        if (paramValue == null) {
                          paramValue = "";
                          e.setAttribute("isNull",
                                  "true");
                        } else
                          e.setAttribute("isNull",
                                  "false");
                        e.appendChild(normalDoc.createTextNode(paramValue.
                                toString()));
                        returnPartElement.appendChild(e);
                      }
                    } catch (SQLException sqle) {
                      throw sqle;
                    } finally {
                      if (rs != null)
                        try {
                          rs.close();
                        } catch (SQLException e) {
                          /* Ignore... */ ;
                        }
                    }

                  }
                  wrapperBuilder.addPart(part.getName(),
                          returnPartElement);
                } else {
                  final QName element = part.getElementName();
                  NS = element.getNamespaceURI();

                  final Element elementRoot = normalDoc.createElementNS(NS,
                          element.getLocalPart());

                  // returnPartElement.appendChild(elementRoot);
                  // get resultset metadata rsmd and add it to the element
                  for (int i = 0; i < outParamIndex.size(); i++) {
                    int paramIndex =
                            (Integer) outParamIndex.get(i);
                    String paramType = outParamTypes.get(
                            paramIndex);
                    String paramName = outParamNames.get(
                            paramIndex);
                    ResultSet rs = null;
                    try {
                      if (paramType.equalsIgnoreCase(
                              "REF CURSOR") || paramType.equalsIgnoreCase(
                              "RESULTSET")) {
                        if (dbName.trim().contains(
                                "sql server") || dbName.trim().contains(
                                "adaptive server"))
                          rs = (ResultSet) cs.getResultSet();
                        else
                          rs =
                                  (ResultSet) cs.getObject(paramIndex);
                        if (rs != null) {
                          final ResultSetMetaData rsmd =
                                  rs.getMetaData();
                          while (rs.next()) {
                            mRowCount++;
                            if (mLogger.isLoggable(
                                    Level.FINE))
                              mLogger.log(
                                      Level.FINE,
                                      "DBBC_R00706.JDBCN_ProcessNextRecord");
                            Element paramElem =
                                    normalDoc.createElementNS(
                                    NS,
                                    paramName + "_0_Resultset");
                            elementRoot.appendChild(
                                    paramElem);

                            for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                              final String colName =
                                      rsmd.getColumnName(
                                      j);
                              String colValue =
                                      JDBCUtil.convertToString(
                                      j, rs, rsmd,
                                      dbName);
                              final Element e =
                                      normalDoc.createElementNS(
                                      NS, colName);
                              if (rs.wasNull()) {
                                colValue = "";
                                e.setAttribute(
                                        "isNull",
                                        "true");
                              } else
                                e.setAttribute(
                                        "isNull",
                                        "false");
                              e.appendChild(normalDoc.createTextNode(
                                      colValue));
                              paramElem.appendChild(
                                      e);
                              if (mLogger.isLoggable(
                                      Level.FINEST))
                                mLogger.log(
                                        Level.FINEST,
                                        "Col Name == " + colName + " and Col Val == " + colValue);
                            }
                          }
                        }
                      } else {
                        Object paramValue = null;
                        if (paramType.equalsIgnoreCase(
                                "CLOB")) {
                          java.sql.Clob clob =
                                  (java.sql.Clob) cs.getObject(paramIndex);
                          if (clob != null)
                            paramValue = clob.getSubString(1,
                                    (int) clob.length());
                         } else if (paramType.equalsIgnoreCase("BLOB")) {
                          java.sql.Blob blob =
                                  (java.sql.Blob) cs.getObject(paramIndex);
                          if (blob != null)
                            paramValue = new String(
                                    blob.getBytes(1,
                                    (int) blob.length()));
                        } else
                          paramValue = cs.getObject(
                                  paramIndex);
                        final Element e = normalDoc.createElementNS(NS,
                                paramName);
                        if (paramValue == null) {
                          paramValue = "";
                          e.setAttribute("isNull",
                                  "true");
                        } else
                          e.setAttribute("isNull",
                                  "false");
                        e.appendChild(normalDoc.createTextNode(paramValue.
                                toString()));
                        elementRoot.appendChild(e);
                      }
                    } catch (SQLException sqle) {
                      throw sqle;
                    } finally {
                      if (rs != null)
                        try {
                          rs.close();
                        } catch (SQLException e) {
                          /* Ignore... */ ;
                        }
                    }
                  }
                  wrapperBuilder.addPart(part.getName(),
                          elementRoot);
                }
                normalDoc = wrapperBuilder.getResult();
              }
            } else {
              final String msgEx = JDBCNormalizer.mMessages.getString(
                      "DBBC_E00703.JDBCN_Failed_NM_Part") + returnPartName +
                      "in message " + msg.getQName();
              throw new MessagingException(msgEx);
            }
          } else {
            final String msgEx = JDBCNormalizer.mMessages.getString(
                    "DBBC_E00704.JDBCN_Failed_NM_WS_OPER") +
                    meta.getOperation().getName() +
                    " is missing message in its <output>";
            throw new MessagingException(msgEx);
          }
        } else {
          final String msgEx = JDBCNormalizer.mMessages.getString(
                  "DBBC_E00704.JDBCN_Failed_NM_WS_OPER") +
                  meta.getOperation().getName() +
                  " is missing <output> ";
          throw new MessagingException(msgEx);
        }
      } else {
        final String operationName =
                meta.getBindingOperation().getName();
        final Element normalRoot =
                normalDoc.createElement(operationName);
        normalDoc.appendChild(normalRoot);

        if (returnPartName != null) {
          final Element returnElement = normalDoc.createElement(
                  returnPartName);

          // get resultset metadata rsmd and add it to the element
          for (int i = 0; i < outParamIndex.size(); i++) {
            int paramIndex = (Integer) outParamIndex.get(i);
            String paramType = outParamTypes.get(paramIndex);
            String paramName = outParamNames.get(paramIndex);
            ResultSet rs = null;
            try {
              if (paramType.equalsIgnoreCase("REF CURSOR") || paramType.
                      equalsIgnoreCase("RESULTSET")) {
                if (dbName.trim().contains("sql server") || dbName.trim().
                        contains("adaptive server"))
                  rs = (ResultSet) cs.getResultSet();
                else
                  rs = (ResultSet) cs.getObject(paramIndex);
                if (rs != null) {
                  final ResultSetMetaData rsmd = rs.getMetaData();
                  while (rs.next()) {
                    mRowCount++;
                    if (mLogger.isLoggable(Level.FINE))
                      mLogger.log(Level.FINE,
                              "DBBC_R00706.JDBCN_ProcessNextRecord");
                    Element paramElem = normalDoc.createElementNS(NS,
                            paramName + "_0_Resultset");
                    returnElement.appendChild(paramElem);

                    for (int j = 1; j <= rsmd.getColumnCount(); j++) {
                      final String colName = rsmd.getColumnName(j);
                      String colValue = JDBCUtil.convertToString(j, rs, rsmd,
                              dbName);
                      mLogger.log(Level.FINE,
                              "Got resultset for param: " + paramName + " colName: " + colName + " value: " + colValue);
                      final Element e = normalDoc.createElementNS(NS, colName);
                      if (rs.wasNull()) {
                        colValue = "";
                        e.setAttribute("isNull", "true");
                      } else
                        e.setAttribute("isNull", "false");
                      e.appendChild(normalDoc.createTextNode(colValue));
                      returnElement.appendChild(e);
                      if (mLogger.isLoggable(Level.FINEST))
                        mLogger.log(Level.FINEST,
                                "Col Name == " + colName + " and Col Val == " + colValue);
                    }
                  }
                }
              } else {
                Object paramValue = null;
                if (paramType.equalsIgnoreCase("CLOB")) {
                  java.sql.Clob clob = (java.sql.Clob) cs.getObject(paramIndex);
                  if (clob != null)
                    paramValue = clob.getSubString(1,
                            (int) clob.length());
                } else if (paramType.equalsIgnoreCase("BLOB")) {
                  java.sql.Blob blob = (java.sql.Blob) cs.getObject(paramIndex);
                  if (blob != null)
                    paramValue = new String(blob.getBytes(1,
                            (int) blob.length()));
                } else
                  paramValue = cs.getObject(paramIndex);
                final Element e = normalDoc.createElementNS(NS,
                        paramName);
                if (paramValue == null) {
                  paramValue = "";
                  e.setAttribute("isNull", "true");
                } else
                  e.setAttribute("isNull", "false");
                e.appendChild(normalDoc.createTextNode(paramValue.toString()));
                returnElement.appendChild(e);
              }
            } catch (SQLException sqle) {
              throw sqle;
            } finally {
              if (rs != null)
                try {
                  rs.close();
                } catch (SQLException e) {
                  /* Ignore... */ ;
                }
            }
          }
          normalRoot.appendChild(returnElement);
        }
      }
      if (JDBCNormalizer.mLogger.isLoggable(Level.FINE))
        DebugLog.debugLog(mLogger, Level.INFO,
                "***normalize Procedure***", normalDoc);

      if (JDBCNormalizer.mLogger.isLoggable(Level.INFO))
        JDBCNormalizer.mLogger.log(Level.INFO, "normalized message",
                normalDoc);

      normalMsg.setContent(new DOMSource(normalDoc));
    } catch (final ParserConfigurationException tex) {
      final String msg = JDBCNormalizer.mMessages.getString(
              "DBBC_E00704.JDBCN_Failed_NM_WS_OPER");
      throw new MessagingException(msg, tex);
    } catch (final WrapperProcessingException ex) {
      final String exMsg = JDBCNormalizer.mMessages.getString(
              "DBBC_E00702.JDBCN_Failed_NM") + ex.getMessage();
      throw new MessagingException(exMsg, ex);
    } catch (final IOException ex) {
      final String exMsg = JDBCNormalizer.mMessages.getString(
              "DBBC_R00707.JDBCN_Failed_Base64_Encode");
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
      final DocumentBuilderFactory factory =
              DocumentBuilderFactory.newInstance();
      JDBCNormalizer.mBuilder = factory.newDocumentBuilder();
    }

    return JDBCNormalizer.mBuilder.newDocument();
  }

  private String[] validateColumnNames(ResultSetMetaData rsmd, String[] colNames) throws SQLException {
    try {
      for (int i = 0; i < rsmd.getColumnCount(); i++)
        colNames[i] = rsmd.getColumnName(i + 1);
    } catch (SQLException e) {
      throw e;
    }
    for (int i = 0; i < colNames.length - 1; i++)
      for (int j = i + 1; j < colNames.length; j++)
        if (colNames[i].equalsIgnoreCase(colNames[j])) {
          String temp = colNames[j] + "_1";
          do {
            temp = validate(colNames, temp);
          } while (status);
          colNames[j] = temp;
        }
    return colNames;
  }

  private String validate(String[] colNames, String temp) {
    for (int i = 0; i < colNames.length; i++)
      if (temp.equalsIgnoreCase(colNames[i])) {
        temp = temp + "_1";
        status = true;
        return temp;
      } else
        status = false;
    return temp;
  }

  protected void setOutParamIndex(ArrayList outParamIndex) {
    this.outParamIndex = outParamIndex;
  }

  protected void setOutParamTypes(HashMap outParamTypes) {
    this.outParamTypes = outParamTypes;
  }

  protected void setOutParamNames(HashMap outParamNames) {
    this.outParamNames = outParamNames;
  }

  public void setConnection(Connection con) {
    this.connection = con;
  }

  public void setInboundExchangeProcessRecordsMap(Map map) {
    this.mInboundExchangeProcessRecordsMap = map;
  }

  public void setRecordsProcessedList(ArrayList list) {
    this.mProcessedList = list;
  }

  public void setJDBCClusterManager(JDBCClusterManager jdbcClusterManager) {
    this.mJDBCClusterManager = jdbcClusterManager;
  }

  private boolean isRecordInserted(EndpointBean endpoint, Connection con, String pkName,
                                   String colValue) {
    boolean recordInserted = true;
    String insertQuery =
            "insert into OWNER_" + endpoint.getTableName() + "(" + pkName + ",INSTANCE_NAME) values(?,?)";
    PreparedStatement ps = null;
    ParameterMetaData paramMetaData = null;
    int parameters = 0;
    try {
      ps = con.prepareStatement(insertQuery);
      paramMetaData = ps.getParameterMetaData();
      if (paramMetaData != null)
        parameters = paramMetaData.getParameterCount();
    } catch (final SQLException ex) {
      mLogger.log(Level.WARNING, ex.getLocalizedMessage());
      return false;
    }
    if (parameters != 0)
      if ((colValue != null) && !colValue.trim().equals("")) {
        // set default type.
        int columnType = java.sql.Types.VARCHAR;
        try {
          columnType = paramMetaData.getParameterType(1);
          ps.setObject(1, JDBCUtil.convert(colValue, columnType),
                  columnType);
          columnType = paramMetaData.getParameterType(2);
          ps.setObject(2, JDBCUtil.convert(endpoint.getInstanceName(),
                  columnType), columnType);
          int rowsUpdated = ps.executeUpdate();
          recordInserted = true;
        } catch (final Exception e) {
          mLogger.log(Level.WARNING, e.getLocalizedMessage());
          mLogger.log(Level.INFO, mMessages.getString(
                  "DBBC-R01127.JDBCN_RECORD_LOCKED",
                  new Object[]{endpoint.getInstanceName()}));
          recordInserted = false;
        }
      }
    return recordInserted;
  }

  private boolean isRecordProcessed(String colValue) {
    boolean recordProcessed = true;
    if (mProcessedList.isEmpty() || !mProcessedList.contains(colValue)) {
      mProcessedList.add(colValue);
      recordProcessed = false;
    }
    return recordProcessed;
  }
}
