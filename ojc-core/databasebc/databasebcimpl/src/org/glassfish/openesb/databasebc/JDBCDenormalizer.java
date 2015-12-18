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
 * @(#)JDBCDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.databasebc;

import java.io.StringWriter;
import java.sql.CallableStatement;
import java.sql.DatabaseMetaData;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import org.glassfish.openesb.databasebc.model.metadata.DBMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;
import org.glassfish.openesb.databasebc.extensions.JDBCOperationInput;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import java.io.InputStream;
import java.io.Reader;
import java.util.Scanner;
import javax.xml.transform.stream.StreamSource;
import org.glassfish.openesb.databasebc.extensions.SPOperationInput;
import org.glassfish.openesb.databasebc.util.CachedQueryParameter;
import org.glassfish.openesb.databasebc.util.ParamMetadataCache;

/**
 *
 * JDBCDenormalizer
 *
 */
public class JDBCDenormalizer {

  private static final Messages mMessages = Messages.getMessages(
          JDBCDenormalizer.class);
  private static final Logger mLogger = Messages.getLogger(
          JDBCDenormalizer.class);
  private String dbName = null;
  private String catalog = null;
  private ArrayList outParamIndex = new ArrayList();
  private HashMap<Integer, String> outParamTypes =
          new HashMap<Integer, String>();
  private HashMap<Integer, String> outParamNames =
          new HashMap<Integer, String>();
  private HashMap<String, String> mColNamesTypes =
          new HashMap<String, String>();
  private String mRecordPrefix = null; //113494
  private String driverName_;

  /**
   *
   * @param normalizedMessage
   * @param opMetaData
   * @param ps
   * @throws MessagingException
   */
  protected void denormalizeOutbound(final NormalizedMessage normalizedMessage,
                                     String dbName, final String jndiName, final OperationMetaData opMetaData, final PreparedStatement ps)
          throws MessagingException {
    if (opMetaData != null) {
      final JDBCOperationInput mJdbcOperationInput = opMetaData.
              getJDBCOperationInput();
      final String operationType = mJdbcOperationInput.getOperationType();
      mColNamesTypes = mJdbcOperationInput.getColNamesTypes();
      //if (JDBCUtil.opList.contains(operationType)) {
      try {
        Element element =
                transformMessage(normalizedMessage, opMetaData);
        if (element != null)
          populatePreparedStatement(element, dbName, jndiName, opMetaData, ps);
      } catch (final SQLException ex) {
        final String msg =
                JDBCDenormalizer.mMessages.getString(
                "DBBC_E00709.JDBCDN_FailedPopulate_PS") +
                "Reason: " + ex.getLocalizedMessage() + " SQLState: " + ex.
                getSQLState() + " ErrorCode:" + ex.getErrorCode();
        throw new MessagingException(msg, ex);
      } catch (final Exception ex) {
        final String msg = JDBCDenormalizer.mMessages.getString(
                "DBBC_E00709.JDBCDN_FailedPopulate_PS") +
                "Reason: " + ex.getLocalizedMessage();
        throw new MessagingException(msg, ex);
      }
      //}
    }
  }

  /**
   *
   * @param normalizedMessage
   * @param opMetaData
   * @param dbmeta
   * @param cs
   * @throws MessagingException
   */
  protected void denormalizeOutboundProc(final NormalizedMessage normalizedMessage,
                                         final OperationMetaData opMetaData, final DatabaseMetaData dbmeta, final String jndiName, final CallableStatement cs)
          throws MessagingException {
    if (opMetaData != null) {
      final SPOperationInput mJdbcOperationInput = opMetaData.
              getJDBCSPOperationInput();
      final String operationType = mJdbcOperationInput.getOperationType();

      try {
        Element element =
                transformMessage(normalizedMessage, opMetaData);
        if (element != null)
          populateProcedure(element, opMetaData, dbmeta, jndiName, cs);
      } catch (final SQLException ex) {
        final String msg =
                JDBCDenormalizer.mMessages.getString(
                "DBBC_E00711.DN_FailedPopulate_Proc") +
                "Reason: " + ex.getLocalizedMessage() + " SQLState: " + ex.
                getSQLState() + " ErrorCode:" + ex.getErrorCode();
        throw new MessagingException(msg, ex);
      } catch (final Throwable th) {
        final String msg = JDBCDenormalizer.mMessages.getString(
                "DBBC_E00712.DN_Failed_Proc") + "Reason: " + th.
                getLocalizedMessage();
        throw new MessagingException(msg, th);
      }
    }
  }

  /**
   *
   * @param tableElem
   * @param opMetaData
   * @param cs
   * @throws SQLException
   * @throws MessagingException
   * @throws Exception
   */
  @SuppressWarnings("empty-statement")
  private void populateProcedure(final Element tableElem,
                                 final OperationMetaData opMetaData, final DatabaseMetaData dbmeta, final String jndiName, CallableStatement cs)
          throws SQLException, MessagingException, Exception {
    final SPOperationInput jdbcSql = opMetaData.getJDBCSPOperationInput();
    driverName_ = dbmeta.getDriverName();
    String sqltext = jdbcSql.getExecutionString();
    String procName = getProcName(sqltext);
    /*
     * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
     */
    String pcatalog = null;
    String pschema = null;
    if (dbName != null)
      pschema = dbName;
    if (catalog != null)
      pcatalog = catalog;
    ResultSet rs = null;
    if (mLogger.isLoggable(Level.INFO))
      mLogger.log(Level.INFO, JDBCDenormalizer.mMessages.getString(
              "DBBC_R00716.JDBCDN_StartPopulateProc"));
    if (jdbcSql != null) {
      String dbURL = dbmeta.getURL();
      if (dbURL == null)
        dbURL = "jndi:"+jndiName;
      ParamMetadataCache paramsCache = ParamMetadataCache.instance();
      ArrayList<CachedQueryParameter> params = paramsCache.getMetadata(
              dbURL, opMetaData);

      if (params == null) {
        try {
          final String dbms = dbmeta.getDatabaseProductName().
                  toLowerCase();
          final String colNamePattern =
                  ((dbms.contains("sql server") || dbms.contains(
                  "adaptive server")) && sqltext.contains("=")) ? null : "%";
          
          rs = dbmeta.getProcedureColumns(pcatalog, pschema, procName,
                  colNamePattern);
        } catch (final SQLException ex) {
          if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.SEVERE, JDBCDenormalizer.mMessages.getString(
                    "DBBC_R00721.JDBCDN_UnableToProcessMedataData"),
                    ex);
          else if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.SEVERE, JDBCDenormalizer.mMessages.getString(
                    "DBBC_R00721.JDBCDN_UnableToProcessMedataData"));
        }

        if (rs != null)
          params = cacheQueryParameters(rs);
        else {
          final String msg = JDBCDenormalizer.mMessages.getString(
                  "DBBC_R00721.JDBCDN_UnableToProcessMedataData");
          MessagingException me = new MessagingException(msg);
          mLogger.log(Level.SEVERE, msg);
          throw me;
        }

        try {
          rs.close();
        } catch (SQLException e) {
          mLogger.log(Level.WARNING,
                  JDBCDenormalizer.mMessages.getString(
                  "DBBC_R00727.JDBCDN_CursorLeak") + procName);
        }
        paramsCache.storeMetadata(dbURL, opMetaData, params);
      }


      for (int i = 0; i < params.size(); ++i) {
        if (mLogger.isLoggable(Level.FINE))
          mLogger.log(Level.FINE,
                  JDBCDenormalizer.mMessages.getString(
                  "DBBC_R00719.JDBCDN_ProcessColumn"));
        CachedQueryParameter param = params.get(i);
        String columnName = param.getName();
        // strip off "@" in front of columnName
        // In case if it is function, for RETURN parameter coulumnName will be null.
        if (columnName != null && columnName.charAt(0) == '@')
          columnName = columnName.substring(1);

        int targetSqlType = param.getType();
        int colType = param.getDirection();
        String type_Name = param.getTypeName();
        short numericScale = param.getPrecision();

        final int columnNumber = i + 1;
        if (colType == DatabaseMetaData.procedureColumnIn) {
          if ((targetSqlType == 1111) && (type_Name.equals(
                  "PL/SQL TABLE")))
            targetSqlType = -14;

          if ((targetSqlType == 1111) && (type_Name.equals(
                  "PL/SQL RECORD")))
            targetSqlType = -14;

          if ((targetSqlType == 1111) && (type_Name.equals("CLOB")))
            targetSqlType = 2005;

          if ((targetSqlType == 1111) && (type_Name.equals("NVARCHAR2")))
            targetSqlType = 12;
          final Element columnElem = findMatchingColumnElement(
                  columnName,
                  tableElem, mRecordPrefix); //113494

          if (columnElem != null) {
            final String value = DOMUtils.getChildCharacterData(
                    columnElem);
            String isNull = "";
            try {
              isNull = columnElem.getAttribute("isNull");
            } catch (Exception e) {
              mLogger.log(Level.FINE, JDBCDenormalizer.mMessages.getString(
                      "DBBC_R00722.JDBCDN_IsNullNotSet"));
            }
            if (((value != null) && !value.trim().equals("")) || (!isNull.
                    equalsIgnoreCase("true")))
              if (dbmeta.getDriverName().toLowerCase().contains(
                      "oracle") && targetSqlType == java.sql.Types.BLOB)
                cs.setBinaryStream(columnNumber,
                        (java.io.ByteArrayInputStream) JDBCUtil.convert(value,
                        targetSqlType, cs.getConnection()), value.length());
              else if (dbmeta.getDriverName().toLowerCase().
                      contains("oracle") && targetSqlType == java.sql.Types.CLOB)
                cs.setCharacterStream(columnNumber,
                        (java.io.StringReader) JDBCUtil.convert(
                        value, targetSqlType, cs.getConnection()),
                        value.length());
              else
                cs.setObject(columnNumber,
                        JDBCUtil.convert(value, targetSqlType,
                        cs.getConnection()), targetSqlType);
            else
              cs.setNull(columnNumber, targetSqlType);
          }//end of if
        }//end of if

        if (colType == DatabaseMetaData.procedureColumnInOut || colType == DatabaseMetaData.procedureColumnOut)
          try {
            // if the parameter is a cursor type, add its index to the arraylist
            if ((targetSqlType == 1111) && (type_Name.equals(
                    "REF CURSOR"))) {
              targetSqlType = -10;
              cs.registerOutParameter(columnNumber, targetSqlType);
            } else if ((targetSqlType == 1111) && (type_Name.equals("CLOB"))) {
              targetSqlType = 2005;
              cs.registerOutParameter(columnNumber, targetSqlType,
                      type_Name);
            } else if (dbmeta.getDatabaseProductName().
                    toLowerCase().contains("oracle"))
              cs.registerOutParameter(columnNumber, targetSqlType);
            else
              cs.registerOutParameter(columnNumber, targetSqlType,
                      type_Name);
            outParamIndex.add(Integer.valueOf(Double.valueOf(
                    columnNumber).intValue()));
            outParamTypes.put(columnNumber, type_Name);
            outParamNames.put(columnNumber, columnName);
          } catch (SQLException e) {
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.WARNING, e.getLocalizedMessage(),
                      e);
            else if (mLogger.isLoggable(Level.INFO))
              mLogger.log(Level.WARNING, e.getLocalizedMessage());
            throw e;
          }

        // check if the parameter is RETURN type (i.e. it is a function)
        if (colType == DatabaseMetaData.procedureColumnReturn)
          try {
            // if the parameter is a cursor type, add its index to the arraylist
            if ((targetSqlType == 1111) && (type_Name.equals(
                    "REF CURSOR"))) {
              targetSqlType = -10;
              cs.registerOutParameter(columnNumber, targetSqlType);
            } else if ((dbmeta.getDatabaseProductName().
                    toLowerCase().contains("sql server") ||
                    dbmeta.getDatabaseProductName().
                    toLowerCase().contains("adaptive server")) &&
                    columnName.equals("RETURN_VALUE"))
              cs.registerOutParameter(columnNumber, targetSqlType,
                      type_Name);
            else
              cs.registerOutParameter(columnNumber, targetSqlType,
                      numericScale);

            outParamIndex.add(Integer.valueOf(Double.valueOf(
                    columnNumber).intValue()));
            outParamTypes.put(columnNumber, type_Name);
            //columnName will be null for RETURN type. However DB BC wizard puts it as param1.
            //So hardcoding it so that normalizes return value.
            if (columnName == null)
              columnName = "param1";
            outParamNames.put(columnNumber, columnName);
          } catch (SQLException e) {
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.WARNING, e.getLocalizedMessage(),
                      e);
            else if (mLogger.isLoggable(Level.INFO))
              mLogger.log(Level.WARNING, e.getLocalizedMessage());
            throw e;
          }

        //i++;
        if (mLogger.isLoggable(Level.FINEST)) {
          mLogger.log(Level.FINEST,
                  JDBCDenormalizer.mMessages.getString(
                  "DBBC_R00717.JDBCDN_ColumnName") + "  == " + columnName);
          mLogger.log(Level.FINEST,
                  JDBCDenormalizer.mMessages.getString(
                  "DBBC_R00718.JDBCDN_ColumnType") + "  == " + type_Name);
        }

      }//end of While
    }
  }

  /**
   *
   * @param tableElem
   * @param opMetaData
   * @param ps
   * @throws SQLException
   * @throws MessagingException
   * @throws Exception
   */
  private void populatePreparedStatement(final Element tableElem, String dbName, String jndiName,
                                         final OperationMetaData opMetaData, final PreparedStatement ps)
          throws SQLException, MessagingException, Exception {
    /*
     * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
     */
    DatabaseMetaData dbMeta = ps.getConnection().getMetaData();

    final JDBCOperationInput jdbcSql = opMetaData.getJDBCSql();

    /**
    /*
     * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
     *
     * When generatedKey is specified, parameterMetaData's count includes the generatedKey's parameter also.
     * So we need to reduce the param count value by 1 so that it matches the number of columns specified in paramOrder.
     * This is applicable only for Oracle.
     */
    String generatedKey = jdbcSql.getGeneratedKey();
    boolean isOracle = dbMeta.getDriverName().toLowerCase().contains(
            "oracle");
    boolean reduceParamCountFromParamMetaData = false;
    if (generatedKey != null && !generatedKey.equals("") && isOracle)
      reduceParamCountFromParamMetaData = true;
    /* Logicoy - changes ends here */

    if (mLogger.isLoggable(Level.FINE))
      mLogger.log(Level.FINE, JDBCDenormalizer.mMessages.getString(
              "DBBC_R00720.JDBCDN_StartPopulatePS"));

    if (jdbcSql != null) {

      //113494  start
      mRecordPrefix = opMetaData.getJDBCSql().getTableName();
      // see if the table name is null (SQL SE)
      // then take the query name as the prefix
      if (mRecordPrefix == null) {
        mRecordPrefix = opMetaData.getJDBCOperationOutput().getName();
        mRecordPrefix = mRecordPrefix.substring(0,
                mRecordPrefix.indexOf("Response"));
      }

//113494  end
      String dbURL = dbMeta.getURL();
      if (dbURL == null)
        dbURL = "jndi:"+jndiName;
      ArrayList<CachedQueryParameter> paramsList =
              ParamMetadataCache.instance().getMetadata(dbURL, opMetaData);
      if (paramsList == null) {
        ParameterMetaData paramMetaData = null;
        try {
          paramMetaData = ps.getParameterMetaData();
        } catch (final SQLException ex) {
          mLogger.log(Level.WARNING, ex.getLocalizedMessage());
        }

        if (paramMetaData != null) {
          /*
           * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
           */
          /**
           * Modified by Logicoy.
           * When generatedKey is specified, parameterMetaData's count includes the generatedKey's parameter also.
           * So we need to reduce the param count value by 1 so that it matches the number of columns specified in paramOrder.
           * This is applicable only for Oracle.
           */
          int parameters = paramMetaData.getParameterCount();

          if (reduceParamCountFromParamMetaData)
            parameters = parameters - 1;


          if (parameters > 0) {// If there are no parameter we do not need to set anything /* Logicoy - changes ends here */
            // on Prepared statement
            String paramOrder = jdbcSql.getParamOrder();

            // changed for SQLSE since the user cannot enter param
            // ordering in sqlprojects.
            // we should generate a default param ordering.
            if (paramOrder == null || paramOrder.trim().equals(""))
              paramOrder = getDefaultParameterOrderString(
                      parameters);

            final List columns = extractColumns(paramOrder);

            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.FINE,
                      JDBCDenormalizer.mMessages.getString(
                      "DBBC_R00722.JDBCDN_TotalColumns") + columns.size());

            paramsList =
                    cacheQueryParameters(columns, paramMetaData,
                    reduceParamCountFromParamMetaData);

          } else
            paramsList = cacheEmptyQueryParams();

          ParamMetadataCache.instance().storeMetadata(dbURL,
                  opMetaData, paramsList);
        }
      }
      if (paramsList != null)
        for (int i = 0; i < paramsList.size(); i++) {
          if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, JDBCDenormalizer.mMessages.getString(
                    "DBBC_R00724.JDBCDN_ProcessParam"));

          CachedQueryParameter param = paramsList.get(i);
          final String columnName = param.getName();
          final int columnNumber = i + 1;
          final Element columnElement = findMatchingColumnElement(
                  columnName, tableElem, mRecordPrefix);  //113494 Issue

          if (columnElement != null) {
            final String value = DOMUtils.getChildCharacterData(
                    columnElement);
            String isNull = "";
            String isDefaultColumn = "";
            try {
              isNull = columnElement.getAttribute("isNull");
            } catch (Exception e) {
              mLogger.log(Level.FINE, JDBCDenormalizer.mMessages.getString(
                      "DBBC_R00722.JDBCDN_IsNullNotSet"));
            }

            try {
              isDefaultColumn = columnElement.getAttribute(
                      "isDefaultColumn");
            } catch (Exception e) {
              mLogger.log(Level.FINE, JDBCDenormalizer.mMessages.getString(
                      "DBBC_R00723.JDBCDN_IsDefaultNotSet"));
            }
            /*
             * Modified by Logicoy Inc.,
             * Open ESB Bug No: 107 - Throws error during insert when any of the parameter has empty value
             * The below condition checks if value is not empty, not isNull and not isDefault. Changed the OR condition (BUG) to AND condition
             * 
             */
            if ((((value != null) && !value.trim().equals("")) && (!isNull.equalsIgnoreCase("true"))) && 
            		(((value != null) && !value.trim().equals("")) && (!isDefaultColumn.equalsIgnoreCase("true")))) {
            	 /*
                 * Logiocy Inc.,
                 * Bug 107 - changes ends here	
                 */	
              int columnType = param.getType();
              if (ps.getConnection().getMetaData().getDriverName().
                      toLowerCase().contains("oracle") && columnType == java.sql.Types.BLOB)
                ps.setBinaryStream(columnNumber,
                        (java.io.ByteArrayInputStream) JDBCUtil.convert(value,
                        columnType, ps.getConnection()), value.length());
              else if (ps.getConnection().getMetaData().
                      getDriverName().toLowerCase().contains(
                      "oracle") && columnType == java.sql.Types.CLOB)
                ps.setCharacterStream(columnNumber,
                        (java.io.StringReader) JDBCUtil.convert(
                        value, columnType, ps.getConnection()),
                        value.length());
              else
                ps.setObject(columnNumber, JDBCUtil.convert(
                        value, columnType, ps.getConnection()),
                        columnType);

              if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST,
                        mMessages.getString(
                        "DBBC_R00717.JDBCDN_ColumnName") + "  == " + columnName);
                mLogger.log(Level.FINEST,
                        mMessages.getString(
                        "DBBC_R00726.JDBCDN_ColumnType") + "  == " + columnType);
              }

            } else {
              int columnType = param.getType();

              if ((!isNull.equalsIgnoreCase("true")) && (isDefaultColumn != null && isDefaultColumn.
                      equalsIgnoreCase("true"))) {
                if (!param.defaultSet()) {
                  ResultSet rs = ps.getConnection().
                          getMetaData().getColumns(null, null,
                          jdbcSql.getTableName(), "%");
                  while (rs.next())
                    if (columnName.equalsIgnoreCase(rs.getString("COLUMN_NAME")) && rs.
                            getString("COLUMN_DEF") != null)
                      //ps.setObject(columnNumber, JDBCUtil.convert(rs.getString("COLUMN_DEF"), columnType, ps.getConnection()), columnType);
                      param.setDefault(rs.getString(
                              "COLUMN_DEF"));
                  param.setDefault(null);
                }

                if (param.hasDefault())
                  ps.setObject(columnNumber, JDBCUtil.convert(
                          param.getDefault(), columnType, ps.getConnection()),
                          columnType);
                else
                  ps.setNull(columnNumber, columnType);

              } else
                ps.setNull(columnNumber, columnType);

            }


          }
        }
      else {
        final String msg = mMessages.getString(
                "DBBC_E00714.JDBCDN_Failed_PS_Param");
        throw new MessagingException(msg);
      }

    }
  }

  /**
   *
   * @param dbColumnName
   * @param tableElem
   * @return
   */
  private Element findMatchingColumnElement(final String dbColumnName,
                                            final Element tableElem, String recordPrefix) { // 113494 Issue
    Element columnElem = null;
    NodeList childNodes = tableElem.getChildNodes();

    for (int i = 0; i < childNodes.getLength(); i++) {
      final Node child = childNodes.item(i);
      if ((child instanceof Element) &&
              child.getLocalName().equalsIgnoreCase(
              recordPrefix + "_Record")) { // 113494 Issue
        if (mLogger.isLoggable(Level.FINEST))
          mLogger.log(Level.FINEST, recordPrefix + "_Record");
        columnElem = (Element) child;
        childNodes = columnElem.getChildNodes();
      }
    }
    for (int i = 0; i < childNodes.getLength(); i++) {
      final Node child = childNodes.item(i);
      if ((child instanceof Element) &&
              child.getLocalName().equalsIgnoreCase(dbColumnName)) {
        columnElem = (Element) child;
        break;

      }
    }

    if (null == columnElem) {
      //throw new MessagingException(String.format(JDBCDenormalizer.mMessages.getString("DBBC_E00727.JDBCDN_FailedFindColumnElem"), dbColumnName));
      columnElem =
              tableElem.getOwnerDocument().createElement(dbColumnName);
      columnElem.setAttribute("isNull", "true");
    }

    return columnElem;
  }

  /**
   *
   * @param paramOrder
   * @return
   */
  private List<String> extractColumns(final String paramOrder) {
    final List<String> columnList = new ArrayList<String>();

    if (paramOrder != null) {
      final Scanner tok = new Scanner(paramOrder).useDelimiter(
              "\\s*" + "," + "\\s*");

      while (tok.hasNext()) {
        final String column = tok.next();
        columnList.add(column.trim());
      }

    }

    return columnList;
  }

  /**
   *
   * @param parent
   * @param msgQName
   * @return
   */
  private Element findChildElement(final Element parent,
                                   final QName msgQName) {
    final String ns = msgQName.getNamespaceURI();
    final String localName = msgQName.getLocalPart();
    NodeList nl = null;

    if ((ns != null) && !ns.trim().equals(""))
      nl = parent.getElementsByTagNameNS(ns, localName);
    else
      nl = parent.getElementsByTagName(localName);

    if ((nl != null) && (nl.getLength() > 0)) {
      if (JDBCDenormalizer.mLogger.isLoggable(Level.INFO))
        JDBCDenormalizer.mLogger.log(Level.INFO, "found element");

      if (JDBCDenormalizer.mLogger.isLoggable(Level.FINEST))
        JDBCDenormalizer.mLogger.log(Level.FINEST,
                "found element with name, " + localName);

      final Element e2 = (Element) nl.item(0);

      return e2;
    }

    return null;
  }

  /**
   *
   * @param root
   * @param elemName
   * @return
   */
  private Element findPart(final Element root,
                           final String elemName) {
    // parts wrappers never have namespace
    final NodeList nl = root.getElementsByTagName(elemName);

    if ((nl != null) && (nl.getLength() > 0)) {
      if (JDBCDenormalizer.mLogger.isLoggable(Level.INFO))
        JDBCDenormalizer.mLogger.log(Level.INFO,
                "found element with name, " + elemName);

      final Element e2 = (Element) nl.item(0);

      return e2;
    }

    return null;
  }

  /**
   ** Used by SQLSE to get the default parameter ordering.
   * @param pmeta
   * @return
   */
  /*
   * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
   */
  /**
   * Modified by Logicoy
   * Refactored this method to pass the parameterMetaData's count instead of passing the ParameterMetaDataObject itself
   * since we only need the count value.
   */
  private String getDefaultParameterOrderString(int numParams) {
    String parameterOrderString = null;
    //int numParams = 0;

    //if (pmeta != null) {
            /*try {
    numParams = pmeta.getParameterCount();
    } catch (final SQLException sqle) {
    JDBCDenormalizer.mLogger.log(Level.WARNING,
    JDBCDenormalizer.mMessages.getString("JDBCDN_Failed_ParamCount"));

    return null;
    }*/

    if (numParams > 0)
      for (int i = 1; i <=
              numParams; i++) {
        final String paramname = "param" + String.valueOf(i);
        parameterOrderString =
                (parameterOrderString == null)
                ? paramname : (parameterOrderString + "," + paramname);
      }
    //}

    return parameterOrderString;
  }
//register out parameters

  private void registerOutParameters(CallableStatement cstmt, final DatabaseMetaData dbmeta)
          throws SQLException, NullPointerException {
    String errMsg = "";
    int colCount = 0;
    boolean isFunction = false;
    boolean hasParameters = true;
    // indicates if the procedure is within a package or standalone
    boolean isPackaged = true;
    ArrayList paramIndices = new ArrayList();
    ArrayList result = new ArrayList();
    int paramIndex = 0;
    try {
      ParameterMetaData pmeta = cstmt.getParameterMetaData();
      if (pmeta != null) {
        int numParams = pmeta.getParameterCount();
        if (numParams > 0)
          // get info for each parameter
          for (int i = 1; i <=
                  numParams; i++) {
            // try to get the sql type info - default to VARCHAR
            String sqlType = "VARCHAR";
            try {
              sqlType = DBMetaData.getSQLTypeDescription(pmeta.getParameterType(
                      i));
            } catch (SQLException e) {
              mLogger.log(Level.WARNING,
                      "Could not get SQL Type Description from DBMetadata",
                      e.getLocalizedMessage());
            }

// try to get the java type info - default to String
            /**
             * Changing it to not use metadata class name and instead use the HashMap SQLTOJAVATYPES.
             * Without the change the parameter datatypes java.lang.Double and WSDLGenerator look up list
             * exepects native type double, float, short etc.
             **/
            String javaType = "java.lang.String";
            javaType =
                    DBMetaData.getJavaFromSQLTypeDescription(sqlType);

//                      added abey for Procedure ResultSet
            try {
              if ((pmeta.getParameterType(i) == java.sql.Types.OTHER) && (pmeta.
                      getParameterTypeName(i).equalsIgnoreCase(
                      "REF CURSOR"))) {
                sqlType = "RESULTSET";
                javaType =
                        "java.sql.ResultSet";
              }

            } catch (SQLException e) {
              mLogger.log(Level.WARNING,
                      "Could not get Java type information from DBMetadata",
                      e.getLocalizedMessage());
            }

// try to get the param type, default to IN
// always default it since getParameterMode() in data direct 3.3 throws exception
// and 3.4 return UNKNOWN type
            String paramType = "IN";

            try {
              paramType = DBMetaData.getPrepStmtParamTypeDescription(pmeta.
                      getParameterMode((i)));
            } catch (SQLException e) {
              mLogger.log(Level.WARNING,
                      "Could not get PreparedStatement Parameter Description",
                      e.getLocalizedMessage());
            }

//                      set defalut type
            int sqlTypeCode = java.sql.Types.VARCHAR;
            if (paramType.equalsIgnoreCase("INOUT") || paramType.
                    equalsIgnoreCase("OUT"))
              try {

                // if the parameter is a cursor type, add its index to the arraylist
                if ((pmeta.getParameterType(i) == 1111) && (paramType.equals(
                        "OTHER")))
                  sqlTypeCode = java.sql.Types.OTHER;

              } catch (SQLException e) {
                mLogger.log(Level.WARNING,
                        "Driver Does not support getting the Datatypes",
                        e.getLocalizedMessage());
              }
            if (paramType.equals("RETURN"))
              try {
                // if the parameter is a cursor type, add its index to the arraylist
                if ((pmeta.getParameterType(i) == 1111) && (paramType.equals(
                        "OTHER"))) {
                  //sqlTypeCode = java.sql.Types.OTHER;
                }

              } catch (SQLException e) {
                mLogger.log(Level.WARNING,
                        "Driver Does not support getting the Datatypes",
                        e.getLocalizedMessage());
              }
            cstmt.registerOutParameter(paramIndex, sqlTypeCode);
            outParamIndex.add(Integer.valueOf(Double.valueOf(
                    paramIndex).intValue()));


          }
      }





    } catch (SQLException e) {
      mLogger.log(Level.WARNING, "Could not get Parameter MetaData", e);
    }

  }

  /*
  private String getProcName(String sqlText) {
  String proc_name = "";
  String schema = "";
  final Scanner tok = new Scanner(sqlText).useDelimiter("\\s*" + " " + "\\s*");

  while (tok.hasNext()) {
  String column = tok.next();
  int cnt = 0;
  column=column.toLowerCase();
  if(column.endsWith("call")){
  cnt++;
  proc_name=tok.next();
  if(proc_name.contains(".")){
  final Scanner tok1 = new Scanner(proc_name).useDelimiter("\\s*" + "." + "\\s*");
  schema=tok1.next();
  proc_name=tok1.next();
  }
  if(proc_name.contains("(")){
  int i = proc_name.indexOf("(");
  proc_name=proc_name.substring(0, i);
  }
  if(proc_name.contains("}")){
  int i = proc_name.indexOf("}");
  proc_name=proc_name.substring(0, i);
  }
  }
  if(cnt>0)
  break;
  }
  return proc_name;
  }
   */
  public String getProcName(
          String sqlText) {
    String proc_name = "";
    String schema = "";
    final StringTokenizer tok = new StringTokenizer(sqlText, " ");

    while (tok.hasMoreElements()) {
      String column = (String) tok.nextElement();
      int cnt = 0;
      column =
              column.toLowerCase();
      if (column.endsWith("call")) {
        cnt++;
        proc_name =
                (String) tok.nextElement();
        if (proc_name.contains(".")) {
          //final StringTokenizer tok1 = new StringTokenizer(proc_name, ".");
          //catalog = tok1.nextToken();
          //final String beforeDot1 = tok1.nextToken();
          //proc_name = tok1.nextToken();
          String[] procData = proc_name.split("\\.");
          final int len = procData.length;
          proc_name =
                  procData[len - 1];
          String catName = null;
          String schemaName = null;
          if (len == 3) {
            catName = procData[0];
            schemaName =
                    procData[1];
          } else
            schemaName = procData[0];

          if (driverName_.toLowerCase().contains("oracle")) {
            catalog = schemaName;
            dbName =
                    catName;
          } else {
            catalog = catName;
            dbName =
                    schemaName;
          }

        }
        if (proc_name.contains("(")) {
          int i = proc_name.indexOf("(");
          proc_name =
                  proc_name.substring(0, i);
        }

        if (proc_name.contains("}")) {
          int i = proc_name.indexOf("}");
          proc_name =
                  proc_name.substring(0, i);
        }

      }
      if (cnt > 0)
        break;

    }
    return proc_name;
  }

  private final Element transformMessage(final NormalizedMessage normalizedMessage,
                                         final OperationMetaData opMetaData) throws MessagingException {
    Element element = null;
    try {
      final TransformerFactory tFactory = TransformerFactory.newInstance();
      final Transformer trans = tFactory.newTransformer();
      final Source source = normalizedMessage.getContent();
      final DOMResult result = new DOMResult();
      trans.transform(source, result);

      if (source instanceof StreamSource) {
        StreamSource stream = (StreamSource) source;
        InputStream inputStream = stream.getInputStream();
        if (inputStream != null)
          inputStream.reset();

        Reader reader = stream.getReader();
        if (reader != null)
          reader.reset();

      }

      final Node node = result.getNode();
      final StringWriter strWriter = new StringWriter();
      final StreamResult sResult = new StreamResult(strWriter);
      trans.transform(source, sResult);

      if (source instanceof StreamSource) {
        StreamSource stream = (StreamSource) source;
        InputStream inputStream = stream.getInputStream();
        if (inputStream != null)
          inputStream.reset();

        Reader reader = stream.getReader();
        if (reader != null)
          reader.reset();

      }
      if (node != null) {
        Document normalDoc = null;

        if (node instanceof Document)
          normalDoc = (Document) node;
        else
          normalDoc = ((Element) node).getOwnerDocument();

        final Element normalRoot = normalDoc.getDocumentElement();
        //final BindingOperation bindingOperation = opMetaData.getBindingOperation();
        final Operation operation = opMetaData.getOperation();
        final Input input = operation.getInput();
        final Message inputMessage = input.getMessage();

        if (HelperFactory.WRAPPER_ENABLED) {
          final WrapperParser wrapperParser = HelperFactory.createParser();
          wrapperParser.parse(normalDoc, inputMessage);

          final Map parts = inputMessage.getParts();
          final Iterator it = parts.values().iterator();

          while (it.hasNext()) {
            final Part part = (Part) it.next();

            if (wrapperParser.hasPart(part.getName())) {
              final QName elementQName = part.getElementName();

              if (elementQName == null) {
                final String msgEx = JDBCDenormalizer.mMessages.getString(
                        "DBBC_E00706.DN_Failed_Denormalize") +
                        part.getName() +
                        "should have element attribute defined.";
                throw new MessagingException(msgEx);
              }

//Element element = null;
              final NodeList unwrappedList = wrapperParser.getPartNodes(part.
                      getName());

              for (int j = 0;
                      j < unwrappedList.getLength();
                      j++) {
                final Node unwrapped = unwrappedList.item(j);

                if ((unwrapped.getNodeType() == Node.ELEMENT_NODE) &&
                        (unwrapped.getLocalName() != null) &&
                        unwrapped.getLocalName().equals(elementQName.
                        getLocalPart())) {
                  element = (Element) unwrapped;

                  break;
                }
              }

              /*if (element != null) {
              populateProcedure(element, opMetaData,
              dbmeta,cs);
              //registerOutParameters(cs);
              } else {
              final String msgEx = JDBCDenormalizer.mMessages.getString(
              "JDBCDN_Failed_Finding_Node") +
              elementQName.getLocalPart() +
              ", in the part wrapper";
              JDBCDenormalizer.mLogger.log(Level.WARNING, msgEx);
              throw new MessagingException(msgEx);
              }*/
            }
          }
        } else {
          final Element messageElement = normalRoot;

          if (messageElement != null) {
            final Map parts = inputMessage.getParts();
            final Iterator it = parts.values().iterator();

            while (it.hasNext()) {
              final Part part = (Part) it.next();

              final Element partElement = findPart(messageElement,
                      part.getName());

              if (partElement != null) {
                final QName elementQName = part.getElementName();

                if (elementQName != null)
                  element = findChildElement(partElement,
                          elementQName);
              }
            }
          }
        }
      }
    } catch (final TransformerConfigurationException ex) {
      final String msg = JDBCDenormalizer.mMessages.getString(
              "DBBC_E00708.DN_Failed_Convert_NM");
      throw new MessagingException(msg, ex);
    } catch (final TransformerException ex) {
      final String msg = JDBCDenormalizer.mMessages.getString(
              "DBBC_E00708.DN_Failed_Convert_NM");
      throw new MessagingException(msg, ex);
    } catch (final Throwable th) {
      final String msg = JDBCDenormalizer.mMessages.getString(
              "DBBC_E00712.DN_Failed_Proc");
      throw new MessagingException(msg, th);
    }
    return element;
  }

  protected void setDatabaseName(String databaseName) {
    dbName = databaseName;
  }

  protected ArrayList getOutParamIndex() {
    return outParamIndex;
  }

  protected HashMap getOutParamTypes() {
    return outParamTypes;
  }

  protected HashMap getOutParamNames() {
    return outParamNames;
  }

  /*
   * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
   */
  /*
   * Modified by Logicoy
   * Added reduceParamCountFromParamMetaData
   */
  protected ArrayList<CachedQueryParameter> cacheQueryParameters(List<String> paramNames, ParameterMetaData paramMeta, boolean reduceParamCountFromParamMetaData) throws SQLException {
    int size = paramNames.size();
    int paramCount = paramMeta.getParameterCount();
    if (reduceParamCountFromParamMetaData)
      paramCount = paramCount - 1;
    if (size != paramCount)
      return null;

    ArrayList<CachedQueryParameter> params =
            new ArrayList<CachedQueryParameter>(size);
    for (int i = 0; i < size; ++i) {
      final int metaIdx = i + 1;
      String name = paramNames.get(i);
      int type = -1;
      try { //Oracle doesn't support this feature. Shame on them!
        type = paramMeta.getParameterType(metaIdx);
      } catch (SQLException e) {
        type = (int) JDBCUtil.builtInTypes.get(mColNamesTypes.get(name));
      }
      /*String typeName = null;
      try{ // ... and this
      typeName = paramMeta.getParameterTypeName(metaIdx);
      }
      catch(SQLException e){
      }
      int direction = paramMeta.getParameterMode(metaIdx);
      short precision = (short) paramMeta.getPrecision(metaIdx);*/
      CachedQueryParameter cp = new CachedQueryParameter(name, type);
      params.add(cp);
    }

    return params;
  }

  protected ArrayList<CachedQueryParameter> cacheEmptyQueryParams() {
    return new ArrayList<CachedQueryParameter>(0);
  }

  protected ArrayList<CachedQueryParameter> cacheQueryParameters(ResultSet paramsInfo) throws SQLException {
    if (paramsInfo == null)
      return null;

    ArrayList<CachedQueryParameter> params =
            new ArrayList<CachedQueryParameter>();
    while (paramsInfo.next()) {
      String name = paramsInfo.getString("COLUMN_NAME");
      int type = (int) java.sql.Types.VARCHAR;
      try {
        type = paramsInfo.getInt("DATA_TYPE");

      } catch (final SQLException e) {
        type = (int) JDBCUtil.builtInTypes.get(mColNamesTypes.get(name));
        if (mLogger.isLoggable(Level.FINEST))
          mLogger.log(Level.WARNING,
                  "DBBC_R00723.JDBCDN_DriverNoSupportForMetaData", e);
        else if (mLogger.isLoggable(Level.INFO))
          mLogger.log(Level.WARNING,
                  "DBBC_R00723.JDBCDN_DriverNoSupportForMetaData", e.
                  getLocalizedMessage());

      }
      String typeName = "VARCHAR";
      try {
        typeName = paramsInfo.getString("TYPE_NAME");
      } catch (final SQLException e) {
        //typeName = JDBCUtil.builtInTypes.get(mColNamesTypes.get(name));
        if (mLogger.isLoggable(Level.FINEST))
          mLogger.log(Level.WARNING,
                  "DBBC_R00723.JDBCDN_DriverNoSupportForMetaData", e);
        else if (mLogger.isLoggable(Level.INFO))
          mLogger.log(Level.WARNING,
                  "DBBC_R00723.JDBCDN_DriverNoSupportForMetaData", e.
                  getLocalizedMessage());

      }
      int direction = Integer.valueOf(ParameterMetaData.parameterModeIn);
      try {
        direction = paramsInfo.getInt("COLUMN_TYPE");
      } catch (final SQLException e) {
        if (mLogger.isLoggable(Level.FINEST))
          mLogger.log(Level.WARNING,
                  "DBBC_R00723.JDBCDN_DriverNoSupportForMetaData", e);
        else if (mLogger.isLoggable(Level.INFO))
          mLogger.log(Level.WARNING,
                  "DBBC_R00723.JDBCDN_DriverNoSupportForMetaData", e.
                  getLocalizedMessage());

      }
      short precision = 0;
      try {
        precision = paramsInfo.getShort("SCALE");
      } catch (final SQLException e) {
        if (mLogger.isLoggable(Level.FINEST))
          mLogger.log(Level.WARNING,
                  "DBBC_R00723.JDBCDN_DriverNoSupportForMetaData", e);
        else if (mLogger.isLoggable(Level.INFO))
          mLogger.log(Level.WARNING,
                  "DBBC_R00723.JDBCDN_DriverNoSupportForMetaData", e.
                  getLocalizedMessage());

      }
      CachedQueryParameter cp = new CachedQueryParameter(name, type,
              typeName, direction, precision);
      params.add(cp);
    }

    return params;
  }
}
