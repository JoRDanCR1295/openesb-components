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

package com.sun.jbi.jdbcbc;

import java.io.StringWriter;
import java.sql.CallableStatement;
import java.sql.DatabaseMetaData;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import com.sun.jbi.jdbcbc.model.metadata.DBMetaData;
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
import com.sun.jbi.jdbcbc.extensions.JDBCOperationInput;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import java.io.InputStream;
import java.io.Reader;
import java.util.Scanner;
import javax.xml.transform.stream.StreamSource;


/**
 *
 * JDBCDenormalizer
 *
 */
public class JDBCDenormalizer {
    private static final Messages mMessages = Messages.getMessages(JDBCDenormalizer.class);
    private static final Logger mLogger = Messages.getLogger(JDBCDenormalizer.class);
    private String dbName=null;
    private String catalog=null;
    private ArrayList outParamIndex = new ArrayList();
    private HashMap<Integer,String> outParamTypes = new HashMap<Integer,String>();
    private HashMap<Integer,String> outParamNames = new HashMap<Integer,String>(); 
    private String mRecordPrefix = null; //113494

    /**
     *
     * @param normalizedMessage
     * @param opMetaData
     * @param ps
     * @throws MessagingException
     */
    protected void denormalizeOutbound(final NormalizedMessage normalizedMessage,
        final OperationMetaData opMetaData, final PreparedStatement ps)
        throws MessagingException {
        if (opMetaData != null) {
            final JDBCOperationInput mJdbcOperationInput = opMetaData.getJDBCOperationInput();
            final String operationType = mJdbcOperationInput.getOperationType();

            if (JDBCUtil.opList.contains(operationType)) {
                try {
                    Element element = transformMessage(normalizedMessage,opMetaData);
                    if (element != null) {
                       populatePreparedStatement(element,opMetaData,ps);
                    }
                }  catch (final SQLException ex) {
                    final String msg = JDBCDenormalizer.mMessages.getString("SQLSE_E00709.JDBCDN_FailedPopulate_PS");
                    throw new MessagingException(msg, ex);
                } catch (final Exception ex) {
                    final String msg = JDBCDenormalizer.mMessages.getString("SQLSE_E00709.JDBCDN_FailedPopulate_PS");
                    throw new MessagingException(msg, ex);
                } 
            }
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
        final OperationMetaData opMetaData, final DatabaseMetaData dbmeta,final CallableStatement cs)
        throws MessagingException {
        if (opMetaData != null) {
            final JDBCOperationInput mJdbcOperationInput = opMetaData.getJDBCOperationInput();
            final String operationType = mJdbcOperationInput.getOperationType();

            if (JDBCUtil.dbOperations[JDBCUtil.dbOperations.length-1].equals(operationType)) {
                try {
                    Element element = transformMessage(normalizedMessage,opMetaData);
                    if (element != null) {
                         populateProcedure(element, opMetaData,dbmeta,cs);
						 //registerOutParameters(cs, dbmeta);
                    }
                } catch (final SQLException ex) {
                    final String msg = JDBCDenormalizer.mMessages.getString(
                            "SQLSE_E00711.JDBCDN_FailedPopulate_Proc");
                    throw new MessagingException(msg, ex);
                } catch (final Throwable th) {
                    final String msg = JDBCDenormalizer.mMessages.getString("SQLSE_E00712.JDBCDN_Failed_Proc");
                    throw new MessagingException(msg, th);
                }
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
        final OperationMetaData opMetaData, final DatabaseMetaData dbmeta,CallableStatement cs)
        throws SQLException, MessagingException, Exception {
        final JDBCOperationInput jdbcSql = opMetaData.getJDBCSql();
        String sqltext = jdbcSql.getSql();
        String procName = getProcName(sqltext);
        String pcatalog = "";
        String pschema = "";
        if(dbName != null) {
        	pschema = dbName;
        }
        if(catalog!=null){
        	pcatalog = catalog;
        }
        ResultSet rs = null;
        if (jdbcSql != null) {
        	
            try {
                    rs = dbmeta.getProcedureColumns(pcatalog, pschema, procName, "%");;
            } catch (final SQLException ex) {
                    mLogger.log(Level.WARNING, "Could not get Procedure Columns from DBMetaData", ex);
             }

            if (rs != null) {
            	int i = 0;
               while(rs.next()) {
//            	 set default type.
            	   int targetSqlType = java.sql.Types.VARCHAR;
	               	final String columnName = rs.getString("COLUMN_NAME");
	                targetSqlType = rs.getInt("DATA_TYPE");
	        		int colType = rs.getShort("COLUMN_TYPE");
	        		String type_Name = rs.getString("TYPE_NAME");
	        		     
	                final int columnNumber = i + 1;
	                if ( colType == DatabaseMetaData.procedureColumnIn) {
		    			if ((targetSqlType == 1111) && (type_Name.equals("PL/SQL TABLE"))) {
		    			targetSqlType = -14;
		    			}
		    			
		    			if ((targetSqlType == 1111) && (type_Name.equals("PL/SQL RECORD"))) {
		    			targetSqlType = -14;
		    			}
	                final Element columnElem = findMatchingColumnElement(columnName,
                        tableElem, mRecordPrefix); //113494 
	
	                if (columnElem != null) {
	                    final String value = DOMUtils.getChildCharacterData(columnElem);
	                    
	                       	if ((value != null) && !value.trim().equals("")) {
	                       		cs.setObject(columnNumber,
	                                  JDBCUtil.convert(value, targetSqlType), targetSqlType);
	                          }else {
	                        	  cs.setNull(columnNumber,targetSqlType);
	                          }
	                    }//end of if
	                }//end of if	
	                
	                if (colType == DatabaseMetaData.procedureColumnInOut || colType == DatabaseMetaData.procedureColumnOut) {
	        			try {
	        			// if the parameter is a cursor type, add its index to the arraylist
	        			if ((targetSqlType == 1111) && (type_Name.equals("REF CURSOR"))) {
	        			targetSqlType = -10;
	        			}
	        			cs.registerOutParameter(columnNumber, targetSqlType);
	        			outParamIndex.add(Integer.valueOf(Double.valueOf(columnNumber).intValue()));
	        			outParamTypes.put(columnNumber, type_Name);
	        			outParamNames.put(columnNumber, columnName);
	        			} catch(SQLException e) {
	        			System.out.println(e.getMessage());
	        			e.printStackTrace();
	        			//throw e;
	        			}
	        			}
	        			
	        			// check if the parameter is RETURN type (i.e. it is a function)
	        			if (colType == DatabaseMetaData.procedureColumnReturn) {
	        			try {
	        			// if the parameter is a cursor type, add its index to the arraylist
	        			if ((targetSqlType == 1111) && (type_Name.equals("REF CURSOR"))) {
	        			targetSqlType = -10;
	        			}
	        			cs.registerOutParameter(columnNumber, targetSqlType);
	        			outParamIndex.add(Integer.valueOf(Double.valueOf(columnNumber).intValue()));
	        			outParamTypes.put(columnNumber, type_Name);
	        			outParamNames.put(columnNumber, columnName);
	        			} catch(SQLException e) {
	        			System.out.println(e.getMessage());
	        			e.printStackTrace();
	        			
	        			//throw e;
	        			}
	        			}
	                	
	                    i++;
	                }//end of While
            }// end of If
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
    private void populatePreparedStatement(final Element tableElem,
        final OperationMetaData opMetaData, final PreparedStatement ps)
        throws SQLException, MessagingException, Exception {
        final JDBCOperationInput jdbcSql = opMetaData.getJDBCSql();

        if (jdbcSql != null) {
            ParameterMetaData paramMetaData = null;

            try {
                paramMetaData = ps.getParameterMetaData();
            } catch (final SQLException ex) {
                ex.printStackTrace();
            }

            //113494  start
            mRecordPrefix = opMetaData.getJDBCSql().getTableName();
            // see if the table name is null (SQL SE)
            // then take the query name as the prefix
            if (mRecordPrefix == null) {
                mRecordPrefix = opMetaData.getJDBCOperationOutput().getName();
                mRecordPrefix = mRecordPrefix.substring(0, mRecordPrefix.indexOf("Response"));
            }
            //113494  end
            if (paramMetaData != null) {
				final int parameters = paramMetaData.getParameterCount();

				if (parameters != 0) {// If there are no parameter we do not need to set anything  
									  // on Prepared statement
					String paramOrder = jdbcSql.getParamOrder();

					// changed for SQLSE since the user cannot enter param
					// ordering in sqlprojects.
					// we should generate a default param ordering.
					if (paramOrder == null) {
						paramOrder = getDefaultParameterOrderString(paramMetaData);
					}

					final List columns = extractColumns(paramOrder);

					if (columns.size() == parameters) {
						for (int i = 0; i < parameters; i++) {
							final String columnName = (String) columns.get(i);
							final int columnNumber = i + 1;
							final Element columnElement = findMatchingColumnElement(
									columnName, tableElem, mRecordPrefix);  //113494 Issue

							if (columnElement != null) {
								final String value = DOMUtils
										.getChildCharacterData(columnElement);

								if ((value != null) && !value.trim().equals("")) {
									 // set default type.
                                                                         int columnType = java.sql.Types.VARCHAR;

									try {
										columnType = paramMetaData
												.getParameterType(columnNumber);
									} catch (final SQLException e) {
										mLogger.log(Level.WARNING, "Driver Does not support getting the Parameter Metadata", e);
									}

									ps.setObject(columnNumber, JDBCUtil
											.convert(value, columnType),
											columnType);
								}else{
									 // set default type.
                                                                         int columnType = java.sql.Types.VARCHAR;
									try {
										columnType = paramMetaData
												.getParameterType(columnNumber);
									} catch (final SQLException e) {
										mLogger.log(Level.WARNING, "Driver Does not support getting the Datatypes", e);
									}
									ps.setNull(columnNumber,columnType);
								}


							}
						}
					} else {
						final String msg = JDBCDenormalizer.mMessages
								.getString("SQLSE_E00714.JDBCDN_Failed_PS_Param");
						throw new MessagingException(msg);
					}
				}
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
			///////////////////////////////////
			if ((child instanceof Element) &&
					child.getLocalName().equalsIgnoreCase(recordPrefix + "_Record")) { // 113494 Issue
					columnElem = (Element) child;
				    childNodes = columnElem.getChildNodes();
				}
			///////////////////////////////////
		 }		
        for (int i = 0; i < childNodes.getLength(); i++) {
            final Node child = childNodes.item(i);
				if ((child instanceof Element) &&
						child.getLocalName().equalsIgnoreCase(dbColumnName)) {
					columnElem = (Element) child;
					break;
				}
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
            final Scanner tok = new Scanner(paramOrder).useDelimiter("\\s*" + "," + "\\s*");

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
    private Element findChildElement(final Element parent, final QName msgQName) {
        final String ns = msgQName.getNamespaceURI();
        final String localName = msgQName.getLocalPart();
        NodeList nl = null;

        if ((ns != null) && !ns.trim().equals("")) {
            nl = parent.getElementsByTagNameNS(ns, localName);
        } else {
            nl = parent.getElementsByTagName(localName);
        }

        if ((nl != null) && (nl.getLength() > 0)) {
            if (JDBCDenormalizer.mLogger.isLoggable(Level.INFO)) {
                JDBCDenormalizer.mLogger.log(Level.INFO, "found element with name, " +
                    localName);
            }

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
    private Element findPart(final Element root, final String elemName) {
        // parts wrappers never have namespace
        final NodeList nl = root.getElementsByTagName(elemName);

        if ((nl != null) && (nl.getLength() > 0)) {
            if (JDBCDenormalizer.mLogger.isLoggable(Level.INFO)) {
                JDBCDenormalizer.mLogger.log(Level.INFO, "found element with name, " + elemName);
            }

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
    private String getDefaultParameterOrderString(final ParameterMetaData pmeta) {
        String parameterOrderString = null;
        int numParams = 0;

        if (pmeta != null) {
            try {
                numParams = pmeta.getParameterCount();
            } catch (final SQLException sqle) {
                JDBCDenormalizer.mLogger.log(Level.WARNING,
                    JDBCDenormalizer.mMessages.getString("JDBCDN_Failed_ParamCount"));

                return null;
            }

            if (numParams > 0) {
                for (int i = 1; i <= numParams; i++) {
                	final String paramname = "param" + String.valueOf(i);
                    parameterOrderString = (parameterOrderString == null)
                        ? paramname : (parameterOrderString + "," + paramname);
                }
            }
        }

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
	    int paramIndex=0;
	    try {
	    	ParameterMetaData pmeta = cstmt.getParameterMetaData();
            if (pmeta != null) {
                int numParams = pmeta.getParameterCount();
                if (numParams > 0) {
                    // get info for each parameter
                    for (int i = 1; i <= numParams; i++) {
                        // try to get the sql type info - default to VARCHAR
                        String sqlType = "VARCHAR";
                        try {
                            sqlType = DBMetaData.getSQLTypeDescription(pmeta.getParameterType(i));
                        } catch (SQLException e) {
                            mLogger.log(Level.WARNING, "Could not get SQL Type Description from DBMetadata", e);
                        }
                        
                        // try to get the java type info - default to String
                        /**
                         * Changing it to not use metadata class name and instead use the HashMap SQLTOJAVATYPES.
                         * Without the change the parameter datatypes java.lang.Double and WSDLGenerator look up list
                         * exepects native type double, float, short etc.
                         **/
                        String javaType = "java.lang.String";
                        javaType = DBMetaData.getJavaFromSQLTypeDescription(sqlType);
                        
//                      added abey for Procedure ResultSet
                        try{
    				    if((pmeta.getParameterType(i)==java.sql.Types.OTHER)&&(pmeta.getParameterTypeName(i).equalsIgnoreCase("REF CURSOR"))){
    				        sqlType = "RESULTSET";
    				        javaType = "java.sql.ResultSet";
    				    }
                        }catch (SQLException e) {
                            mLogger.log(Level.WARNING, "Could not get Java type information from DBMetadata", e);
                        }
                        
                        // try to get the param type, default to IN
                        // always default it since getParameterMode() in data direct 3.3 throws exception
                        // and 3.4 return UNKNOWN type
                        String paramType = "IN";
                        
                        try {
                            paramType = DBMetaData.getPrepStmtParamTypeDescription(pmeta.getParameterMode((i)));
                        } catch (SQLException e) {
                               mLogger.log(Level.WARNING, "Could not get PreparedStatement Parameter Description", e);                        
                        }
                        
//                      set defalut type
    	            	int sqlTypeCode = java.sql.Types.VARCHAR;
                        if (paramType.equalsIgnoreCase("INOUT") || paramType.equalsIgnoreCase("OUT")) {
            	            try {
                                
            	                // if the parameter is a cursor type, add its index to the arraylist
            	                if ((pmeta.getParameterType(i) == 1111) && (paramType.equals("OTHER"))) {
            	                    sqlTypeCode = java.sql.Types.OTHER;	                    
            	                }
            	                
            	            } catch(SQLException e) {
            	                mLogger.log(Level.WARNING, "Driver Does not support getting the Datatypes", e);
            	            }
            	        }
                        if (paramType.equals("RETURN")) {
            	            try {
            	            	// if the parameter is a cursor type, add its index to the arraylist
            	                if ((pmeta.getParameterType(i) == 1111) && (paramType.equals("OTHER"))) {
            	                    //sqlTypeCode = java.sql.Types.OTHER;
            	                   
            	                }
            	                
            	            } catch(SQLException e) {
            	                mLogger.log(Level.WARNING, "Driver Does not support getting the Datatypes", e);
            	            }
            	        }
                        cstmt.registerOutParameter(paramIndex, sqlTypeCode);
                        outParamIndex.add(Integer.valueOf(Double.valueOf(paramIndex).intValue()));
                        
                        
                    }
                }
            }
	    	        
	        
	        
	        
	        
	    }catch(SQLException e) {
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
    private String getProcName(String sqlText) {
    	String proc_name = "";
    	String schema = "";
    	final StringTokenizer tok = new StringTokenizer(sqlText, " ");
		
        while (tok.hasMoreElements()) {
            String column = (String) tok.nextElement();
            int cnt = 0;
            column=column.toLowerCase();
            if(column.endsWith("call")){
            	cnt++;
            	proc_name=(String)tok.nextElement();
            	if(proc_name.contains(".")){
            		final StringTokenizer tok1 = new StringTokenizer(proc_name, ".");
            		catalog=tok1.nextToken();
            		proc_name=tok1.nextToken();
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
    
    private final Element transformMessage(final NormalizedMessage normalizedMessage,
        final OperationMetaData opMetaData) throws MessagingException{
        Element element  = null;
        try {
                    final TransformerFactory tFactory = TransformerFactory.newInstance();
                    final Transformer trans = tFactory.newTransformer();
                    final Source source = normalizedMessage.getContent();
                    final DOMResult result = new DOMResult();
                    trans.transform(source, result);

                    if (source instanceof StreamSource) {
                        StreamSource stream = (StreamSource) source;
                        InputStream inputStream = stream.getInputStream();
                        if (inputStream != null) {
                            inputStream.reset();
                        }
                        Reader reader = stream.getReader();
                        if (reader != null) {
                            reader.reset();
                        }
                    }

                    final Node node = result.getNode();
                    final StringWriter strWriter = new StringWriter();
                    final StreamResult sResult = new StreamResult(strWriter);
                    trans.transform(source, sResult);

                    if (source instanceof StreamSource) {
                        StreamSource stream = (StreamSource) source;
                        InputStream inputStream = stream.getInputStream();
                        if (inputStream != null) {
                            inputStream.reset();
                        }
                        Reader reader = stream.getReader();
                        if (reader != null) {
                            reader.reset();
                        }
                    }                    
                    if (node != null) {
                        Document normalDoc = null;

                        if (node instanceof Document) {
                            normalDoc = (Document) node;
                        } else {
                            normalDoc = ((Element) node).getOwnerDocument();
                        }

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
                                                "SQLSE_E00706.JDBCDN_Failed_Denormalize") +
                                            part.getName() +
                                            "should have element attribute defined.";
                                        throw new MessagingException(msgEx);
                                    }

                                    //Element element = null;
                                    final NodeList unwrappedList = wrapperParser.getPartNodes(part.getName());

                                    for (int j = 0;
                                            j < unwrappedList.getLength();
                                            j++) {
                                        final Node unwrapped = unwrappedList.item(j);

                                        if ((unwrapped.getNodeType() == Node.ELEMENT_NODE) &&
                                                (unwrapped.getLocalName() != null) &&
                                                unwrapped.getLocalName()
                                                             .equals(elementQName.getLocalPart())) {
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

                                        if (elementQName != null) {
                                            element = findChildElement(partElement,
                                                    elementQName);

                                            /*if (element != null) {
                                                populateProcedure(element,
                                                    opMetaData, dbmeta,cs);
                                                //registerOutParameters(cs);
                                            }*/
                                        }
                                    }
                                }
                            }
                        }
                    }
                } catch (final TransformerConfigurationException ex) {
                    final String msg = JDBCDenormalizer.mMessages.getString("SQLSE_E00708.JDBCDN_Failed_Convert_NM");
                    throw new MessagingException(msg, ex);
                } catch (final TransformerException ex) {
                    final String msg = JDBCDenormalizer.mMessages.getString("SQLSE_E00708.JDBCDN_Failed_Convert_NM");
                    throw new MessagingException(msg, ex);
                } catch (final Throwable th) {
                    final String msg = JDBCDenormalizer.mMessages.getString("SQLSE_E00712.JDBCDN_Failed_Proc");
                    throw new MessagingException(msg, th);
                }
        return element;
    }
    
    protected void setDatabaseName(String databaseName){
    	dbName = databaseName;
    }
    
    protected ArrayList getOutParamIndex(){
    	return outParamIndex;
    }
    
    protected HashMap getOutParamTypes(){
    	return outParamTypes;
    }
    
    protected HashMap getOutParamNames(){
    	return outParamNames;
    }
    
}
