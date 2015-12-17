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
 * @(#)JDBCUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import java.io.IOException;
import java.math.BigDecimal;
import java.sql.Date;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.util.Base64Utils;
import com.sun.jbi.internationalization.Messages;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.sql.rowset.serial.SerialBlob;
import javax.sql.rowset.serial.SerialClob;
import java.sql.PreparedStatement;
import java.sql.ParameterMetaData;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;

/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCUtil {
    
    private static final Messages mMessages = Messages.getMessages(JDBCUtil.class);
    private static final Logger mLogger = Messages.getLogger(JDBCUtil.class);
    public static final String[] dbOperations = {"insert","INSERT","update","UPDATE","delete","DELETE","select","SELECT","create","CREATE",
    											 "find","FIND","poll","POLL","drop","DROP","truncate","TRUNCATE","execute","EXECUTE"};
    public static final List<String> opList = Arrays.asList(dbOperations);
    public static HashMap<String, Integer> builtInTypes = new HashMap<String, Integer>();
    private static Timestamp convertDateTimeField(String tmString) throws Exception {
        Timestamp tm = null;
        try {
            //try  parsing as a normal java sql string
            tm = Timestamp.valueOf(tmString);

        } catch (Exception e) {
            //Check if this is xsd:datetime format
            DatatypeFactory dtf = DatatypeFactory.newInstance();
            XMLGregorianCalendar cr = dtf.newXMLGregorianCalendar(tmString);
            tm = new Timestamp(cr.toGregorianCalendar().getTime().getTime());
        }
        return tm;
    }
    /**
     *
     * @param value
     * @param jdbcType
     * @return
     * @throws Exception
     */
    static Object convert(final String value, final int jdbcType, java.sql.Connection conn)
        throws Exception {
        Object convertedVal = null;

        try {
            switch (jdbcType) {
            case Types.BIGINT:
                convertedVal = Long.valueOf(value);

                break;

            case Types.BIT:
                convertedVal = Boolean.valueOf(value);

                break;

            case Types.BOOLEAN:
                convertedVal = Boolean.valueOf(value);

                break;

            case Types.DATE:
                try{
                    convertedVal = Date.valueOf(value);
                }
                catch(Throwable e){
                    convertedVal = javax.xml.bind.DatatypeConverter.parseDate(value).getTime();
                }

                break;

            case Types.DISTINCT:
                convertedVal = Date.valueOf(value);

                break;

            case Types.DOUBLE:
                convertedVal = Double.valueOf(value);

                break;

            case Types.DECIMAL:
            case Types.NUMERIC:
                convertedVal = new BigDecimal(value);

                break;

            case Types.FLOAT:
                convertedVal = Float.valueOf(value);

                break;

            case Types.INTEGER:
                convertedVal = Integer.valueOf(Double.valueOf(value).intValue());

                break;

            case Types.REAL:
                convertedVal = Float.valueOf(value);

                break;

            case Types.SMALLINT:
                convertedVal = Short.valueOf(value);

                break;

            case Types.TIMESTAMP:
                try{
                    convertedVal = Timestamp.valueOf(value);
                }
                catch(Throwable e){                    
                    convertedVal = new Timestamp(javax.xml.bind.DatatypeConverter.parseDateTime(value).getTime().getTime());
                }

                break;

            case Types.BINARY:
            case Types.VARBINARY:
                convertedVal = value.getBytes();
                break;
                
            //CLOB conversion    
            case Types.CLOB:
            case Types.LONGVARCHAR:
            	if(conn.getMetaData().getDriverName().toLowerCase().contains("oracle")){
	            	java.io.StringReader stringReader = new java.io.StringReader(value);  
	            	convertedVal = stringReader;
            	}else{
            	convertedVal = new SerialClob(value.toCharArray()); 
            	}
            	break;
            	
            //BLOB conversion	
            case Types.BLOB:
            case Types.LONGVARBINARY:
            	if(conn.getMetaData().getDriverName().toLowerCase().contains("oracle")){
            		java.io.ByteArrayInputStream byteInputStream  = new java.io.ByteArrayInputStream(Base64Utils.base64DecodeToByte(value));
            		convertedVal = byteInputStream;
            	}else{
                convertedVal = new SerialBlob(Base64Utils.base64DecodeToByte(value));
            	}
            	break;

            case Types.CHAR:
            case Types.VARCHAR:default:
                convertedVal = value;

                break;
            }
        } catch (final Exception ex) {
            if( (ex instanceof IOException) && (jdbcType == Types.BLOB || jdbcType == Types.LONGVARBINARY) ){
                JDBCUtil.mLogger.log(Level.INFO, "JDBCUtil_Base64_Decode_Failed");
                throw new Exception("Failed to Base64 decode the value to bytes. Blob is not set properly.");
            }
            JDBCUtil.mLogger.log(Level.INFO, "JDBCUtil_Failed_Convert", new Object[] { value, jdbcType });
            throw new Exception("Failed to convert value " + value + " to jdbc type " + jdbcType,ex);
        }

        return convertedVal;
    }
    
    /**
    *
    * @param value
    * @param jdbcType
    * @return
    * @throws Exception
    */
   static Object convert(final String value, final int jdbcType)
       throws Exception {
       Object convertedVal = null;

       try {
           switch (jdbcType) {
           case Types.BIGINT:
               convertedVal = Long.valueOf(value);

               break;

           case Types.BIT:
               convertedVal = Boolean.valueOf(value);

               break;

           case Types.BOOLEAN:
               convertedVal = Boolean.valueOf(value);

               break;

           case Types.DATE:
               try{                   
                    convertedVal = Date.valueOf(value);
                }
                catch(Throwable e){                   
                    convertedVal = javax.xml.bind.DatatypeConverter.parseDate(value).getTime();
                }

               break;

           case Types.DISTINCT:
               convertedVal = Date.valueOf(value);

               break;

           case Types.DOUBLE:
               convertedVal = Double.valueOf(value);

               break;

           case Types.DECIMAL:
           case Types.NUMERIC:
               convertedVal = new BigDecimal(value);

               break;

           case Types.FLOAT:
               convertedVal = Float.valueOf(value);

               break;

           case Types.INTEGER:
               convertedVal = Integer.valueOf(Double.valueOf(value).intValue());

               break;

           case Types.REAL:
               convertedVal = Float.valueOf(value);

               break;

           case Types.SMALLINT:
               convertedVal = Short.valueOf(value);

               break;

           case Types.TIMESTAMP:
               try{
                    convertedVal = Timestamp.valueOf(value);
                }
                catch(Throwable e){                    
                    convertedVal = new Timestamp(javax.xml.bind.DatatypeConverter.parseDateTime(value).getTime().getTime());
                }

               break;

           case Types.BINARY:
           case Types.VARBINARY:
               convertedVal = value.getBytes();
               break;
               
           //CLOB conversion    
           case Types.CLOB:
           case Types.LONGVARCHAR:
           	convertedVal = new SerialClob(value.toCharArray()); 
           	break;
           	
           //BLOB conversion	
            case Types.BLOB:
            case Types.LONGVARBINARY:
                convertedVal = new SerialBlob(Base64Utils.base64DecodeToByte(value));
            	break;

            case Types.CHAR:
            case Types.VARCHAR:default:
                convertedVal = value;

                break;
            }
        } catch (final Exception ex) {
            if( (ex instanceof IOException) && (jdbcType == Types.BLOB || jdbcType == Types.LONGVARBINARY) ){
                JDBCUtil.mLogger.log(Level.INFO, "JDBCUtil_Base64_Decode_Failed");
                throw new Exception("Failed to Base64 decode the value to bytes. Blob is not set properly.");
            }
            JDBCUtil.mLogger.log(Level.INFO, "JDBCUtil_Failed_Convert", new Object[] { value, jdbcType });
            throw new Exception("Failed to convert value " + value + " to jdbc type " + jdbcType);
        }

        return convertedVal;
    }
    public static String convertToString (int index , ResultSet rs ,ResultSetMetaData rsmd , String driverName)
	throws java.sql.SQLException, java.io.IOException{
		switch (rsmd.getColumnType(index))
		{
			case java.sql.Types.LONGVARCHAR:
				if (driverName.toLowerCase().contains("db2")){
					return rs.getString(index);
				} else if(driverName.toLowerCase().contains("oracle")){
                                    return rs.getString(index);
                                }
                                    //else fall through
			case java.sql.Types.CLOB:
				return LobHandler.getClob(index, rs, driverName);

			case java.sql.Types.LONGVARBINARY:
			case java.sql.Types.BLOB:
				return LobHandler.getBlob(index, rs, driverName);
			default:
				return rs.getString(index);
		}
		
		
	}
    static {
        // NOTE: CLOB not supported
        JDBCUtil.builtInTypes.put("xsd:base64Binary", Integer.valueOf(java.sql.Types.BINARY));
        JDBCUtil.builtInTypes.put("xsd:boolean", Integer.valueOf(java.sql.Types.BIT));
        JDBCUtil.builtInTypes.put("xsd:byte", Integer.valueOf(java.sql.Types.TINYINT));
        JDBCUtil.builtInTypes.put("xsd:decimal", Integer.valueOf(java.sql.Types.DECIMAL));
        JDBCUtil.builtInTypes.put("xsd:double", Integer.valueOf(java.sql.Types.DOUBLE));
        JDBCUtil.builtInTypes.put("xsd:float", Integer.valueOf(java.sql.Types.REAL));
        JDBCUtil.builtInTypes.put("xsd:hexBinary", Integer.valueOf(java.sql.Types.BLOB));
        JDBCUtil.builtInTypes.put("xsd:int", Integer.valueOf(java.sql.Types.INTEGER));
        JDBCUtil.builtInTypes.put("xsd:integer", Integer.valueOf(java.sql.Types.INTEGER));
        
        JDBCUtil.builtInTypes.put("xsd:long", Integer.valueOf(java.sql.Types.BIGINT));
        JDBCUtil.builtInTypes.put("xsd:short", Integer.valueOf(java.sql.Types.INTEGER));
        
        JDBCUtil.builtInTypes.put("xsd:string", Integer.valueOf(java.sql.Types.VARCHAR));
        JDBCUtil.builtInTypes.put("xsd:time", Integer.valueOf(java.sql.Types.TIME));
        JDBCUtil.builtInTypes.put("xsd:dateTime", Integer.valueOf(java.sql.Types.TIMESTAMP));
        JDBCUtil.builtInTypes.put("xsd:date", Integer.valueOf(java.sql.Types.DATE));

        // temporary for demo
        JDBCUtil.builtInTypes.put("xsd:base64Binary", Integer.valueOf(java.sql.Types.BLOB));
        JDBCUtil.builtInTypes.put("CLOB", Integer.valueOf(java.sql.Types.CLOB));
	JDBCUtil.builtInTypes.put("LONGVARCHAR", Integer.valueOf(java.sql.Types.CLOB));
        JDBCUtil.builtInTypes.put("BLOB", Integer.valueOf(java.sql.Types.BLOB));
	JDBCUtil.builtInTypes.put("LONGVARBINARY", Integer.valueOf(java.sql.Types.BLOB));
        // added by abey for Procedure with parameter of type RefCursor
        //JDBCUtil.builtInTypes.put("xsd:ResultSet", "java.sql.ResultSet");

    }
    
    public static final String getSQLStatementType(final String sqlText) {
            if(opList.contains(sqlText.split("\\s")[0]))
                return sqlText.split("\\s")[0];
            else return dbOperations[dbOperations.length-1];
            /*String opName = JDBCOperations.getOpType(sqlText);
            JDBCOperations jdbcOps = JDBCOperations.getJDBCOperations(opName);
            return jdbcOps.toString();*/
    }

    public static void bindParams(PreparedStatement ps, String... params) throws Exception
    {
        ParameterMetaData meta = ps.getParameterMetaData();
        for (int i = 0; i < params.length; i++)
        {
            int columnType = java.sql.Types.VARCHAR;
            try { columnType = meta.getParameterType(i+1); } catch(Exception e) {}
            ps.setObject(i+1, JDBCUtil.convert(params[i], columnType), columnType);
        }
    }
}
