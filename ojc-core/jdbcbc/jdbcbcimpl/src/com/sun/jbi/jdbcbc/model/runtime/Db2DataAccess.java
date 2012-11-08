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
 * @(#)Db2DataAccess.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.model.runtime;

import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;

import java.util.logging.Logger;


/**
 * @author  Venkat P
 *
 */
public class Db2DataAccess implements DatabaseModel {
    private static final Messages mMessages = Messages.getMessages(Db2DataAccess.class);
    private static final Logger mLogger = Messages.getLogger(Db2DataAccess.class);

	private Db2DataAccess(){
    	
    }
    private static final Db2DataAccess instance = new Db2DataAccess();
    
    public static final Db2DataAccess getInstance(){
    	return instance; 
    }

    //@Override
    public String createQualifiedQuery(final String souTable, final String tarTable,
        final String flagName, final String markcolumnValue, final String pkName, final String operation,
		final String FlagColumnType)
        throws Exception {
        String Sql = null;

        try {
            if (operation.equals("UPDATE")) {
				if(FlagColumnType.equalsIgnoreCase("LONGVARCHAR") || FlagColumnType.equalsIgnoreCase("CHAR")
									|| FlagColumnType.equalsIgnoreCase("VARCHAR")){
	                Sql = "update " + souTable + " set " + flagName + " = " +
		               "'"+markcolumnValue+"'"+ " where" + souTable + "." + pkName;
				}else{
				    Sql = "update " + souTable + " set " + flagName + " = " +
		                markcolumnValue + " where" + souTable + "." + pkName;
				}
            } else if (operation.equals("DELETE")) {
                Sql = "delete from " + souTable + " where " + souTable + "." +
                    pkName;
            } else if (operation.equals("INSERT")) {
                Sql = "insert into " + tarTable + " select * from " + souTable +
                    " where " + souTable + "." + pkName;
            } else if (operation.equals("SELECT")) {
                Sql = "select * from " + "\"" + souTable + "\"";
            }
        } catch (final Exception e) {
            mLogger.log(Level.SEVERE,mMessages.getString("Derby_Create_Query"),e);
            throw new Exception(Db2DataAccess.mMessages.getString("Derby_Create_Query"), e);
        }

        return Sql;
    }

    //@Override
    public String generateSelectQuery(final String selectSql, final String TableName)
        throws Exception {
        String Sql = null;
        Sql = "select * from " + TableName;
        return Sql;
    }
}
//public class Db2DataAccess implements DatabaseModel{

//	private String mtabName = null;
//	private String mtrigName = null;
//	private TableColumn[] mcolumns = null;
//	private String mSchemaName = null;
//	private String mMarkColumnName = null;
//	private String mstageFlag = null;
//	private static final Messages mMessages = Messages.getMessages(Db2DataAccess.class);
//  private static Logger mLogger = Messages.getLogger(Db2DataAccess.class);

/**
         * @param souTable
         * @param schemaName
         * @param flagName
         */

//	public void init(Table souTable,String schemaName,String flagName,String stageFlag){
//		mMarkColumnName = flagName;
//		mstageFlag = stageFlag;
//		setTableName(souTable.getName());
//		setTriggerName(souTable.getName());		
//		mcolumns = souTable.getColumns();
//		this.mSchemaName = schemaName;

//	}	

/**
 * @throws Exception
 */

//	public String createTableQuery() throws Exception {
//		StringBuffer sb = new StringBuffer();
//		sb.append("create table ");
//		sb.append(" ");
//		sb.append(getTableName());
//		sb.append(" ");
//		sb.append("(");
//		for (int i = 0; i < this.mcolumns.length; i++) {
//			sb.append("\"" + this.mcolumns[i].getName() + "\"");
//			sb.append("  ");
//			sb.append(this.mcolumns[i].getSqlType());
//			if ((this.mcolumns[i].getNumericPrecision() != 0)
//					&& ((this.mcolumns[i].getSqlType())
//							.equalsIgnoreCase("VARCHAR"))) {
//				sb.append("(");
//				sb.append(this.mcolumns[i].getNumericPrecision());
//				sb.append(")");
//			}
//			sb.append("  ");
//			if (this.mcolumns[i].getIsPrimaryKey()) {
//				sb.append("primary key");
//			}
//			sb.append("  ");
//			if (this.mcolumns[i].getIsNullable()) {
//				sb.append("not null");
//			}
//			if ((i == (this.mcolumns.length - 1)))
//				sb.append(")");
//			else
//				sb.append(",");
//		}
//		mLogger.log(Level.INFO, mMessages.getString("Derby_Create_SQL") + sb.toString());
//		return sb.toString();
//	}

/**
 * @param source
 * @throws Exception
 */

//	public String createTriggerQuery(String source) throws Exception {
//		StringBuffer sb = new StringBuffer();
//		sb.append("CREATE TRIGGER ");
//		sb.append(" ");
//		sb.append(getTriggerName());
//		sb.append(" ");
//		sb.append("AFTER INSERT ON");
//		sb.append(" ");
//		sb.append(source);
//		sb.append(" ");
//		sb.append("REFERENCING NEW AS UPDATEDROW FOR EACH ROW MODE DB2SQL");
//		sb.append(" ");
//		sb.append("INSERT INTO");
//		sb.append(" ");
//		sb.append(getTableName());
//		sb.append(" ");
//		sb.append("VALUES");
//		sb.append(" ");
//		sb.append("(");
//		for (int i = 0; i < this.mcolumns.length; i++) {
//			sb.append("UPDATEDROW.");
//			sb.append("\"");
//			sb.append(this.mcolumns[i].getName());
//			sb.append("\"");
//			if ((i == (this.mcolumns.length - 1)))
//				sb.append(")");
//			else
//				sb.append(",");
//		}
//		mLogger.log(Level.INFO, mMessages.getString("Derby_Create_SQL") + sb.toString());
//		return sb.toString();
//	}	
//
//	public String getTableName(){
//		return (getSchemaName()+"."+this.mtabName);
//	}
//
//	public String getTriggerName(){
//		return (getSchemaName()+"."+this.mtrigName);
//	}
//
//	public String getSchemaName(){
//		return "\""+this.mSchemaName+"\"";
//	}
//
//	public void setTableName(String tabSource){
//	mLogger.log(Level.INFO, "Flag Name" + mMarkColumnName);
//		if(mMarkColumnName != null && mMarkColumnName != "" && mstageFlag.equalsIgnoreCase("Yes"))
//			this.mtabName = "\""+tabSource+"_jdbc_StaggingTable"+"\"";
//		else
//			this.mtabName = "\""+tabSource+"\"";
//	}
//
//	public void setTriggerName(String trigName){
//		if(mMarkColumnName != null && mMarkColumnName != "" && mstageFlag.equalsIgnoreCase("Yes"))
//			this.mtrigName = "\""+trigName+"_jdbc_staggingTrigger"+"\"";
//		else
//			this.mtabName = "\""+trigName+"\"";
//	}

/**
 * @param souTable
 * @param flagName
 * @param pkName
 * @param operation
 * @throws Exception
 */

//	public String createQualifiedQuery(String souTable, String flagName,
//			String pkName, String operation) throws Exception {
//		String Sql = null;
//		
//		try {
//			if (operation.equals("UPDATE")) {
//				Sql = "update " + souTable + " set " + flagName + " = 0 where"
//						+ souTable + "." + pkName;
//			} else if (operation.equals("DELETE")) {
//				Sql = "delete from  " + souTable + " where " + souTable + "."
//						+  pkName ;
//			} else if (operation.equals("INSERT")) {
//				Sql = "insert into " + "\"" + souTable + "\"" + "." + "\""
//						+ flagName + "_jdbc_StaggingTable" + "\""
//						+ " select * from " + pkName;
//			}
//		} catch (Exception e) {
//			throw new Exception( mMessages.getString("Derby_Create_Query"), e);
//		}
//		return Sql;
//	}
//}
