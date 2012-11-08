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
 * @(#)InitTask.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.etl.engine.impl;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.sun.etl.engine.ETLEngine;
import com.sun.etl.engine.ETLTask;
import com.sun.etl.engine.ETLTaskNode;
import com.sun.etl.runtime.RuntimeUtil;
import com.sun.etl.engine.utils.ETLException;
import com.sun.etl.exception.BaseException;
import com.sun.etl.jdbc.DBConnectionFactory;
import com.sun.etl.jdbc.DBConnectionParameters;
import com.sun.etl.jdbc.SQLPart;
import com.sun.etl.jdbc.SQLUtils;
import com.sun.etl.utils.Logger;
import com.sun.etl.utils.RuntimeAttribute;
import com.sun.etl.utils.ScEncrypt;
import com.sun.etl.utils.StringUtil;
import java.io.File;
import java.util.HashMap;
import com.sun.jbi.internationalization.Messages;

/**
 * Init Task created by default now. But this should be a specialized task for only Flat file DB.
 * 
 * @author Sudhi Seshachala
 */
public class InitTask extends SimpleTask {
    
    private static final Messages mMessages = Messages.getMessages(InitTask.class);    

    private final static String LOG_CATEGORY = InitTask.class.getName();

    private static final Pattern PASSWORD_PATTERN = Pattern.compile("PASSWORD\\ *=\\ *'([^']*)'");

    private static final Pattern USERNAME_PATTERN = Pattern.compile("USERNAME\\ *=\\ *'([^']*)'");
    
    private String dataDir = null;
    
    private String workingDir = null;
    
    private boolean isDynamicFlatFileEnabled = false;
    private boolean disableMonitor = false;
    
    private HashMap designTimeAttrs;

    
    
    /**
     * @param node Process the given ETLTaskNode
     * @exception Exception caused during Process
     * @return Return Success or Failure String
     */
    public String process(ETLTaskNode node) throws ETLException {
        if (node == null) {
            throw new ETLException(DN + mMessages.getString("ETLSE-E0439.InitTaskNode_is_null"));
        }
        ETLEngine engine = node.getParent();
        dataDir = (String) engine.getContext().getValue("DATA_DIR");
        workingDir = (String) engine.getContext().getValue("WORKING_DIR");
        String dynamicFlatFile = (String) engine.getContext().getValue("DYNAMIC_FLAT_FILE");
        isDynamicFlatFileEnabled = (dynamicFlatFile == null) ? false : Boolean.parseBoolean(dynamicFlatFile);
        
        String monitorFlag = System.getProperty("JAVACAPS_DATAINTEGRATOR_DISABLE_MONITOR");
        disableMonitor = (monitorFlag == null) ? false : Boolean.parseBoolean(monitorFlag);
        


        designTimeAttrs = (HashMap) engine.getContext().getValue("DESIGN_TIME_ATTRS");
        designTimeAttrs = (designTimeAttrs == null) ? new HashMap() : designTimeAttrs;
        
        if (!StringUtil.isNullString(node.getDisplayName())) {
            DN += " <" + node.getDisplayName().trim() + ">";
        }

        List conDefList = engine.getConnectionDefList();
       
        Logger.print(Logger.DEBUG, LOG_CATEGORY, DN + "Processing init task...");

        try {
            boolean isInitDone = false;
            //checked the timings for this block. After the first hit, 
            //its in the range of 20-40 milliseconds and settles down 
            //to <20 ms. This should not cause any bottleneck as the same
            //amount of time will be spent in createsummarytable method otherwise
            //which is more costly operation. Ran 25 iterations with 9 instances
            //of three different colloborations for verification. 
            
            if (!disableMonitor) {
                synchronized (InitTask.class) {
                    isInitDone = doesSummaryTableExists(node);
                    if (!isInitDone) {
                        super.createSummaryTable(node);
                    }
                }
            }

            List sqlParts = node.getOptionalTasks();
            if (sqlParts.size() != 0) {
                Map rtAttributes = node.getParent().getInputAttributes();
                execute(sqlParts, rtAttributes, conDefList,isInitDone);
            } else {
                Logger.print(Logger.DEBUG, LOG_CATEGORY, "No initialization required.");
            }
        } catch (Exception e) {
            Logger.print(Logger.ERROR, LOG_CATEGORY, DN + mMessages.getString("ETLSE-E0440.Failed_to_execute_InitTask"), e);
            throw new ETLException(DN + mMessages.getString("ETLSE-E0440.Failed_to_execute_InitTask"), e);
        }

        return ETLTask.SUCCESS;
    }
    private boolean doesSummaryTableExists(ETLTaskNode node) throws ETLException {
        List sqlParts = node.getOptionalTasks();
        Connection conn = null;
        boolean tableExists = false;
       
        if( sqlParts.size() != 0 ) {
            Iterator iter = sqlParts.iterator();
            if (iter.hasNext()) {
                SQLPart part = (SQLPart) iter.next();
                String sql = part.getSQL();
                String poolName = part.getConnectionPoolName();
                try {
                    conn = this.getConnection(poolName, node.getParent().getConnectionDefList());
                    tableExists = super.isTableExists(conn, "SELECT 1 FROM SUMMARY",node);
                } catch(BaseException e ) {
                    Logger.print(Logger.ERROR, LOG_CATEGORY, DN + "Failed to execute InitTask:doesSummaryTableExists", e);
                    throw new ETLException(DN + "Failed to execute InitTask:doesSummaryTableExists", e);
                } finally {
                    DBConnectionFactory.getInstance().closeConnection(conn);
                }
            }
         }
         return tableExists;
    }

    private String associateDynamicFileWithTable(SQLPart sqlPart, Map inputAttrs) throws Exception {
        Iterator iter = inputAttrs.entrySet().iterator();
        String rawSQL = sqlPart.getSQL();
        while (iter.hasNext()) {
            Map.Entry e = (Map.Entry) iter.next();
            String keyName = (String) e.getKey();
            if(rawSQL.indexOf(keyName) != -1) {
                String value = (String) e.getValue();
                if (StringUtil.isNullString(value)) {
                    if(!StringUtil.isNullString(sqlPart.getDefaultValue())) {
                        value = sqlPart.getDefaultValue();
                    } else {
                        continue;
                    }                
                }
                value = StringUtil.escapeJavaLiteral(value);
                String resolvedPath = resolveInputFile(value, keyName);
                Logger.print(Logger.DEBUG,LOG_CATEGORY,"value = " + value + "keyName = " + keyName + "resolvedPath = " + resolvedPath );
                value = (resolvedPath == null) ? value:StringUtil.escapeJavaLiteral(resolvedPath);
                rawSQL = StringUtil.replaceAll(rawSQL, value, "\\$" + keyName);
            }
        }
        return rawSQL;
    }

    /**
     * @param rawSql
     * @return
     */
    private String decryptDbLinkSql(String rawSql) {
        String processedSql = rawSql;
        String userName = null;
        String encryptedPassword = null;
        String password = null;

        Matcher matcher = USERNAME_PATTERN.matcher(rawSql);
        if (matcher.find()) {
            userName = matcher.group(1);
            if (!StringUtil.isNullString(userName)) {
                matcher = PASSWORD_PATTERN.matcher(rawSql);
                if (matcher.find()) {
                    encryptedPassword = matcher.group(1);
                    try {
                    	Map resolved = RuntimeUtil.getLDAPResolvedParameters(userName, encryptedPassword);
                    	userName = (String) resolved.get(DBConnectionParameters.USER_NAME_ATTR);
                    	encryptedPassword = (String) resolved.get(DBConnectionParameters.PASSWORD_ATTR);
                    if (!StringUtil.isNullString(encryptedPassword)) {
                            password = ScEncrypt.decrypt(userName, encryptedPassword);
                        processedSql = matcher.replaceFirst("PASSWORD='" + StringUtil.escapeJavaRegexpChars(password) + "'");
                    }
                    	
                    	if (!StringUtil.isNullString(userName)) {
                    		matcher = USERNAME_PATTERN.matcher(processedSql);
                            processedSql = matcher.replaceFirst("USERNAME='" + StringUtil.escapeJavaRegexpChars(userName) + "'");
                }
                    }catch(Exception ex){
                    	Logger.printThrowable(Logger.ERROR, LOG_CATEGORY, this, DN + "InitTask:decryptDbLinkSql", ex);                    	
            }
        }
            }
        }

        return processedSql;
    }

    private synchronized void doExecute(SQLPart sqlPart, List connDefs, boolean useAutoCommit) throws SQLException, BaseException {
        Connection conn = null;
        Statement stmt = null;
        boolean oldCommitType = false;

        String sql = sqlPart.getSQL();
        String poolName = sqlPart.getConnectionPoolName();
        try {
            if (sql != null) {
                conn = this.getConnection(poolName, connDefs);

                if (useAutoCommit) {
                    oldCommitType = conn.getAutoCommit();
                    conn.setAutoCommit(true);
                }

                stmt = conn.createStatement();
                Logger.print(Logger.DEBUG, LOG_CATEGORY, sql);
                stmt.executeUpdate(sql);
            }
            // FIXME Should capture and somehow figure out if SQLException here is a
            // serious exception
        } finally {
            if (useAutoCommit) {
                conn.setAutoCommit(oldCommitType);
            }
            closeStatement(stmt);
            DBConnectionFactory.getInstance().closeConnection(conn);
        }
    }
    
    private String resolveInputFile(String filePath, String attrName) {
        Logger.print(Logger.DEBUG, LOG_CATEGORY,"dumping designtime map = " + this.designTimeAttrs.toString());
        
        if (filePath == null || "".equalsIgnoreCase(filePath)) {
            return null;
        }

        if (!this.isDynamicFlatFileEnabled && (dataDir == null || "".equalsIgnoreCase(dataDir)))  {
            return filePath;
        }
    
        String resolvedFilePath = filePath;
        String designTimeDataDir = null;
        RuntimeAttribute designTimePath = (RuntimeAttribute ) this.designTimeAttrs.get(attrName);
        if(designTimePath != null) {
            File f = new File(designTimePath.getAttributeValue());
            if(f.exists()) {
                designTimeDataDir = f.getParentFile().getPath(); 
            }
            Logger.print(Logger.DEBUG, LOG_CATEGORY,"designtime folder resolved as " + designTimeDataDir);
        }
        if(!this.isDynamicFlatFileEnabled) {
            resolvedFilePath = this.dataDir + File.separator + filePath;
            if(! new File(resolvedFilePath).exists() && designTimeDataDir != null) {
                resolvedFilePath = designTimeDataDir + File.separator + filePath;
            }
        } else if(! new File(filePath).exists() && designTimeDataDir != null ) {
            resolvedFilePath = designTimeDataDir + File.separator + new File(filePath).getName();
            
        }
        Logger.print(Logger.DEBUG, LOG_CATEGORY,"filePath = " + filePath + "attrName = " + attrName + " resolvedFilePath = " + resolvedFilePath);
        return resolvedFilePath;
    }
    
    private void runtimeOverrideDBLink(SQLPart dblinkPart, List conDefList) throws Exception {
        javax.sql.DataSource ds = null;
        String url = dblinkPart.getSQL();
        String tableName = "";
        String jndiResName = "";
        String driverClass = "";
        String linkName = "";
              
        Iterator conIter = conDefList.iterator();
        while (conIter.hasNext()) {
            DBConnectionParameters condef = (DBConnectionParameters) conIter.next();
            if (url.indexOf(condef.getName().toUpperCase()) != -1) {
                jndiResName = condef.getJNDIPath();
                tableName = condef.getName();
                driverClass = condef.getDriverClass();
                linkName = condef.getName();
                break;
            }
        }
        if (jndiResName != null && !"".equalsIgnoreCase(jndiResName)) {
            try {
                String dbLinkSql = SQLUtils.createDBLinkSQL(linkName,jndiResName);
                Logger.print(Logger.DEBUG, LOG_CATEGORY, "dbLinkSql is " + dbLinkSql);
                dblinkPart.setSQL(dbLinkSql);
            } catch (Exception e) {
                Logger.printThrowable(Logger.ERROR, LOG_CATEGORY, this, DN + "InitTask:runtimeOverrideDBLink", e);
            }
        }
    }

    private void execute(List sqlParts, Map rtAttributes, List conDefList, boolean isInitDone) throws SQLException, Exception {
        // Drop First
        Iterator iter = sqlParts.iterator();
        while (iter.hasNext()) {
            SQLPart part = (SQLPart) iter.next();
            String sql = part.getSQL();
            String poolName = part.getConnectionPoolName();
            if (!StringUtil.isNullString(sql)) {
                sql = sql.trim();
                Logger.print(Logger.DEBUG, LOG_CATEGORY, sql);
                // FIXME Why are we differentiating between different statement types
                // when deciding whether or not to use autocommit?
                if (sql.startsWith("CREATE DATABASE LINK") && !isInitDone) {
                    SQLPart processedSqlPart = new SQLPart(decryptDbLinkSql(sql), part.getType(), poolName);
                    runtimeOverrideDBLink(processedSqlPart, conDefList);
                    doExecute(processedSqlPart, conDefList, true);
                } else if (sql.startsWith("DROP") && !isInitDone) {
                    doExecute(part, conDefList, true);
                } else if (sql.startsWith("CREATE") ) {
                    if (part.getType().equalsIgnoreCase(SQLPart.STMT_CREATELOGSUMMARYTABLE) && disableMonitor) {
                        continue;
                    } else {
                        if (rtAttributes != null && rtAttributes.size() != 0) {
                            sql = associateDynamicFileWithTable(part, rtAttributes);
                            part.setSQL(sql);
                        }

                        doExecute(part, conDefList, true);
                    }
                } else if (SQLPart.STMT_INITIALIZESTATEMENTS.equals(part.getType())) {
                    doExecute(part, conDefList, true);
                }
            }
        }
    }
}
