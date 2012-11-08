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
package com.sun.mashup.engine.impl;

import com.sun.mashup.engine.EDMProcessDefinition;
import com.sun.mashup.engine.MashupEngine;
import com.sun.sql.framework.exception.BaseException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.sun.rowset.WebRowSetImpl;
import com.sun.sql.framework.utils.ScEncrypt;
import com.sun.sql.framework.utils.StringUtil;
import com.sun.jbi.internationalization.Messages;

import com.sun.mashup.engine.QueryContext;
import com.sun.sql.framework.jdbc.SQLPart;
import com.sun.sql.framework.jdbc.SQLUtils;
import java.io.File;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.PreparedStatement;

import org.axiondb.util.StringIdentifierGenerator;

/**
 * This class is the container for running the Mashup Query. 
 * 
 * @author Srinivasan Rengarajan
 * @version :$Revision: 1.16 $
 */
public class MashupEngineImpl implements MashupEngine {

    /** Runtime context for this class. */
    private static final String LOG_CATEGORY = MashupEngineImpl.class.getName();
    private static final Messages mMessages = Messages.getMessages(MashupEngineImpl.class);
    private static final Pattern PASSWORD_PATTERN = Pattern.compile("PASSWORD\\ *=\\ *'([^']*)'");
    private static final Pattern USERNAME_PATTERN = Pattern.compile("USERNAME\\ *=\\ *'([^']*)'");
    private EDMProcessDefinition mashupDefinition = null;
    private String displayName = null;
    private QueryContext queryContext = null;
    private Connection conn = null;
    private String instanceName = null;
    private String dbName = null;

    /**
     * Constructor: loads the mashup process definition file.
     */
    public MashupEngineImpl(EDMProcessDefinition mashupDef, QueryContext queryContext) {
        this.mashupDefinition = mashupDef;
        this.queryContext = queryContext;
    }

    /**
     * execute the tasks by invoking process() method.
     * 
     * @param theExecListener Which implements ETLEngineExecListener
     * @return an integer
     */
    public final ResultSet exec() throws Exception {
        String managerName = "MashupEngine";
        ResultSet rs = null;
        try {
            conn = this.getAxionMemoryDbConnection();//
            runInitStatements(conn);
            rs = selectData(conn);
            if (displayName != null) {
                managerName += displayName;
            }
        } catch (Exception e) {
            throw e;
        }

        return rs;
    }

    private synchronized void runInitStatements(Connection conn) throws SQLException {
        if (conn == null) {
            return;
        }

        List<String> sqlsToExecute = new LinkedList<String>();

        Logger.getLogger(MashupEngineImpl.class.getName()).finer(mMessages.getString("EDMSE-F0220.runInitStatements") + this.mashupDefinition.getInitSQLParts().size());

        Iterator sqlIter = this.mashupDefinition.getInitSQLParts().listIterator();
        while (sqlIter.hasNext()) {
            SQLPart sql = (SQLPart) sqlIter.next();
            if (sql.getType().equalsIgnoreCase("createDbLinkStatement")) {
                // Need to decrypt the passwords in the DB Link statement
                if (!StringUtil.isNullString(sql.getSQL())) {
                    sqlsToExecute.add(decryptDbLinkSql(sql.getSQL()));
                }
            } else {
                // Other statement sql can be executed without decrypting.
                sqlsToExecute.add(sql.getSQL());
            }
        }

        executeBatch(sqlsToExecute, conn);
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
                        if (!StringUtil.isNullString(encryptedPassword)) {
                            password = ScEncrypt.decrypt(userName, encryptedPassword);
                            processedSql = matcher.replaceFirst("PASSWORD='" + StringUtil.escapeJavaRegexpChars(password) + "'");
                        }

                        if (!StringUtil.isNullString(userName)) {
                            matcher = USERNAME_PATTERN.matcher(processedSql);
                            processedSql = matcher.replaceFirst("USERNAME='" + StringUtil.escapeJavaRegexpChars(userName) + "'");
                        }
                    } catch (Exception ex) {
                        //Logger.printThrowable(Logger.ERROR, LOG_CATEGORY, this, DN + "InitTask:decryptDbLinkSql", ex);                    	
                    }
                }
            }
        }

        return processedSql;
    }

    private void executeBatch(List queries, Connection con) throws SQLException {
        Statement stmt = null;
        String sql = null;
        try {
            stmt = con.createStatement();// .createStatement(sql);
            for (ListIterator iter = queries.listIterator(); iter.hasNext();) {
                sql = (String) iter.next();
                Logger.getLogger(MashupEngineImpl.class.getName()).fine(mMessages.getString("EDMSE-F0221.executing_Query")+ sql);
                stmt.executeUpdate(sql);//TBD: switch to batch update
            }
        } catch (SQLException e) {
            throw e;
        } finally {
            if (stmt != null) {
                stmt.close();
                stmt = null;
            }
        }

    }

    private ResultSet selectData(Connection conn) throws SQLException {

        if (conn == null) {
            return null;
        }
        ResultSet rs = null;
        Statement stmt = null;
        PreparedStatement pstmt = null;
        String limitString = "limit ";
        String offsetString = "offset  ";
        String queryString = "";
        int limit = this.queryContext.getPageSize();
        int offset = 0;
        int pageNumber = this.queryContext.getPageNumber();
        int rowNumber = this.queryContext.getRow();
        String column = this.queryContext.getColumn();

        // calculate limit and offset from query context

        // offset = ((pageNumber * limit) + 1) - limit;
        offset = (pageNumber * limit) - limit;
        if (pageNumber == 0) {
            offset = 0;
        }

        if (rowNumber != 0) {
            limit = 1;
            offset = rowNumber - 1;
        }
        if (offset < 0) {
            limit = this.queryContext.getPageSize();
            offset = 0;
        }
        queryString = limitString + limit + " " + offsetString + offset;
        this.queryContext.setOffset(offset);
        try {
            //stmt = conn.createStatement();
            // append limit and offset to query
            Logger.getLogger(MashupEngineImpl.class.getName()).fine(mMessages.getString("EDMSE-F0222.runtime_Query")+ this.mashupDefinition.getDataMashupQuery().getSQL().trim() + " " + queryString);

            SQLPart mashupQuery = this.mashupDefinition.getDataMashupQuery();

            String ps = SQLUtils.createPreparedStatement(mashupQuery.getSQL().trim() + " " + queryString, this.mashupDefinition.getAttributeMap(), this.queryContext.getDynamicParams());
            pstmt = conn.prepareStatement(ps);
            try {
                SQLUtils.populatePreparedStatement(pstmt, this.mashupDefinition.getAttributeMap(), this.queryContext.getDynamicParams(), this.queryContext.getDynamicMap());
            } catch (BaseException ex) {
                Logger.getLogger(MashupEngineImpl.class.getName()).log(Level.SEVERE, null, ex);
            }
            //rs = stmt.executeQuery(mashupQuery.getSQL().trim() + " " + queryString);            
            rs = pstmt.executeQuery();

        } catch (SQLException e) {
            throw e;
        }
        finally {
        }
        return rs;
    }

    /**
     * get display name of the engine.
     */
    public String getDisplayName() {
        return displayName;
    }

    /**
     * set display name of the engine.
     */
    public void setDisplayName(String theDisplayName) {
        displayName = theDisplayName;
    }

    public void setMashupDefinition(EDMProcessDefinition def) {
        this.mashupDefinition = def;
    }

    public EDMProcessDefinition getMashupDefinition() {
        return this.mashupDefinition;
    }

    /**
     * Starts MashupEngine
     */
    public void start() {
    }

    /**
     * Stop the MashupEngine 
     */
    public void stopMashupEngine() {
        if (conn != null) {
            try {
                //Shutdown the instance, and then delete it.
                conn.createStatement().execute("shutdown");
                if (instanceName != null && dbName != null) {
                    boolean isDeleted = deleteAxionDBInstance(new File(this.queryContext.getWorkingDirectory() + File.separator + instanceName));
                    if (isDeleted) {
                        Logger.getLogger(MashupEngineImpl.class.getName()).info(mMessages.getString("EDMSE-I0440.virtual_Database_Instance")+ this.queryContext.getWorkingDirectory() + File.separator + instanceName + " " + mMessages.getString("EDMSE-I0441.deleted_Successfully"));
                    }
                }
                conn.close();
            } catch (SQLException ex) {
                Logger.getLogger(MashupEngineImpl.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    private boolean deleteAxionDBInstance(File dir) {
        if ((dir == null) || !dir.isDirectory() || !dir.exists()) {
            return false;
        }

        // Get the content of the dir.
        final File[] files = dir.listFiles();

        // Iterate over the items
        for (int i = 0; i < files.length; i++) {
            if (files[i].isDirectory()) {
                // If the item is a directory itself,
                // do a recursive call with it
                deleteAxionDBInstance(files[i]);
            } else {
                // If it is a file, delete it
                files[i].delete();
            }
        }

        // Finally, delete the directory itself.
        return dir.delete();
    }

    /**
     * This method explicitly loads the axion connection. 
     * Need more seperation to allow pluggable providers. 
     **/
    protected synchronized Connection getAxionMemoryDbConnection() throws SQLException, ClassNotFoundException, InstantiationException, IllegalAccessException {
        Connection con = null;
        try {
            Driver originalDriver = (Driver) Class.forName("org.axiondb.jdbc.AxionDriver").newInstance();
            DriverManager.registerDriver(originalDriver);
            //instanceName = String.valueOf(System.currentTimeMillis());
            //On Windows the granularity of currentTimeMillis is about 10-15ms, so if two threads are executing
            //at the same time, it is possible that both will share the same Axion DB instance. This will cause
            //unexpeted behavior and errors. To avoid this, following line generates a system wide unique ID.
            instanceName = StringIdentifierGenerator.INSTANCE.nextIdentifier().substring(0, 6);
            dbName = "jdbc:axiondb:" + instanceName + ":" + this.queryContext.getWorkingDirectory() + File.separator + instanceName;
            con = DriverManager.getConnection(dbName, "sa", "sa");
            con.setAutoCommit(true);
        } catch (SQLException e) {
            throw e;
        } catch (ClassNotFoundException e) {
            throw e;
        }
        return con;
    }

    public static void main(String[] args) throws Exception {
        java.io.File f = new java.io.File(args[0]);
        EDMProcessDefinition processDef = new EDMProcessDefinition(f);
        QueryContext queryContext = new QueryContext();
        Logger.getLogger(MashupEngineImpl.class.getName()).fine(processDef.dumpQueries());
        MashupEngine engine = new MashupEngineImpl(processDef, queryContext);
        ResultSet rs = engine.exec();
        try {
            WebRowSetImpl ws = new WebRowSetImpl();
            ws.populate(rs);
            ws.writeXml(System.out);

            String col1 = ws.getString(0);
            String col2 = ws.getString(1);

        } catch (SQLException e) {
            e.printStackTrace();
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }

    }
}
