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
 * @(#)SQLEngineFileEntry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;


/**
 * Data structure to hold an entry in the SQLSE engine file. Contains connectivity
 * and sqlfile information.
 */
public class SQLEngineFileEntry {
    private String sqlText = null;
    private String sqlFileName;
    private String wsdlFileName;
    private String driverClass;
    private String dbURL;
    private String databaseName;
    private String user;
    private String password;
	private String jndi;
	private String transactionRequired;
    /**
     * sets the sql text used by the user.
     * @param sql
     */
    protected void setSQLText(final String sql) {
        sqlText = sql;
    }

    /**
     * sets the sql file name.
     * @param sqlFileName
     */
    protected void setSQLFileName(final String sqlFileName) {
        this.sqlFileName = sqlFileName;
    }

    /**
     * sets the driverclass used by the sql file.
     * @param driverClass
     */
    protected void setDriverClass(final String driverClass) {
        this.driverClass = driverClass;
    }

    /**
     * sets the URL used for the sql file.
     * @param dbURL
     */
    protected void setDatabaseUrl(final String dbURL) {
        this.dbURL = dbURL;
    }

    /**
     * sets the database name.
     * @param databaseName
     */
    protected void setDatabaseName(final String databaseName) {
        this.databaseName = databaseName;
    }

    /**
     * sets the database user.
     * @param user
     */
    protected void setDatabaseUserName(final String user) {
        this.user = user;
    }

    /**
     * sets the database password.
     * @param password
     */
    protected void setDatabasePassword(final String password) {
        this.password = password;
    }

	/**
     * sets the jndi name.
     * @param jndi
     */
    protected void setJNDI(final String jndi) {
        this.jndi = jndi;
    }

    /**
     * returns the jndi name.
     * @return
     */
    String getJNDI() {
        return jndi;
    }
    
    /**
     * sets the transaction attribute.
     * @param transactionRequired 
     * 
     */
    protected void setTransactionRequired(String transactionRequired) {
        String trReqd =transactionRequired.toLowerCase().trim();
    	if (trReqd.equals("yes") || trReqd.equals("true")){
        	trReqd = "yes";
        }
    	this.transactionRequired = trReqd;
    }

    /**
     * returns the transaction attribute.
     * @return
     */
    String getTransactionRequired() {
        return transactionRequired;
    }

    /**
     * returns the sql text.
     * @return
     */
    String getSqlText() {
        return sqlText;
    }

    /**
     * Returns the sql file name given by the user.
     * @return
     */
    protected String getSqlFileName() {
        return sqlFileName;
    }

    /**
     * Returns the driver class.
     * @return
     */
    String getDriverClass() {
        return driverClass;
    }

    /**
     * Returns the database URL used.
     * @return
     */
    String getDbURL() {
        return dbURL;
    }

    /**
     * Returns the database name.
     * @return
     */
    String getDatabaseName() {
        return databaseName;
    }

    /**
     * Returns the database user.
     * @return
     */
    String getUser() {
        return user;
    }

    /**
     * Returns the database password.
     * @return
     */
    String getPassword() {
        return password;
    }

    /**
     * Returns the wsdl filename.
     * @return
     */
    private String getWsdlFileName() {
        return wsdlFileName;
    }

    /**
     * sets the wsdl file name.
     * @param wsdlFileName
     */
    private void setWsdlFileName(final String wsdlFileName) {
        this.wsdlFileName = wsdlFileName;
    }
}
