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
 * @(#)HL7DBConnection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.hl7.xsdbuilder;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

/**
 * This a connection object for the HL7 MS Access database
 *
 * @author Julie Knight
 * @version 
 */
public class HL7DBConnection {

    private static final String JDBC_DRIVER = "sun.jdbc.odbc.JdbcOdbcDriver";

    private String dbFilename = new String();
    private Connection dbConn = null;

    /** Constructor 
     * @throws SQLException 
     * @throws ClassNotFoundException */
    public HL7DBConnection(String dbFname)
            throws ClassNotFoundException, SQLException {
        this.dbFilename = dbFname;
            createConnection();
    }
    
    /** Destructor */
    protected void finalize() 
            throws java.sql.SQLException {
        close();
    }

    /**
     * Creates the database connection
     */
    private void createConnection() 
            throws java.lang.ClassNotFoundException, java.sql.SQLException {
        String url =
            "jdbc:odbc:Driver={Microsoft Access Driver (*.mdb)};DBQ="
                + dbFilename;
        Class.forName(JDBC_DRIVER);
        dbConn = DriverManager.getConnection (url);
    }  
       
    /**
     * Returns the HL7 MS Access database connection
     *
     * @return database connection
     */
    public Connection getConn() {
        return this.dbConn;
    }
    
    /**
     * Closes the database connection
     */
    public void close()
            throws java.sql.SQLException {
        if(!dbConn.isClosed()) {
            dbConn.close();
            dbConn = null;
        }
    }
}
