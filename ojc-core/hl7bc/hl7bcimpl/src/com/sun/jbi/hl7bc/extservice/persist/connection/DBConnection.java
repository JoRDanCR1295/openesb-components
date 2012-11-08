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
 * @(#)DBConnection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.persist.connection;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.hl7bc.extservice.persist.dbo.HL7MessageLogDBO;

import com.sun.jbi.hl7bc.I18n;


/**
 * @author Sun Microsystems
 */
public class DBConnection extends AbstractDBConnection {

	private static final Logger logger = Logger.getLogger(DBConnection.class.getName());

    /**
     * @param conn A JDBC connection
     * @throws SQLException SQLException
     */
    public DBConnection(Connection conn) throws SQLException {
        super(conn);
    }
    
    /**
     * executes query
     *
     * @param obj DBObject
     * @return ResultSet ResultSet
     * @throws SQLException SQLException
     */
    public ResultSet getRowWithStatus(HL7MessageLogDBO dbo) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = mConn.prepareStatement(dbo.getQueryStmtWithStatus());
            dbo.fillQueryStmtWithStatus(stmt);
            rs = stmt.executeQuery();
        } catch (SQLException ex) {
            if (rs != null) {
                rs.close();
            }

            if (stmt != null) {
                stmt.close();
            }

            throw ex;
        }

        return rs;
    }
    
    /**
     * @param obj DBObject
     * @throws SQLException SQLException
     */
    public int updateRowWithStatus(HL7MessageLogDBO obj) throws SQLException {
        if (obj == null) {
            return -1;
        }
        int updatedRows = -1;

        PreparedStatement stmt = null;
        try {
            stmt = constructUpdateStmtWithStatus(obj);

            updatedRows = stmt.executeUpdate();

        } catch (SQLException ex) {
			throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }

        return updatedRows;
    }
    
    private PreparedStatement constructUpdateStmtWithStatus(HL7MessageLogDBO obj) throws SQLException {
        String stmtStr = obj.getUpdateStmtWithStatus();
		if(logger.isLoggable(Level.FINE)){
			logger.fine(I18n.msg("Constructing update statement : {0}", stmtStr));
		}
        PreparedStatement stmt = mConn.prepareStatement(stmtStr);
		obj.fillUpdateStmtWithStatus(stmt);
        return stmt;
    }
    
}
