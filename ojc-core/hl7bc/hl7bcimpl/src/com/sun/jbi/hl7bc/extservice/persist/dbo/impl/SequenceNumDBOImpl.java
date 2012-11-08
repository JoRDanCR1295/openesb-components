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
 * @(#)SequenceNumDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.persist.dbo.impl;

import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObject;
import com.sun.jbi.hl7bc.extservice.persist.dbo.SequenceNumDBO;
import com.sun.jbi.hl7bc.extservice.persist.connection.ConnectionProperties;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 *
 * @author TVA Raghunadh
 */
public class SequenceNumDBOImpl extends DBObjectImpl implements SequenceNumDBO {
    private int mESN;

    private String mESNState;

    private String mQueryingKey;

    /**
     * constructor
     *
     * @param dbType database type
     */
    public SequenceNumDBOImpl(int dbType) {
        if (dbType == ConnectionProperties.AXION_DB.intValue()) {
            super.init(BASE_INSERT_STMT_STR_AXIONDB, BASE_UPDATE_STMT_STR_AXIONDB, BASE_DELETE_STMT_STR_AXIONDB, BASE_QUERY_STMT_STR_AXIONDB);
        } else {
            super.init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR,
                    BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
        }
       // init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param queryString Unique ID for the records in EXPSEQUENCENO Table
     * @param esn expected sequence number
     * @param esnState expected sequence number state
     */
    public SequenceNumDBOImpl(int dbType, String queryString, int esn, String esnState) {
        this(dbType);
        mQueryingKey = queryString;
        mESN = esn;
        mESNState = esnState;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param queryString Unique ID for the records in EXPSEQUENCENO Table
     */
    public SequenceNumDBOImpl(int dbType, String queryString) {
        this(dbType);
        mQueryingKey = queryString;
    }

    private SequenceNumDBOImpl() {
    }

    /**
     * @see com.sun.jbi.hl7bc.connection.dbo.SequenceNumDBOImpl
     */
    public int getESN() {
        return mESN;
    }

    public void setESN(int val) {
        mESN = val;
    }

    /**
     * get Expected Sequence Number State
     * 	@return mESNState
     */
    public String getESNState() {
        return mESNState;
    }

    public void setESNState(String val) {
        mESNState = val;
    }

    public String getQueryingKey() {
        return mQueryingKey;
    }

    public void setQueryingKey(String val) {
        mQueryingKey = val;
    }

    public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getQueryingKey());

        stmt.setInt(2, getESN());
        stmt.setString(3, getESNState());

    }

    public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
        stmt.setInt(1, getESN());
        stmt.setString(2, getESNState());
        stmt.setString(3, getQueryingKey());
    }

    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getQueryingKey());
    }

    public void populateDBO(ResultSet rs) throws SQLException {
        mQueryingKey = rs.getString(1);

        mESN = rs.getInt(2);
        mESNState = rs.getString(3);
    }

    public DBObject getNewObject() {
        return new SequenceNumDBOImpl();
    }
}
