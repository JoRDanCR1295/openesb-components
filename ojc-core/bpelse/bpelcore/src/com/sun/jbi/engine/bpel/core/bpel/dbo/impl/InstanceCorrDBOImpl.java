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
 * @(#)InstanceCorrDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class InstanceCorrDBOImpl extends DBObjectImpl
    implements InstanceCorrelationDBO {
    private long mId;
    private String mBPId;
    private String mValue;

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     */
    public InstanceCorrDBOImpl(int dbType, String bpId) {
        this(dbType);
        mBPId = bpId;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param id ID
     * @param bpId BPID
     * @param value value
     */
    public InstanceCorrDBOImpl(int dbType, long id, String bpId, String value) {
        this(dbType);
        mId = id;
        mBPId = bpId;
        mValue = value;
    }

    /**
     * constructor
     *
     * @param dbType database type
     */
    public InstanceCorrDBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR,
            BASE_QUERY_STMT_STR);
    }

    private InstanceCorrDBOImpl() {
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO#getId()
     */
    public long getId() {
        return mId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO#getBPId()
     */
    public String getBPId() {
        return mBPId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO#getValue()
     */
    public String getValue() {
        return mValue;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setLong(2, getId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setLong(2, getId());
        stmt.setString(3, getValue());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt)
        throws SQLException {
        // TODO Auto-generated method stub
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillQueryStmt(java.sql.PreparedStatement)
     */
    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getBPId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#populateDBO(java.sql.ResultSet)
     */
    public void populateDBO(ResultSet rs) throws SQLException {
        mBPId = rs.getString(1);
        mId = rs.getLong(2);
        mValue = rs.getString(3);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new InstanceCorrDBOImpl();
    }
    
    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getBPId());
        retStr.append("\n\t");
        retStr.append("Variable ID = " + getId());
        retStr.append("\n\t");
        retStr.append("Value = " + getValue());
        return retStr.toString();
    }
}
