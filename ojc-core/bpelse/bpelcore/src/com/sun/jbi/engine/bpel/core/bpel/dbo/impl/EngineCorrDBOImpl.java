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
 * @(#)EngineCorrDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EngineCorrelationDBO;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class EngineCorrDBOImpl extends DBObjectImpl
    implements EngineCorrelationDBO {
    private long mId;
    //private String mBPELId;
    private QName mBPELId;
    private String mEngId;
    private String mValue;

    /**
     * constructor
     *
     * @param dbType database type
     */
    public EngineCorrDBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR,
            BASE_QUERY_STMT_STR);
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param id ID
     * @param bpelId BPELID
     * @param engId engine ID
     * @param value value
     */
    public EngineCorrDBOImpl(int dbType, long id, QName bpelId, String engId,
        String value) {
        this(dbType);
        mId = id;
        //mBPELId = bpelId;
        mBPELId = bpelId;
        mEngId = engId;
        mValue = value;
    }

    private EngineCorrDBOImpl() {
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.EngineCorrelationDBO#getId()
     */
    public long getId() {
        return mId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.EngineCorrelationDBO#getBpelId()
     */
    public QName getBpelId() {
        return mBPELId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.EngineCorrelationDBO#getEngineId()
     */
    public String getEngineId() {
        return mEngId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.EngineCorrelationDBO#getValue()
     */
    public String getValue() {
        return mValue;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setLong(1, getId());
        stmt.setString(2, getBpelId().toString());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setLong(1, getId());
        stmt.setString(2, getBpelId().toString());
        stmt.setString(3, getEngineId());
        stmt.setString(4, getValue());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt)
        throws SQLException {
        // Do nothing (throw exception?) update shouldn't be called for correlations.
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillQueryStmt(java.sql.PreparedStatement)
     */
    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        // TODO Auto-generated method stub
        stmt.setLong(1, getId());
        stmt.setString(2, getBpelId().toString());
        stmt.setString(3, getValue());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#populateDBO(java.sql.ResultSet)
     */
    public void populateDBO(ResultSet rs) throws SQLException {
        mId = rs.getLong(1);
        mBPELId = QName.valueOf(rs.getString(2));
        mEngId = rs.getString(3);
        mValue = rs.getString(4);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new EngineCorrDBOImpl();
    }
}
