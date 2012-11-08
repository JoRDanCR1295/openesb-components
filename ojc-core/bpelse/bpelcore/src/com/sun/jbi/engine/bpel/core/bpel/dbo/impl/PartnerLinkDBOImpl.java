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
 * @(#)VariableDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.io.Reader;
import java.io.StringReader;
import java.sql.*;

import javax.sql.rowset.serial.SerialClob;
import javax.sql.rowset.serial.SerialException;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.PartnerLinkDBO;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class PartnerLinkDBOImpl extends DBObjectImpl implements PartnerLinkDBO {
    private String mBPId;
    private long mId;
    private Clob mContent;
    private String mScopeGuid;
    private int mDbType;
    
    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     */
    public PartnerLinkDBOImpl(int dbType, String bpId) {
        this(dbType);
        mBPId = bpId;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     * @param id variable ID
     * @param scopeGuid TODO
     * @param val value
     * @throws SerialException SerialException
     * @throws SQLException SQLException
     */
    public PartnerLinkDBOImpl(int dbType, String bpId, long id, String scopeGuid, char[] val)
        throws SerialException, SQLException {
        this(dbType);
        mBPId = bpId;
        mId = id;
        mScopeGuid = scopeGuid;
        mContent = (val == null) ? new SerialClob(new char[] {}) : new SerialClob(val);
    }

    /**
     * constructor
     *
     * @param dbType database type
     */
    public PartnerLinkDBOImpl(int dbType) {
        mDbType = dbType;
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    }

    private PartnerLinkDBOImpl() {
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO#getId()
     */
    public long getId() {
        return mId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO#getValue()
     */
    public Object getValue() {
        Reader retVal = null;
        try {
            retVal = mContent.getCharacterStream();
        } catch (SQLException e) {
            retVal = new StringReader("");
        }
        return retVal;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO#getBPId()
     */
    public String getBPId() {
        return mBPId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.PartnerLinkDBO#getScopeGuid()
     */
    public String getScopeGuid() {
        return mScopeGuid;
    }
    

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, mBPId);
        stmt.setLong(2, mId);
        stmt.setString(3, mScopeGuid);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setLong(2, getId());
        stmt.setCharacterStream(3, mContent.getCharacterStream(), (int) mContent.length());
        stmt.setString(4, getScopeGuid());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setCharacterStream(1, mContent.getCharacterStream(), (int) mContent.length());
        stmt.setString(2, getBPId());
        stmt.setLong(3, getId());
        stmt.setString(4, getScopeGuid());
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
        if( mDbType == ConnectionProperties.POSTGRES_DB.intValue() ){
            mContent = new SerialClob( rs.getString(3).toCharArray() );
        } else {
            mContent = new SerialClob(rs.getClob(3));
        }
        mScopeGuid = rs.getString(4);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new PartnerLinkDBOImpl();
    }
    
    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getBPId());
        retStr.append("\n\t");
        retStr.append("PartnerLink ID = " + getId());
        retStr.append("\n\t");
        retStr.append("Scope GUID = " + getScopeGuid());
        retStr.append("\n\t");
        retStr.append("Value = 'some clob'");
        return retStr.toString();
    }
}
