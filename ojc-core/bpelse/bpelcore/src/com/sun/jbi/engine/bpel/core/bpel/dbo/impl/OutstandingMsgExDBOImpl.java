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
import com.sun.jbi.engine.bpel.core.bpel.dbo.OutstandingMsgExDBO;


/**
 * Used by the scalability thread for phase 2 persistence of outstanding 
 * message exchagnes.
 *
 * @author Sun Microsystems
 */
public class OutstandingMsgExDBOImpl extends DBObjectImpl
    implements OutstandingMsgExDBO {
    private String mMsgExId;
    private String mInstanceId;
    private String mBPELId;

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     */
    public OutstandingMsgExDBOImpl(int dbType, String msgExId, String bpelid) {
        this(dbType);
        mMsgExId = msgExId;
        mBPELId = bpelid;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param id ID
     * @param bpId BPID
     * @param value value
     */
    public OutstandingMsgExDBOImpl(int dbType, String msgExId, String instanceId, String bpelId) {
        this(dbType);
        mMsgExId = msgExId;
        mInstanceId = instanceId;
        mBPELId = bpelId;
    }

    /**
     * constructor
     *
     * @param dbType database type
     */
    public OutstandingMsgExDBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR,
            BASE_QUERY_STMT_STR);
    }

    private OutstandingMsgExDBOImpl() {
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO#getBPId()
     */
    public String getInstanceId() {
        return mInstanceId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO#getValue()
     */
    public String getMsgExId() {
        return mMsgExId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getMsgExId());
        stmt.setString(2, getBPELId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getMsgExId());
        stmt.setString(2, getInstanceId());
        stmt.setString(3, getBPELId());
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
        stmt.setString(1, getMsgExId());
        stmt.setString(2, getBPELId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#populateDBO(java.sql.ResultSet)
     */
    public void populateDBO(ResultSet rs) throws SQLException {
        mMsgExId = rs.getString(1);
        mInstanceId = rs.getString(2);
        mBPELId = rs.getString(3);        
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new OutstandingMsgExDBOImpl();
    }
    
    /**
     * BPEL Process Id
     */
    public String getBPELId () {
        return mBPELId;
    }
}
