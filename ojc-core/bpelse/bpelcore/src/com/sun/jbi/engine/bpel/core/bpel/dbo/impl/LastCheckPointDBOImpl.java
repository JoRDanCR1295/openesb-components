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
 * @(#)LastCheckPointDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;

import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO;


/**
 * Default implementation of {@link LastCheckPointDBO}.
 *
 * @author Sun Microsystems
 */
public class LastCheckPointDBOImpl extends DBObjectImpl
                                   implements LastCheckPointDBO {
    private String mBPId;
    private long mActId;
    private long mOldActId = DEFUALT_OLD_PC_VAL;
    private Timestamp mDate;
    private long mPickCompositeActId;
    private long mBranchInvokeCounter;

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     */
    public LastCheckPointDBOImpl(int dbType, String bpId) {
        this(dbType);
        mBPId = bpId;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     * @param actId activity ID
     * @param oldActId old activity ID
     * @param date timestamp
     * @param pickCompositeActId pick composite activity ID
     */
    public LastCheckPointDBOImpl(int dbType, String bpId, long actId,
                                 long oldActId, Timestamp date, 
                                 long pickCompositeActId, long branchInvokeCounter) {
        this(dbType);
        mBPId = bpId;
        mActId = actId;
        mOldActId = oldActId;
        mDate = date;
        mPickCompositeActId = pickCompositeActId;
        mBranchInvokeCounter = branchInvokeCounter;
    }

    /**
     * constructor
     *
     * @param dbType database type
     */
    public LastCheckPointDBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR,
            BASE_QUERY_STMT_STR);
    }

    private LastCheckPointDBOImpl() {
        //niladic
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO#getPCId()
     */
    public String getBPId() {
        return mBPId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO#getActivityId()
     */
    public long getActivityId() {
        return mActId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO#getOldActivityId()
     */
    public long getOldActivityId() {
        return mOldActId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO#getTimerValue()
     */
    public Timestamp getTimerValue() {
        return mDate;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO#getPickCompositeActId()
     */
    public long getPickCompositeActId() {
        return mPickCompositeActId;
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO#getBranchInvokeCounter()
     */
    public long getBranchInvokeCounter() {
    	return mBranchInvokeCounter;
    }
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setLong(2, getActivityId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setLong(2, getActivityId());
        stmt.setTimestamp(3, getTimerValue());
        stmt.setLong(4, getPickCompositeActId());
        stmt.setLong(5, getBranchInvokeCounter());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setLong(1, getActivityId());
        stmt.setTimestamp(2, getTimerValue());
        stmt.setLong(3, getPickCompositeActId());
        if (DEFAULT_BRANCH_INVOKE_COUNTER != mBranchInvokeCounter) {
            stmt.setLong(4, getBranchInvokeCounter());
            stmt.setString(5, getBPId());
            stmt.setLong(6, getOldActivityId());
        } else {
            stmt.setString(4, getBPId());
            stmt.setLong(5, getOldActivityId());
        }
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
        mActId = rs.getLong(2);
        mDate = rs.getTimestamp(3);
        mPickCompositeActId = rs.getLong(4);
        mBranchInvokeCounter = rs.getLong(5);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new LastCheckPointDBOImpl();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.impl.DBObjectImpl#getUpdateStmt()
     */
    public String getUpdateStmt() {
        return (DEFAULT_BRANCH_INVOKE_COUNTER != mBranchInvokeCounter) ? 
            BASE_UPDATE_STMT_STR_WITH_BRANCHINVOKEID : super.getUpdateStmt();
    }
    
    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getBPId());
        retStr.append("\n\t");
        retStr.append("Activity ID = " + getActivityId());
        retStr.append("\n\t");
        retStr.append("Old Activity ID = " + getOldActivityId());
        retStr.append("\n\t");
        retStr.append("Pick Composite ID = " + getPickCompositeActId());
        retStr.append("\n\t");
        retStr.append("Timer Value = " + getTimerValue());
        retStr.append("\n\t");
        retStr.append("Branch Invoke Counter " + getBranchInvokeCounter());
        return retStr.toString();
    }
}
