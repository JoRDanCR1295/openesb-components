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
 * @(#)ForEachDBOImpl.java 
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
import com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO;


/**
 * Default implementation of {@link ForEachDBO}.
 *
 * @author Sun Microsystems
 */
public class ForEachDBOImpl extends DBObjectImpl implements ForEachDBO {
    private String mBPId;
    private long mForEachId;
    private int mCounter, mSuccesses, mStartCount, mFinalCount, mCompletionCount;
    
    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     */
    public ForEachDBOImpl(int dbType, String bpId) {
        this(dbType);
        mBPId = bpId;
    }
    
    public ForEachDBOImpl(int dbType, String bpId, long foreachId,
                          int counter, int successes,
                          int startCount, int finalCount, int completionCount) {
        this(dbType, bpId);
        mForEachId = foreachId;
        mCounter = counter;
        mSuccesses = successes;
        mStartCount = startCount;
        mFinalCount = finalCount;
        mCompletionCount = completionCount;
    }

    /**
     * constructor
     *
     * @param dbType database type
     */
    protected ForEachDBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, 
             BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    }

    private ForEachDBOImpl() {
        //niladic
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO#getBPId() */
    public String getBPId() {
        return mBPId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO#getForEachId() */
    public long getForEachId() {
        return mForEachId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO#getCounter() */
    public int getCounter() {
        return mCounter;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO#getSuccesses() */
    public int getSuccesses() {
        return mSuccesses;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO#getCompletionCount() */
    public int getCompletionCount() {
        return mCompletionCount;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO#getFinalCount() */
    public int getFinalCount() {
        return mFinalCount;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO#getStartCount() */
    public int getStartCount() {
        return mStartCount;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setLong(2, getForEachId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setLong(1, getForEachId());
        stmt.setString(2, getBPId());
        stmt.setInt(3, getCounter());
        stmt.setInt(4, getSuccesses());
        stmt.setInt(5, getStartCount());
        stmt.setInt(6, getFinalCount());
        stmt.setInt(7, getCompletionCount());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setInt(1, getCounter());
        stmt.setInt(2, getSuccesses());
        stmt.setInt(3, getStartCount());
        stmt.setInt(4, getFinalCount());
        stmt.setInt(5, getCompletionCount());
        stmt.setString(6, getBPId());
        stmt.setLong(7, getForEachId());
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
        mForEachId = rs.getLong(1);
        mBPId = rs.getString(2);
        mCounter = rs.getInt(3);
        mSuccesses = rs.getInt(4);
        mStartCount = rs.getInt(5);
        mFinalCount = rs.getInt(6);
        mCompletionCount = rs.getInt(7);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new ForEachDBOImpl();
    }
    
    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getBPId());
        retStr.append("\n\t");
        retStr.append("ForEach ID = " + getForEachId());
        retStr.append("\n\t");
        retStr.append("Start Count = " + getStartCount());
        retStr.append("\n\t");
        retStr.append("Final Count = " + getFinalCount());
        retStr.append("\n\t");
        retStr.append("Counter = " + getCounter());
        retStr.append("\n\t");
        retStr.append("Completion count = " + getCompletionCount());
        retStr.append("\n\t");
        retStr.append("Success count = " + getSuccesses());
        return retStr.toString();
    }
}
