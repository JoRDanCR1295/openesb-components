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

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;

import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class EventHandlerDBOImpl extends DBObjectImpl implements EventHandlerDBO {
    private String mBPId;
    private String mId;
    private String mScopeGuid;
    private long mEventId;
    private String mUpdateStatus;
    private Timestamp mDate;
//  repeatEvery value can't be less than or equal to zero as per BPEL spec. hence using 0 as default value to persist
    private long mRepeatEveryVal = 0; 

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     */
    public EventHandlerDBOImpl(int dbType, String bpId) {
        this(dbType);
        mBPId = bpId;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     * @param id event handler Id
     * @param scopeId event handler ID
     * @param eventModelId onEvent or onAlarm static model Id
     * @param updateStatus TODO
     * @param date TODO
     * @throws SQLException SQLException
     */
    public EventHandlerDBOImpl(int dbType, String bpId, String id, String scopeId, 
            long eventModelId, String updateStatus, Timestamp date, Long repeatEveryVal) {
        this(dbType);
        mBPId = bpId;
        mId = id;
        mScopeGuid = scopeId;
        mEventId = eventModelId; 
        mUpdateStatus = updateStatus;
        mDate = date;
        if (repeatEveryVal != null) {
            mRepeatEveryVal = repeatEveryVal.longValue();
        }
    }

    /**
     * constructor
     *
     * @param dbType database type
     */
    public EventHandlerDBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    }

    private EventHandlerDBOImpl() {
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO#getId()
     */
    public String getId() {
        return mId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO#getBPId()
     */
    public String getBPId() {
        return mBPId;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO#getScopeGuid()
     */
    public String getScopeGuid() {
        return mScopeGuid;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO#getEventModelId()
     */
    public long getEventModelId() {
        return mEventId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO#getTimerValue()
     */
    public Timestamp getTimerValue() {
        return mDate;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO#isUpdated()
     */
    public boolean isUpdated() {
        return Utility.areEqual(UPDATE_SATUS, mUpdateStatus);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO#getRepeatEveryVal()
     */
    public long getRepeatEveryVal() {
        return mRepeatEveryVal;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, mBPId);
        stmt.setString(2, mId);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setString(2, getId());
        stmt.setString(3, mScopeGuid);
        stmt.setLong(4, mEventId);
        stmt.setString(5, mUpdateStatus);
        stmt.setTimestamp(6, mDate);
        stmt.setLong(7, mRepeatEveryVal);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, mUpdateStatus);
        stmt.setString(2, getBPId());
        stmt.setString(3, getId());
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
        mId = rs.getString(2);
        mScopeGuid = rs.getString(3);
        mEventId = rs.getLong(4);
        mUpdateStatus = rs.getString(5);
        mDate = rs.getTimestamp(6);
        mRepeatEveryVal = rs.getLong(7);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()
     */
    public DBObject getNewObject() {
        return new EventHandlerDBOImpl();
    }

    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getBPId());
        retStr.append("\n\t");
        retStr.append("Event ID = " + getId());
        retStr.append("\n\t");
        retStr.append("Event Model ID = " + getEventModelId());
        retStr.append("\n\t");
        retStr.append("Scope GUID = " + getScopeGuid());
        retStr.append("\n\t");
        retStr.append("Is updated = " + isUpdated());
        retStr.append("\n\t");
        retStr.append("Timer Value = " + getTimerValue());
        retStr.append("\n\t");
        retStr.append("Repeat Every Value = " + getRepeatEveryVal());
        return retStr.toString();
    }
}
