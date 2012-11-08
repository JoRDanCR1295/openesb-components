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
 * @(#)StateDBOImpl.java 
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
import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class StateDBOImpl extends DBObjectImpl implements StateDBO {
    private String mId;
    //private String mBPELId;
    private QName mBPELId;
    private String mEngId;
    private State.Mode mMode;
    private String mStatus;
    private String mOwnerLock;

    /**
     * constructor
     *
     * @param dbType database type
     * @param id state ID
     */
    public StateDBOImpl(int dbType, String id) {
        this(dbType);
        mId = id;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param id state ID
     * @param bpelId BPEL ID
     * @param engId engine ID
     */
    //public StateDBOImpl(int dbType, String id, String bpelId, String engId) {
    public StateDBOImpl(int dbType, String id, QName bpelId, String engId) {
        this(dbType);
        mId = id;
        mBPELId = bpelId;
        mEngId = engId;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param id state ID
     * @param engineId TODO
     * @param bpelId BPEL ID
     * @param engId engine ID
     */
    public StateDBOImpl(int dbType, String id, String engineId, State.Mode mode) {
        this(dbType);
        mId = id;
        mEngId = engineId;
        mMode = mode;
    }
    
    /**
     * constructor
     *
     * @param dbType database type
     */
    public StateDBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR,
            BASE_QUERY_STMT_STR);
    }

    private StateDBOImpl() {
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO#getInstanceId()
     */
    public String getId() {
        return mId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO#getBPELId()
     */
    //public String getBPELId() {
    public QName getBPELId() {
        return mBPELId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO#getEngineId()
     */
    public String getEngineId() {
        return mEngId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, COMPLETE_STATUS);
        stmt.setString(2, getId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getId());
        stmt.setString(2, getBPELId().toString());
        stmt.setString(3, getEngineId());
        stmt.setString(4, RUNNING_STATUS);
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getEngineId());
        if (mMode == State.Mode.PASSIVATE) {
            stmt.setString(2, OWNERLOCK_NO);
            stmt.setString(3, OWNERLOCK_YES);
        } else if (mMode == State.Mode.ACTIVATE) {
            stmt.setString(2, OWNERLOCK_YES);
            stmt.setString(3, OWNERLOCK_NO);
        }
        stmt.setString(4, getId());
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillQueryStmt(java.sql.PreparedStatement)
     */
    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#populateDBO(java.sql.ResultSet)
     */
    public void populateDBO(ResultSet rs) throws SQLException {
        
        mId = rs.getString(1);
        String bpelId = rs.getString(2);
        mBPELId = QName.valueOf(rs.getString(2));
        mEngId = rs.getString(3);
        mOwnerLock = rs.getString(4);
        mStatus = rs.getString(5);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new StateDBOImpl();
    }

    public State.Mode getMode() {
        return mMode;
    }
    
    public String getStatus() {
    	return mStatus;
    }
    
    public String getOwnerLock() {
    	return mOwnerLock;
    }

    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPEL ID = " + getBPELId());
        retStr.append("\n\t");
        retStr.append("BPID = " + getId());
        retStr.append("\n\t");
        retStr.append("Engine Id = " + getEngineId());
        retStr.append("\n\t");
        retStr.append("Mode = " + getMode());
        retStr.append("\n\t");
        retStr.append("Owner Lock = " + getOwnerLock());
        return retStr.toString();
    }
    
}
