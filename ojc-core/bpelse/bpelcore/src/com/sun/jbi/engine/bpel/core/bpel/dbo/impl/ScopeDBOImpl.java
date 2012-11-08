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
 * @(#)FaultHandlerDataDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.io.Reader;
import java.io.StringReader;
import java.sql.Clob;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.sql.rowset.serial.SerialClob;
import javax.sql.rowset.serial.SerialException;
import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class ScopeDBOImpl extends DBObjectImpl 
                                     implements ScopeDBO {
    private String mBPId;
    private long mScopeId;
    private String mScopeGuid;
    private Clob mContent;
    private QName mFaultName;
    private String mParentScopeGuid;
    private String mScopeState;
    private long mCompensateId;
    private long mCompletionOrder;
    private long mFaultActivityId;
    private int mDbType;

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     */
    public ScopeDBOImpl(int dbType, String bpId) {
        this(dbType);
        mBPId = bpId;
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param bpId BPID
     * @param id variable ID
     * @param val value
     *
     * @throws SerialException SerialException
     * @throws SQLException SQLException
     */
    public ScopeDBOImpl(int dbType, String bpId, long scopeId, 
    		String scopeGuid, String parentScopeGuid,
    		String scopeState,
    		long compensateId, long completionOrder,
    		QName faultName, char[] val, long faultActivityId)
        throws SerialException, SQLException {
        this(dbType);
        mBPId = bpId;
        mScopeId = scopeId;
        mScopeGuid = scopeGuid;
        mParentScopeGuid = parentScopeGuid;
        mScopeState = scopeState;
        mCompensateId = compensateId;
        mCompletionOrder = completionOrder;
        mFaultName = faultName;
        mContent = (val == null) ? null : new SerialClob(val);
        mFaultActivityId = faultActivityId;
        
    }

    /**
     * constructor
     *
     * @param dbType database type
     */
    public ScopeDBOImpl(int dbType) {
        mDbType = dbType;
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    }

    private ScopeDBOImpl() {
    }

    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO#getScopeId()
	 */
	public long getScopeId() {
		return mScopeId;
	}

	/**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO#getScopeGuid()
     */
    public String getScopeGuid() {
        return mScopeGuid;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO#getFaultData()
     */
    public Reader getFaultData() {
        Reader retVal = null;
        try {
            retVal = (mContent == null) ? null : mContent.getCharacterStream();
        } catch (SQLException e) {
            retVal = new StringReader("");
        }
        return retVal;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO#getBPId()
     */
    public String getBPId() {
        return mBPId;
    }
    
    public String getParentScopeGuid() {
    	return mParentScopeGuid;
    }
    
    public String getScopeState() {
    	return mScopeState;
    }
    
    public long getCompensateId() {
    	return mCompensateId;
    }
    
    public long getCompletionOrder() {
    	return mCompletionOrder;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO#getFaultName()
     */
    public QName getFaultName() {
        return mFaultName;
    }
    
    public long getFaultActivityId() {
    	return mFaultActivityId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setString(2, getScopeGuid());
        stmt.setLong(3, getScopeId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getBPId());
        stmt.setLong(2, getScopeId());
        stmt.setString(3, getScopeGuid());
        stmt.setString(4, getParentScopeGuid());
        stmt.setString(5, getScopeState());
        stmt.setLong(6, getCompensateId());
        stmt.setLong(7, getCompletionOrder());
        stmt.setString(8, String.valueOf(getFaultName()));
        // setClob does not work for oracle
        //stmt.setClob(4, mContent);
        if (mContent == null) {
            stmt.setCharacterStream(9, null, 0);
        }
        else {
            stmt.setCharacterStream(9, mContent.getCharacterStream(), (int) mContent.length());
        }
        stmt.setLong(10, getFaultActivityId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
//        throw new UnsupportedOperationException();
    	stmt.setString(1, getParentScopeGuid());
    	stmt.setString(2, getScopeState());
    	stmt.setLong(3, getCompensateId());
    	stmt.setLong(4, getCompletionOrder());
    	stmt.setString(5, String.valueOf(getFaultName()));
        if (mContent == null) {
            stmt.setCharacterStream(6, null, 0);
        }
        else {
            stmt.setCharacterStream(6, mContent.getCharacterStream(), (int) mContent.length());
        }
        stmt.setLong(7, getFaultActivityId());
        stmt.setString(8, getBPId());
        stmt.setString(9, getScopeGuid());
        stmt.setLong(10, getScopeId());
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
        mScopeId = rs.getLong(2);
        mScopeGuid = rs.getString(3);
        mParentScopeGuid = rs.getString(4);
        mScopeState = rs.getString(5);
        mCompensateId = rs.getLong(6);
        mCompletionOrder = rs.getLong(7);
        String faultNameString = rs.getString(8);
        if (faultNameString != null && !faultNameString.equals("")) {
            mFaultName = QName.valueOf(faultNameString);
        }
        
//        mContent = new SerialClob(rs.getClob(4));
        if( mDbType == ConnectionProperties.POSTGRES_DB.intValue() ) {
            String tmpRes = rs.getString(9);
            mContent = (tmpRes == null) ? null : new SerialClob(tmpRes.toCharArray());
        } else {
            Clob dbval = rs.getClob(9);
            mContent = (dbval == null) ? null : new SerialClob(dbval);
        }
        mFaultActivityId = rs.getLong(10);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new ScopeDBOImpl();
    }
    
    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getBPId());
        retStr.append("\n\t");
        retStr.append("Scope ID = " + getScopeId());
        retStr.append("\n\t");
        retStr.append("Scope GUID = " + getScopeGuid());
        retStr.append("\n\t");
        retStr.append("Parent Scope GUID = " + getParentScopeGuid());
        retStr.append("\n\t");
        retStr.append("Scope State = " + getScopeState());
        retStr.append("\n\t");
        retStr.append("Fault Name = " + getFaultName());
        retStr.append("\n\t");
        retStr.append("Fault Data = " + "' some value'");
        retStr.append("\n\t");
        retStr.append("Fault Activity Id = " + getFaultActivityId());
        retStr.append("\n\t");
        retStr.append("Completion Order = " + getCompletionOrder());
        retStr.append("\n\t");
        retStr.append("Compensate Id = " + getCompensateId());
        return retStr.toString();
    }
}
