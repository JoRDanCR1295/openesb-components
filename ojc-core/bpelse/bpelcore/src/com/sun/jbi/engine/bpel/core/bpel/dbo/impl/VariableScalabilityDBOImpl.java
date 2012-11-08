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
import java.sql.Clob;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.sql.rowset.serial.SerialClob;
import javax.sql.rowset.serial.SerialException;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.VariableScalabilityDBO;
import sun.net.ConnectionResetException;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class VariableScalabilityDBOImpl extends DBObjectImpl implements VariableScalabilityDBO {

	private String mBPId;
    private long mId;
    private Clob mContent;
    private String mScopeGuid;
    private String mScalabilityPassivatedFlag;
    private boolean mIsCleanUpDuringRecovery;
    private int mDbType;

    /**
     * constructor
     *
     * @param dbType database type
     */
    public VariableScalabilityDBOImpl(int dbType) {
    	mDbType = dbType;
        init(INSERT_PASSIVATED_VARIABLE_STMT_STR, UPDATE_PASSIVATED_VAR_SCALABILITY_FLAG_STMT_STR, DELETE_PASSIVATED_VAR_STMT_STR, QUERY_PASSIVATED_VAR_STMT_STR);
    }

    private VariableScalabilityDBOImpl() {
    }
    
    public VariableScalabilityDBOImpl(int dbType, String instanceId) {
       	init(null, null, DELETE_ALL_PASSIVATED_VARS_STMT_STR, null);
        mBPId = instanceId;
    	this.mIsCleanUpDuringRecovery = true;
    }
    
    public VariableScalabilityDBOImpl(int dbType, String bpId, long id, char[] val, String scopeGuid) throws SerialException, SQLException {
        this(dbType);
        mBPId = bpId;
        mId = id;
        mContent = (val == null) ? new SerialClob(new char[] {}) : new SerialClob(val);
        mScopeGuid = scopeGuid;
    }
    
    public VariableScalabilityDBOImpl(int dbType, String bpId, long id, String scopeGuid) {
        this(dbType);
        mBPId = bpId;
        mId = id;
        mScopeGuid = scopeGuid;
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
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO#getScopeGuid()
     */
    public String getScopeGuid() {
    	return mScopeGuid;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
		stmt.setString(1, getBPId());
		stmt.setLong(2, getId());
		stmt.setCharacterStream(3, mContent.getCharacterStream(), (int) mContent.length());
		stmt.setString(4, getScopeGuid());
	}
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt) throws SQLException {
    	if (mIsCleanUpDuringRecovery) {
    		stmt.setString(1, mBPId);
    	} else {
    		stmt.setString(1, mBPId);
    		stmt.setLong(2, mId);
    		stmt.setString(3, getScopeGuid());
    	}
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
		stmt.setString(1, getBPId());
		stmt.setLong(2, getId());
		stmt.setString(3, getScopeGuid());
	}

    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillQueryStmt(java.sql.PreparedStatement)
	 */
    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
		stmt.setString(1, getBPId());
		stmt.setLong(2, getId());
		stmt.setString(3, getScopeGuid());
	}

    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#populateDBO(java.sql.ResultSet)
	 */
    public void populateDBO(ResultSet rs) throws SQLException {
        mBPId = rs.getString(1);
        mId = rs.getLong(2);
        mScalabilityPassivatedFlag = rs.getString(3);
        if( mDbType == ConnectionProperties.POSTGRES_DB.intValue() ) {
            String tmpRes = rs.getString(4);
            mContent = new SerialClob( (tmpRes!=null)?tmpRes.toCharArray():null );
        } else {
            mContent = new SerialClob(rs.getClob(4));
        }
        mScopeGuid = rs.getString(5);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new VariableScalabilityDBOImpl();
    }

	public String getScalabilityPassivatedFlag() {
		return mScalabilityPassivatedFlag;
	}
    
    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getBPId());
        retStr.append("\n\t");
        retStr.append("Variable ID = " + getId());
        retStr.append("\n\t");
        retStr.append("Scope GUID = " + getScopeGuid());
        retStr.append("\n\t");
        retStr.append("Scalability Passivated = " + getScalabilityPassivatedFlag());
        retStr.append("\n\t");
        retStr.append("Value = 'some clob'");
        return retStr.toString();
    }
}