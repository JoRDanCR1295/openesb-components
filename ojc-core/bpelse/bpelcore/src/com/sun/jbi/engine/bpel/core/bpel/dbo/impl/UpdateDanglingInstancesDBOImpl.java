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
 * @(#)UpdateDanglingInstancesDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.UpdateDanglingInstancesDBO;

public class UpdateDanglingInstancesDBOImpl extends DBObjectImpl implements UpdateDanglingInstancesDBO {
    
    String mEngineId;
    long mEngineExirationInterval;
    int mRecoveryBatchSize;
    int mDbType;
    
    /**
     * constructor
     *
     * @param dbType database type
     * @param engineExirationInterval 
     * @param engineId 
     */
    public UpdateDanglingInstancesDBOImpl(int dbType) {
    	this.mDbType = dbType;
    	
        if ((dbType == ConnectionProperties.DERBY_DB.intValue()) 
        	|| (dbType == ConnectionProperties.MYSQL_DB.intValue())) {
            init(null, BASE_UPDATE_STMT_STR, null, null);
        } else if (dbType == ConnectionProperties.ORCL_DB.intValue()) {
            init(null, ORCL_UPDATE_STMT_STR, null, null);
        } else if (dbType == ConnectionProperties.POSTGRES_DB.intValue()) {
            init(null, PGSQL_UPDATE_STMT_STR, null, null);
        } else {
            // TODO support for other databases
        }
    }    

    public UpdateDanglingInstancesDBOImpl(int dbType, String engineId, long engineExirationInterval, int recoveryBatchSize) {
        this(dbType);
        this.mEngineExirationInterval = engineExirationInterval;
        this.mEngineId = engineId;
        this.mRecoveryBatchSize = recoveryBatchSize;
    }

    public long getEngineExirationInterval() {
        return mEngineExirationInterval;
    }

    public String getEngineId() {
        return mEngineId;
    }

    public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
        // TODO Auto-generated method stub
    }

    public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getEngineId());
        stmt.setLong(2, getEngineExirationInterval());
        stmt.setString(3, getEngineId());
        
        // for failover scalabillity we need to limit the number of instances
        // of failed engine for failover (acquiring and recovering). For oracle
        // rownum function is used. For Derby, such function is under development
        // hence for now, no scalabillity support for failover is available, but
        // the queries will still work.
        // TODO when the equivalent of rownum for Derby is available, change the 
        // query for derby accordingly.
        if (mDbType == ConnectionProperties.ORCL_DB.intValue()) {
        	stmt.setInt(4, mRecoveryBatchSize);
        }
    }

    public void fillDeleteStmt(PreparedStatement stmt) throws SQLException {
        // TODO Auto-generated method stub
        
    }

    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        // TODO Auto-generated method stub
        
    }

    public void populateDBO(ResultSet rs) throws SQLException {
        // TODO Auto-generated method stub
        
    }

    public DBObject createNew() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected DBObject getNewObject() {
        // TODO Auto-generated method stub
        return null;
    }
    
    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("Engine ID = " + getEngineId());
        retStr.append("\n\t");
        retStr.append("Engine Expiration Interval = " + getEngineExirationInterval());
        return retStr.toString();
    }
}
