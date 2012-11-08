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
 * @(#)EngineDBOImpl.java 
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
import com.sun.jbi.engine.bpel.core.bpel.dbo.EngineDBO;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class EngineDBOImpl extends DBObjectImpl implements EngineDBO {
    private String mId;
    private String mLocation;
    private long mExpiration;

    /**
     * constructor
     *
     * @param dbType database type
     */
    public EngineDBOImpl(int dbType) {
        if ((dbType == ConnectionProperties.DERBY_DB.intValue()) 
        		|| (dbType == ConnectionProperties.MYSQL_DB.intValue())) {
            init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR,
                    BASE_QUERY_STMT_STR);
        } else if (dbType == ConnectionProperties.ORCL_DB.intValue()) {
            init(ORCL_INSERT_STMT_STR, ORCL_UPDATE_STMT_STR, BASE_DELETE_STMT_STR,
                    BASE_QUERY_STMT_STR);
        } else if (dbType == ConnectionProperties.POSTGRES_DB.intValue()) {
            init(PGSQL_INSERT_STMT_STR, PGSQL_UPDATE_STMT_STR, BASE_DELETE_STMT_STR,
                    BASE_QUERY_STMT_STR);
        }{
            // TODO support for other databases
        }
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param id engine ID
     * @param location engine location
     * @param expiration engine expiration
     */
    public EngineDBOImpl(int dbType, String id, String location) {
        this(dbType);
        mId = id;
        mLocation = location;
    }

    private EngineDBOImpl() {
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.EngineDBO#getId()
     */
    public String getId() {
        return mId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.EngineDBO#getLocation()
     */
    public String getLocation() {
        return mLocation;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.EngineDBO#getExpiration()
     */
    public long getExpiration() {
        return mExpiration;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt)
        throws SQLException {
        stmt.setString(1, getId());
        stmt.setString(2, getLocation());
        //stmt.setLong(3, System.currentTimeMillis() + getExpiration());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt)
        throws SQLException {
        //stmt.setLong(1, getExpiration());
        //stmt.setString(2, getId());
        stmt.setString(1, getId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillQueryStmt(java.sql.PreparedStatement)
     */
    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        // TODO Auto-generated method stub
        stmt.setString(1, getId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#populateDBO(java.sql.ResultSet)
     */
    public void populateDBO(ResultSet rs) throws SQLException {
        mId = rs.getString(1);
        mLocation = rs.getString(2);
        mExpiration = rs.getDate(3).getTime();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new EngineDBOImpl();
    }
}
