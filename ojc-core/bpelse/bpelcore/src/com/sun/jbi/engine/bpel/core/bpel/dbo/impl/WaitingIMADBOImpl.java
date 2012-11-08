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
 * @(#)WaitingIMADBOImpl.java 
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
import com.sun.jbi.engine.bpel.core.bpel.dbo.WaitingIMADBO;

/**
 * DOCUMENT ME!
 * 
 * @author Sun Microsystems
 */
public class WaitingIMADBOImpl extends DBObjectImpl implements WaitingIMADBO {
    private String mId;

    private String mPartnerLinkName;

    private String mOperationName;

    /**
     * constructor
     * 
     * @param dbType database type
     */
    public WaitingIMADBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, null, null, null);
    }

    public WaitingIMADBOImpl(int dbType, String id, String partnerLinkName, String operationName) {
        this(dbType);
        this.mId = id;
        this.mPartnerLinkName = partnerLinkName;
        this.mOperationName = operationName;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.EngineDBO#getId()
     */
    public String getId() {
        return mId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillDeleteStmt(java.sql.PreparedStatement)
     */
    public void fillDeleteStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillInsertStmt(java.sql.PreparedStatement)
     */
    public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getId());
        stmt.setString(2, getPartnerLinkName());
        stmt.setString(3, getOperationName());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillUpdateStmt(java.sql.PreparedStatement)
     */
    public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
        // stmt.setLong(1, getExpiration());
        // stmt.setString(2, getId());
        stmt.setString(1, getId());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#fillQueryStmt(java.sql.PreparedStatement)
     */
    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getId());
        stmt.setString(2, getPartnerLinkName());
        stmt.setString(3, getOperationName());
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#populateDBO(java.sql.ResultSet)
     */
    public void populateDBO(ResultSet rs) throws SQLException {
        mId = rs.getString(1);
        mPartnerLinkName = rs.getString(2);
        mOperationName = rs.getString(3);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return null;
    }

    public String getPartnerLinkName() {
        return mPartnerLinkName;
    }

    public String getOperationName() {
        // TODO Auto-generated method stub
        return mOperationName;
    }

    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getId());
        retStr.append("\n\t");
        retStr.append("Operation Name = " + getOperationName());
        retStr.append("\n\t");
        retStr.append("PartnerLink Name = " + getPartnerLinkName());
        return retStr.toString();
    }
}
