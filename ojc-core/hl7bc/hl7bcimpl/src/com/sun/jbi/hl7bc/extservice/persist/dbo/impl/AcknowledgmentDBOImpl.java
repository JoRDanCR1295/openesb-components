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
 * @(#)AcknowledgmentDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.persist.dbo.impl;

import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObject;
import com.sun.jbi.hl7bc.extservice.persist.dbo.AcknowledgmentDBO;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 *
 * @author S. Nageswara Rao
 */
public class AcknowledgmentDBOImpl extends DBObjectImpl implements AcknowledgmentDBO {
    private String mMcid;

    private String mContent;

    /**
     * constructor
     *
     * @param dbType database type
     */
    public AcknowledgmentDBOImpl(int dbType) {
        init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    }

    private AcknowledgmentDBOImpl() {
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param mcid Message Control ID
     * @param val vaule
     */
    public AcknowledgmentDBOImpl(int dbType, String mcid, String val) {
        this(dbType);
        mMcid = mcid;
        mContent = val;
    }

    /**
     * get Message Control ID
     *
     * @return String messagecontrolID
     */
    public String getmcid() {
        return mMcid;
    }

    /**
     * get Acknowledgment Message
     *
     * @return String acknowledgment
     */
    public String getMessage() {
        return mContent;
    }

    public void fillDeleteStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getmcid());
    }

    public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getmcid());
        stmt.setString(2, getMessage());
    }

    public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
        //stmt.setString(1, getmcid());
        //stmt.setCharacterStream(2, getMessage().getCharacterStream(), (int) getMessage().length());

    }

    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getmcid());
    }

    public void populateDBO(ResultSet rs) throws SQLException {
        mMcid = rs.getString(1);
        mContent = rs.getString(2);
    }

    public DBObject getNewObject() {
        return new AcknowledgmentDBOImpl();
    }
}
