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
 * @(#)QueryRespCRMPDBOImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.io.Reader;
import java.sql.Clob;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.sql.rowset.serial.SerialClob;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.QueryRespCRMPDBO;

public class QueryRespCRMPDBOImpl extends DBObjectImpl implements QueryRespCRMPDBO {
	private Clob mResponseObj;
    private String mCRMPInvId;
    private int mDbType;

    public QueryRespCRMPDBOImpl() {
		
	}
	
	public QueryRespCRMPDBOImpl(int dbType, String crmpInvokeId) {
		mDbType = dbType;
        init(null, null, null, BASE_QUERY_STMT_STR);
        mCRMPInvId = crmpInvokeId;
	}

	@Override
	public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
		// TODO Auto-generated method stub

	}

	@Override
	public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
		//retrieve all the rows in the CRMP table for mBPId where replyVariableId = -1
		stmt.setString(1, getCRMPInvokeId());
	}

	@Override
	protected DBObject getNewObject() {
		return new QueryRespCRMPDBOImpl();
	}

	@Override
	public void populateDBO(ResultSet rs) throws SQLException {
    	if( mDbType == ConnectionProperties.POSTGRES_DB.intValue() ) {
            String tmpRes = rs.getString(1);
            if( tmpRes != null ) {
                mResponseObj = new SerialClob(tmpRes.toCharArray());
            }
        } else {
            Clob tmpResObj = rs.getClob(1);
            if (tmpResObj != null) {
                mResponseObj = new SerialClob(tmpResObj);
            }
        }

	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.dbo.QueryRespCRMPDBO#getResponseObj()
	 */
	public Reader getResponseObj() {
        Reader retVal = null;
        try {
        	if (mResponseObj != null) {
        		retVal = mResponseObj.getCharacterStream();
        	}
        } catch (SQLException e) {
            //retVal = new StringReader("");
        }
        return retVal;		
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.dbo.QueryRespCRMPDBO#getCRMPInvokeId()
	 */
	public String getCRMPInvokeId() {
		return mCRMPInvId;
	}
}
