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
 * @(#)QueryCRMPDBOImpl.java 
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
import com.sun.jbi.engine.bpel.core.bpel.dbo.QueryCRMPDBO;

public class QueryCRMPDBOImpl extends DBObjectImpl implements QueryCRMPDBO {
	
	private String mBPId;
	private String mPartnerLink;
	private String mOperation;
	private String mBpelMsgExchange;
	private String mCRMPInvId;
	private long mReplyVarId;
	
	public QueryCRMPDBOImpl() {
		
	}
	
	public QueryCRMPDBOImpl(int dbType, String bpId) {
		init(null, null, null, BASE_QUERY_STMT_STR);
		mBPId = bpId;
	}
	
	@Override
	public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
		// TODO Auto-generated method stub

	}

	@Override
	public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
		//retrieve all the rows in the CRMP table for mBPId where replyVariableId = -1
		stmt.setString(1, getBPId());
	}

	@Override
	protected DBObject getNewObject() {
		return new QueryCRMPDBOImpl();
	}

	@Override
	public void populateDBO(ResultSet rs) throws SQLException {
    	mBPId = rs.getString(1);
    	mCRMPInvId = rs.getString(2);
    	mPartnerLink = rs.getString(3);
    	mOperation = rs.getString(4);
    	mBpelMsgExchange = rs.getString(5);
    	mReplyVarId = rs.getLong(6);

	}
	
    public String getPartnerLink() {
    	return mPartnerLink;
    }
    
    public String getOperation() {
    	return mOperation;
    }
    
    public String getBpelMessageExchange() {
    	return mBpelMsgExchange;
    }
    
	public String getBPId() {
		return mBPId;
	}
	
	public long getReplyVariableId() {
		return mReplyVarId;
	}	

}
