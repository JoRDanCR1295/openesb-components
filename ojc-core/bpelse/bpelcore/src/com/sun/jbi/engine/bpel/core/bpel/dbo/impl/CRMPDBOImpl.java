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
 * @(#)CRMPDBOImpl.java 
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
import javax.sql.rowset.serial.SerialException;

import com.sun.jbi.engine.bpel.core.bpel.dbo.CRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class CRMPDBOImpl extends DBObjectImpl implements CRMPDBO {
	
	private String mBPId;
	private String mPartnerLink;
	private String mOperation;
	private String mBpelMsgExchange;
	private String mCRMPInvId;
	//private long mReplyVarId;
	private long mReplyVarId = -1;
	private Clob mResponseObj;
	
	private CRMPDBOImpl() {
		
	}
	
	public CRMPDBOImpl(int dbType, String bpId, String crmpInvId, 
				String partnerlink, String operation, String msgExchange) {
		this(dbType, false);
		mBPId = bpId;
		mCRMPInvId = crmpInvId;
		mPartnerLink = partnerlink;
		mOperation = operation;
		mBpelMsgExchange = msgExchange;
	}
	
	public CRMPDBOImpl(int dbType, String bpId, String partnerlink, 
			   String operation, String msgExchange, 
			   long replyVarId, char[] responseObj) 
		throws SerialException, SQLException{

		this(dbType, false);
		mBPId = bpId;
		mPartnerLink = partnerlink;
		mOperation = operation;
		mBpelMsgExchange = msgExchange;
		mReplyVarId = replyVarId;
		// responseObj cannot be null when using this constructor.
		mResponseObj = new SerialClob(responseObj);
	}

	public CRMPDBOImpl(int dbType, String crmpInvId, boolean isClusteredInvoke) {
		this(dbType, isClusteredInvoke);
		mCRMPInvId = crmpInvId;
	}

    public CRMPDBOImpl(int dbType, boolean isClusteredInvoke) {
        if (isClusteredInvoke) {
            init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, 
                    BASE_DELETE_STMT_STR, QUERY_STMT_FOR_CLUSTERED_INVOKE_STR);
        } else {
            init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, 
                    BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
        }
    }
    
	@Override
	public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getBPId());
		stmt.setString(2, getCRMPInvokeId());
		stmt.setString(3, getPartnerLink());
		stmt.setString(4, getOperation());
        stmt.setString(5, getBpelMessageExchange());
	}

	@Override
	public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
		stmt.setString(1, getCRMPInvokeId());
	}
	
    public void fillUpdateStmt(PreparedStatement stmt)
    	throws SQLException {
    	stmt.setLong(1, getReplyVariableId());
    	// in the update case the mResponseObj should never be null
    	stmt.setCharacterStream(2, mResponseObj.getCharacterStream(), (int) mResponseObj.length());
    	stmt.setString(3, getBPId());
    	stmt.setString(4, getPartnerLink());
    	stmt.setString(5, getOperation());
    }	

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#getNewObject()()()
     */
    public DBObject getNewObject() {
        return new CRMPDBOImpl();
    }	

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject#populateDBO(java.sql.ResultSet)
     */
    public void populateDBO(ResultSet rs) throws SQLException {
    	mBPId = rs.getString(1);
    	mPartnerLink = rs.getString(2);
    	mOperation = rs.getString(3);
    	mReplyVarId = rs.getLong(4);
    	mBpelMsgExchange = rs.getString(5);
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

	public String getCRMPInvokeId() {
		return mCRMPInvId;
	}

	public long getReplyVariableId() {
		return mReplyVarId;
	}
	
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
    
    public String toString() {
        StringBuilder retStr = new StringBuilder();
        retStr.append(super.toString());
        retStr.append("\n\t");
        retStr.append("BPID = " + getBPId());
        retStr.append("\n\t");
        retStr.append("CRMP ID = " + getCRMPInvokeId());
        retStr.append("\n\t");
        retStr.append("Operation name = " + getOperation());
        retStr.append("\n\t");
        retStr.append("Partner Link = " + getPartnerLink());
        retStr.append("\n\t");
        retStr.append("BpelMessageExcange = "+getBpelMessageExchange());
        retStr.append("\n\t");
        retStr.append("Reply Variable ID = " + getReplyVariableId());
        return retStr.toString();
    }
}
