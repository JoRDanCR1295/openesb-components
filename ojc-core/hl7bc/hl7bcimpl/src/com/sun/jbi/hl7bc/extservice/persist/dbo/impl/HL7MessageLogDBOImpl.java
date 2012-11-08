/**************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.hl7bc.extservice.persist.dbo.impl;

import java.io.IOException;
import java.io.Reader;
import java.sql.Clob;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sql.rowset.serial.SerialClob;
import javax.sql.rowset.serial.SerialException;

import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.I18n;
import com.sun.jbi.hl7bc.extservice.persist.connection.ConnectionProperties;
import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObject;
import com.sun.jbi.hl7bc.extservice.persist.dbo.HL7MessageLogDBO;


public class HL7MessageLogDBOImpl extends DBObjectImpl implements HL7MessageLogDBO {
	
    
    private String mMessageId;
    private String mApplicationId;
    private Clob mRequestMessage;
    private Clob mResponseMessage;
    private int mStatus;
    private Timestamp mCreatedTime;
    private Timestamp mLastUpdatedTime;
    private int mOldStatus;
    private int dbType = ConnectionProperties.AXION_DB.intValue();

    private static final Logger log = Logger.getLogger(HL7MessageLogDBOImpl.class.getName());
    
    /**
     * constructor
     *
     * @param dbType database type
     */
    public HL7MessageLogDBOImpl(int dbType) {
    	this.dbType = dbType;
    	if(dbType == ConnectionProperties.AXION_DB.intValue()){
    		super.init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    	}else{
    		super.init(BASE_INSERT_STMT_STR_WITH_SCHEMA,BASE_UPDATE_STMT_STR_WITH_SCHEMA, BASE_DELETE_STMT_STR_WITH_SCHEMA, BASE_DELETE_STMT_STR_WITH_SCHEMA);
    	}
    }

    public HL7MessageLogDBOImpl(String messageId, String applicationId, int dbType) {
    	this.dbType = dbType;
        
    	if(dbType == ConnectionProperties.AXION_DB.intValue()){
    		super.init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    	}else{
    		super.init(BASE_INSERT_STMT_STR_WITH_SCHEMA,BASE_UPDATE_STMT_STR_WITH_SCHEMA, BASE_DELETE_STMT_STR_WITH_SCHEMA, BASE_DELETE_STMT_STR_WITH_SCHEMA);
    	}
        
        this.mMessageId = messageId;
        this.mApplicationId = applicationId;
    }

    private HL7MessageLogDBOImpl() {
    	
    	if(dbType == ConnectionProperties.AXION_DB.intValue()){
    		super.init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
    	}else{
    		super.init(BASE_INSERT_STMT_STR_WITH_SCHEMA,BASE_UPDATE_STMT_STR_WITH_SCHEMA, BASE_DELETE_STMT_STR_WITH_SCHEMA, BASE_DELETE_STMT_STR_WITH_SCHEMA);
    	}
    	
    }

    /**
     * constructor
     *
     * @param dbType database type
     * @param mcid Message Control ID
     * @param val vaule
     */
    public HL7MessageLogDBOImpl(int dbType, String messageId, String applicationId) {
        this(dbType);
        mMessageId = messageId;
        mApplicationId = applicationId;
    }
    
    /**
     * Returns Query Statement with status in where clause
     */
    public String getQueryStmtWithStatus() {
    	if(this.dbType == ConnectionProperties.AXION_DB.intValue()){
    		return BASE_QUERY_STMT_WITH_STATUS_STR;
    	}else{
    		return BASE_QUERY_STMT_WITH_STATUS_STR_WITH_SCHEMA;
    	}
    		
        
    }
    
    /**
     * Returns Update Statement with status in where clause
     */
    public String getUpdateStmtWithStatus() {
    	if(this.dbType == ConnectionProperties.AXION_DB.intValue()){
    		return BASE_UPDATE_STMT_WITH_STATUS_STR;
    	}else{
    		return BASE_UPDATE_STMT_WITH_STATUS_STR_WITH_SCHEMA;
    	}
    		
    }

    /**
     * fills delete statement with message ID and Application ID
     *
     * @param stmt PreparedStatement
     *
     * @throws SQLException SQLException
     */
    public void fillDeleteStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getMessageId());
        stmt.setString(2, getApplicationId());
        stmt.setInt(3, getOldStatus());
    }

    /**
     * fills insert statement
     *
     * @param stmt PreparedStatement
     *
     * @throws SQLException SQLException
     */
    public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getMessageId());
        stmt.setString(2, getApplicationId());
		stmt.setCharacterStream(3, mRequestMessage.getCharacterStream(), (int) mRequestMessage.length());
        if(this.mResponseMessage != null){
            stmt.setCharacterStream(4, mResponseMessage.getCharacterStream(), (int) mResponseMessage.length());            
        }else{
        //stmt.setString(3, getRequestMessage());
            stmt.setNull(4,java.sql.Types.VARCHAR);
        }
        stmt.setInt(5, getStatus());
        stmt.setTimestamp(6, getCreatedTime());
        stmt.setTimestamp(7, getLastUpdatedTime());
    }

    /**
     * fills update statement with message ID and Application ID
     *
     * @param stmt PreparedStatement
     *
     * @throws SQLException SQLException
     */
    public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
        stmt.setInt(1,getStatus());
        
        if(getResponseMessage() == null){
            stmt.setNull(2, java.sql.Types.CLOB);
        }else{
            //stmt.setString(2, getResponseMessage());
			stmt.setCharacterStream(2, mResponseMessage.getCharacterStream(), (int) mResponseMessage.length());
        }
        
        stmt.setTimestamp(3, new Timestamp(System.currentTimeMillis()));
        
        stmt.setString(4, getMessageId());
        stmt.setString(5, getApplicationId());

    }

    /**
     * fills update statement with status
     *
     * @param stmt PreparedStatement
     *
     * @throws SQLException SQLException
     */    
    public void fillUpdateStmtWithStatus(PreparedStatement stmt) throws SQLException {
        stmt.setInt(1,getStatus());
        
        if(getResponseMessage() == null){
            stmt.setNull(2, java.sql.Types.CLOB);
        }else{
            //stmt.setString(2, getResponseMessage());
			stmt.setCharacterStream(2, mResponseMessage.getCharacterStream(), (int) mResponseMessage.length());
        }
        
        stmt.setTimestamp(3, new Timestamp(System.currentTimeMillis()));
        
        stmt.setString(4, getMessageId());
        stmt.setString(5, getApplicationId());
        stmt.setInt(6, getOldStatus());

    }

    /**
     * fills query statement with message ID and Application ID
     *
     * @param stmt PreparedStatement
     *
     * @throws SQLException SQLException
     */
    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getMessageId());
        stmt.setString(2, getApplicationId());
    }

    /**
     * fills query statement with status
     *
     * @param stmt PreparedStatement
     *
     * @throws SQLException SQLException
     */    
    public void fillQueryStmtWithStatus(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getMessageId());
        stmt.setString(2, getApplicationId());
        stmt.setInt(3, getOldStatus());
    }

    /**
     * populate DBO Object
     *
     * @param rs ResultSet
     *
     * @throws SQLException SQLException
     */
    public void populateDBO(ResultSet rs) throws SQLException {
        mMessageId = rs.getString(1);
        mApplicationId = rs.getString(2);
		try	{
			Clob reqClob = rs.getClob(3);
			mRequestMessage = new SerialClob(reqClob);
			mResponseMessage = null;
			Clob resClob = rs.getClob(4);
			if(resClob != null) {
				mResponseMessage = new SerialClob(resClob);
			} 
		} catch (SerialException se){
			throw new SQLException(se.getLocalizedMessage());
		}
        mStatus = rs.getInt(5);
        mCreatedTime = rs.getTimestamp(6);
        mLastUpdatedTime = rs.getTimestamp(7);
    }

    /**
     * gets HL7MessageLogDBO object
	 * @return HL7MessageLogDBOImpl
     */
    public DBObject getNewObject() {
        return new HL7MessageLogDBOImpl();
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#getMessageId().
     */    
    public String getMessageId() {
        return mMessageId;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#getApplicationId().
     */    
    public String getApplicationId() {
        return mApplicationId;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#getRequestMessage().
     */
    public String getRequestMessage() {
		return getClobValAsString(mRequestMessage);
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#getResponseMessage().
     */
    public String getResponseMessage() {
		return getClobValAsString(mResponseMessage);
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#getStatus().
     */
    public int getStatus() {
        return mStatus;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#getCreatedTime().
     */
    public Timestamp getCreatedTime() {
        return mCreatedTime;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#getLastUpdatedTime().
     */
    public Timestamp getLastUpdatedTime() {
        return mLastUpdatedTime;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#setMessageId().
     */
    public void setMessageId(String messageId) {
        this.mMessageId =  messageId;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#setApplicationId().
     */
    public void setApplicationId(String applicationId) {
        this.mApplicationId = applicationId;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#setRequestMessage().
     */
    public void setRequestMessage(String requestMessage) throws SQLException {
		char[] requestCharArray = requestMessage.toCharArray();
        this.mRequestMessage = new SerialClob(requestCharArray);
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#setResponseMessage().
     */
    public void setResponseMessage(String responseMessage) throws SQLException {
		this.mResponseMessage = null;
		if(responseMessage != null) {
			char[] responseCharArray = responseMessage.toCharArray();
			this.mResponseMessage = new SerialClob(responseCharArray);
		}
	 }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#setStatus().
     */
    public void setStatus(int status) {
        this.mStatus = status;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#setCreatedTime().
     */
    public void setCreatedTime(Timestamp createdTime) {
        this.mCreatedTime = createdTime;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#setLastUpdatedTime().
     */
    public void setLastUpdatedTime(Timestamp lastUpdatedTime) {
        this.mLastUpdatedTime = lastUpdatedTime;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#setOldStatus().
     */
	public void setOldStatus(int status) {
		this.mOldStatus = status;
	}

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.HL7MessageLogDBO#getOldStatus().
     */
	public int getOldStatus() {
		return this.mOldStatus;
	}

	private String getClobValAsString(Clob clob) {
		Reader reader = null;
		int clobValLength = -1;
		if(clob != null) {
			try {
				reader = clob.getCharacterStream();
				clobValLength = (int) clob.length();
			} catch (SQLException e) {
				return null;
			}
			char[] msgCharArray = new char[clobValLength];
			try {
				reader.read(msgCharArray);
			} catch(IOException ioe) {
				log.severe(I18n.msg("E0301: HL7MessageLogDBOImpl.getClobValAsString(): Failed reading data from java.io.reader into charArray; reason is : {0} ", ioe.getLocalizedMessage()));
			}
			return new String(msgCharArray);
		}
		return null;

	}
    

}
