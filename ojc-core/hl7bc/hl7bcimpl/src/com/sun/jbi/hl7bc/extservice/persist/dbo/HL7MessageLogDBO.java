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
package com.sun.jbi.hl7bc.extservice.persist.dbo;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Timestamp;
import javax.sql.rowset.serial.SerialException;

/**
 * HL7MessageLogDBO instance
 * @author S. Nageswara Rao
 */
public interface HL7MessageLogDBO extends DBObject {

	String SCHEMA_NAME = "HL7BC_SCHEMA";
    String TABLE_NAME_HL7MESSAGELOG = "HL7MESSAGELOG";
    
    /*
    String COLUMN_MESSAGEID = "MessageId";
    String COLUMN_APPLICATIONID = "ApplicationId";
    String COLUMN_REQUESTMESSAGE = "RequestMessage";
    String COLUMN_RESPONSEMESSAGE = "ResponseMessage";
    String COLUMN_STATUS = "Status";
    String COLUMN_CREATEDTIME = "CreatedTime";
    String COLUMN_LASTUPDATEDTIME = "LastUpdatedTime"; 
     */
            

    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + //$NON-NLS-1$
            TABLE_NAME_HL7MESSAGELOG + " VALUES (?, ?, ?, ?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + //$NON-NLS-1$
            TABLE_NAME_HL7MESSAGELOG + " SET status = ?, ResponseMessage = ?, LastUpdatedtime = ? " + //$NON-NLS-1$
            " WHERE messageId = ? and applicationId = ? "; //$NON-NLS-1$
    
    /** update statement */
    String BASE_UPDATE_STMT_WITH_STATUS_STR = "UPDATE " + //$NON-NLS-1$
            TABLE_NAME_HL7MESSAGELOG + " SET status = ?, ResponseMessage = ?, LastUpdatedtime = ? " + //$NON-NLS-1$
            " WHERE messageId = ? and applicationId = ? and status = ? "; //$NON-NLS-1$    

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + //$NON-NLS-1$
            TABLE_NAME_HL7MESSAGELOG + " WHERE messageId = ? and applicationId = ? "; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + //$NON-NLS-1$
            TABLE_NAME_HL7MESSAGELOG + " WHERE messageId = ? and applicationId = ? "; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_WITH_STATUS_STR = "SELECT * FROM " + //$NON-NLS-1$
            TABLE_NAME_HL7MESSAGELOG + " WHERE messageId = ? and applicationId = ? and status = ? "; //$NON-NLS-1$
    
    //Sql statements with schemaname.tablename
    /** insert statement */
    String BASE_INSERT_STMT_STR_WITH_SCHEMA = "INSERT INTO " + //$NON-NLS-1$
            SCHEMA_NAME + "." + TABLE_NAME_HL7MESSAGELOG + " VALUES (?, ?, ?, ?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR_WITH_SCHEMA = "UPDATE " + //$NON-NLS-1$
    	SCHEMA_NAME + "." + TABLE_NAME_HL7MESSAGELOG + " SET status = ?, ResponseMessage = ?, LastUpdatedtime = ? " + //$NON-NLS-1$
            " WHERE messageId = ? and applicationId = ? "; //$NON-NLS-1$
    
    /** update statement */
    String BASE_UPDATE_STMT_WITH_STATUS_STR_WITH_SCHEMA = "UPDATE " + //$NON-NLS-1$
    	SCHEMA_NAME + "." + TABLE_NAME_HL7MESSAGELOG + " SET status = ?, ResponseMessage = ?, LastUpdatedtime = ? " + //$NON-NLS-1$
            " WHERE messageId = ? and applicationId = ? and status = ? "; //$NON-NLS-1$    

    /** delete statement */
    String BASE_DELETE_STMT_STR_WITH_SCHEMA = "DELETE FROM " + //$NON-NLS-1$
    	SCHEMA_NAME + "." + TABLE_NAME_HL7MESSAGELOG + " WHERE messageId = ? and applicationId = ? "; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR_WITH_SCHEMA = "SELECT * FROM " + //$NON-NLS-1$
    	SCHEMA_NAME + "." + TABLE_NAME_HL7MESSAGELOG + " WHERE messageId = ? and applicationId = ? "; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_WITH_STATUS_STR_WITH_SCHEMA = "SELECT * FROM " + //$NON-NLS-1$
    	SCHEMA_NAME + "." + TABLE_NAME_HL7MESSAGELOG + " WHERE messageId = ? and applicationId = ? and status = ? "; //$NON-NLS-1$    
    /**
     * gets Message ID string
     *
     * @return Message ID
     */
	 String getMessageId();
    /**
     * sets Message ID string
     * @param messageId string value
     * @return void
     */    
    void setMessageId(String messageId);
    /**
     * gets Application ID string
     *
     * @return Application ID
     */
    String getApplicationId();
    /**
     * sets applicationId string
     * @param applicationId string value
     * @return void
     */ 
    void setApplicationId(String applicationId);
    /**
     * gets request Message string
     *
     * @return request message string
     */    
    String getRequestMessage();
    /**
     * sets requestMessage string
     * @param requestMessage string value
     * @return void
     * @throws SQLException SQLException
     */     
    void setRequestMessage(String requestMessage) throws SQLException;
    /**
     * gets response Message string
     *
     * @return response message string
     */
	 String getResponseMessage();
    /**
     * sets responseMessage string
     * @param responseMessage string value
     * @return void
     * @throws SQLException SQLException
     */    
    void setResponseMessage(String responseMessage) throws SQLException;
    /**
     * gets status
     *
     * @return status integer value
     */
    int getStatus();
    /**
     * sets status integer value
     * @param status integer value
     * @return void
     */    
    void setStatus(int status);
    /**
     * gets created timeStamp
     *
     * @return timeStamp value
     */
    Timestamp getCreatedTime();
    /**
     * sets createdTime value
     * @param createdTime TimeStamp value
     * @return void
     */    
    void setCreatedTime(Timestamp createdTime);
    /**
     * gets last updated timeStamp
     *
     * @return timeStamp value
     */    
    Timestamp getLastUpdatedTime();
    /**
     * sets last Updated Timestamp value
     * @param lastUpdatedTime TimeStamp value
     * @return void
     */    
    void setLastUpdatedTime(Timestamp lastUpdatedTime);
    /**
     * sets old status integer value
     * @param status integer value
     * @return void
     */    
    void setOldStatus(int status);
    /**
     * gets old status
     *
     * @return status integer value
     */    
    int getOldStatus();

    /**
     * fills query statement with status
     *
     * @param stmt PreparedStatement
     *
     * @throws SQLException SQLException
     */    
    public void fillQueryStmtWithStatus(PreparedStatement stmt) throws SQLException ;

    /**
     * fills update statement with status
     *
     * @param stmt PreparedStatement
     *
     * @throws SQLException SQLException
     */    
    public void fillUpdateStmtWithStatus(PreparedStatement stmt) throws SQLException ;

    /**
     * gets query statement
     *
     * @return String
     */    
    public String getQueryStmtWithStatus() ;
    /**
     * gets query statement
     *
     * @return String
     */
    
    public String getUpdateStmtWithStatus() ;
    

}
