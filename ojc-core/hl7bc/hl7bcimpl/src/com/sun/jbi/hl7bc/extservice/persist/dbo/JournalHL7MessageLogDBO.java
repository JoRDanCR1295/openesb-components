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
 * JournalHL7MessageLogDBO instance
 * @author Raghunadh Teegavarapu
 */
public interface JournalHL7MessageLogDBO extends DBObject {

	String SCHEMA_NAME = "HL7BC_SCHEMA";
    String TABLE_NAME_JOURNALHL7MESSAGELOG = "JOURNALHL7MESSAGELOG";    
           

    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + //$NON-NLS-1$
            TABLE_NAME_JOURNALHL7MESSAGELOG + " VALUES (?, ?, ?, ?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + //$NON-NLS-1$
            TABLE_NAME_JOURNALHL7MESSAGELOG + " SET status = ?, ProcessMode = ?, RequestHL7Message = ?, ResponseACKMessage = ?, ResponseNAKMessage = ?" + //$NON-NLS-1$
            " WHERE messagecontrolId = ? and processmode = ? and applicationid = ?"; //$NON-NLS-1$
    
   
    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + //$NON-NLS-1$
            TABLE_NAME_JOURNALHL7MESSAGELOG + " WHERE messagecontrolId = ? and processmode = ? and applicationid = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + //$NON-NLS-1$
            TABLE_NAME_JOURNALHL7MESSAGELOG + " WHERE messagecontrolId = ? and processmode = ? and applicationid = ?"; //$NON-NLS-1$
	
	// Sql Statements with schema.table name               

    /** insert statement */
    String BASE_INSERT_STMT_STR_WITH_SCHEMA = "INSERT INTO " + //$NON-NLS-1$
            SCHEMA_NAME + "." + TABLE_NAME_JOURNALHL7MESSAGELOG + " VALUES (?, ?, ?, ?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR_WITH_SCHEMA = "UPDATE " + //$NON-NLS-1$
            SCHEMA_NAME + "." + TABLE_NAME_JOURNALHL7MESSAGELOG + " SET status = ?, ProcessMode = ?, RequestHL7Message = ?, ResponseACKMessage = ?, ResponseNAKMessage = ?" + //$NON-NLS-1$
            " WHERE messagecontrolId = ? and processmode = ? and applicationid = ?"; //$NON-NLS-1$
    
   
    /** delete statement */
    String BASE_DELETE_STMT_STR_WITH_SCHEMA = "DELETE FROM " + //$NON-NLS-1$
            SCHEMA_NAME + "." + TABLE_NAME_JOURNALHL7MESSAGELOG + " WHERE messagecontrolId = ? and processmode = ? and applicationid = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR_WITH_SCHEMA = "SELECT * FROM " + //$NON-NLS-1$
            SCHEMA_NAME + "." + TABLE_NAME_JOURNALHL7MESSAGELOG + " WHERE messagecontrolId = ? and processmode = ? and applicationid = ?"; //$NON-NLS-1$

   
    /**
     * gets Message ID string
     *
     * @return Message ID
     */
	 String getMessageControlId();
    /**
     * sets Message ID string
     * @param messageId string value
     * @return void
     */    
    void setMessageControlId(String messageControlId);
	 /**
     * gets Application ID string
     *
     * @return Application ID
     */
	 String getApplicationId();
    /**
     * sets Application  ID string
     * @param applicationid string value
     * @return void
     */    
    void setApplicationId(String applicationID);
    /**
     * gets Mode string
     *
     * @return Mode
     */
	 String getMode();
    /**
     * sets Mode string
     * @param mode string value
     * @return void
     */    
    void setMode(String mode);
    /**
     * gets request HL7 Message string
     *
     * @return request HL7 message string
     */    
    String getRequestHL7Message();
    /**
     * sets requestHL7Message string
     * @param requestHL7Message string value
     * @return void
     * @throws SQLException SQLException
     */     
    void setRequestHL7Message(String requestHL7Message) throws SQLException;
    /**
     * gets response ACK Message string
     *
     * @return response ACK message string
     */
	 String getResponseACKMessage();
    /**
     * sets responseACKMessage string
     * @param responseACKMessage string value
     * @return void
     * @throws SQLException SQLException
     */    
    void setResponseACKMessage(String responseACKMessage) throws SQLException;
    /**
     * gets response NAK Message string
     *
     * @return response NAK message string
     */
	 String getResponseNAKMessage();
    /**
     * sets responseNAKMessage string
     * @param responseNAKMessage string value
     * @return void
     * @throws SQLException SQLException
     */    
    void setResponseNAKMessage(String responseNAKMessage) throws SQLException;
    /**
     * gets status
     *
     * @return status string value
     */
    String getStatus();
    /**
     * sets status integer value
     * @param status integer value
     * @return void
     */    
    void setStatus(String status);    

}
