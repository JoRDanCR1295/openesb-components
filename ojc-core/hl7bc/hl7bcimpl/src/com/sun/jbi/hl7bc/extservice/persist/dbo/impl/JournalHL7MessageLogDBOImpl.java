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
import com.sun.jbi.hl7bc.extservice.persist.dbo.JournalHL7MessageLogDBO;

public class JournalHL7MessageLogDBOImpl extends DBObjectImpl implements JournalHL7MessageLogDBO {

    private String mMessageControlId;

    private String mApplicationId;

    private Clob mRequestHL7Message;

    private Clob mResponseACKMessage;

    private Clob mResponseNAKMessage;

    private String mStatus;

    private String mMode;

    private int dbType = ConnectionProperties.AXION_DB.intValue();

    private static final Logger log = Logger.getLogger(JournalHL7MessageLogDBOImpl.class.getName());

    /**
     * constructor
     * 
     * @param dbType database type
     */
    public JournalHL7MessageLogDBOImpl(int dbType) {
        this.dbType = dbType;
        if (dbType == ConnectionProperties.AXION_DB.intValue()) {
            super.init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
        } else {
            super.init(BASE_INSERT_STMT_STR_WITH_SCHEMA, BASE_UPDATE_STMT_STR_WITH_SCHEMA,
                    BASE_DELETE_STMT_STR_WITH_SCHEMA, BASE_QUERY_STMT_STR_WITH_SCHEMA);
        }
    }

    public JournalHL7MessageLogDBOImpl(String messageControlId, String applicationId, int dbType) {
        this.dbType = dbType;
        if (dbType == ConnectionProperties.AXION_DB.intValue()) {
            super.init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
        } else {
            super.init(BASE_INSERT_STMT_STR_WITH_SCHEMA, BASE_UPDATE_STMT_STR_WITH_SCHEMA,
                    BASE_DELETE_STMT_STR_WITH_SCHEMA, BASE_QUERY_STMT_STR_WITH_SCHEMA);
        }
        this.mMessageControlId = messageControlId;
        this.mApplicationId = applicationId;
    }

    private JournalHL7MessageLogDBOImpl() {
        if (dbType == ConnectionProperties.AXION_DB.intValue()) {
            super.init(BASE_INSERT_STMT_STR, BASE_UPDATE_STMT_STR, BASE_DELETE_STMT_STR, BASE_QUERY_STMT_STR);
        } else {
            super.init(BASE_INSERT_STMT_STR_WITH_SCHEMA, BASE_UPDATE_STMT_STR_WITH_SCHEMA,
                    BASE_DELETE_STMT_STR_WITH_SCHEMA, BASE_QUERY_STMT_STR_WITH_SCHEMA);
        }
    }

    /**
     * constructor
     * 
     * @param dbType database type
     * @param mcid Message Control ID
     * @param val vaule
     */
    public JournalHL7MessageLogDBOImpl(int dbType, String messageControlId, String applicationId) {
        this(dbType);
        mMessageControlId = messageControlId;
        mApplicationId = applicationId;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#getMessageControlId().
     */
    public String getMessageControlId() {
        return mMessageControlId;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#setApplicationId().
     */
    public void setApplicationId(String val) {
        mApplicationId = val;
        ;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#getApplicationId().
     */
    public String getApplicationId() {
        return mApplicationId;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#setMessageControlId().
     */
    public void setMessageControlId(String val) {
        mMessageControlId = val;
        ;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#getMode().
     */
    public String getMode() {
        return mMode;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#setMode().
     */
    public void setMode(String val) {
        mMode = val;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#getRequestHL7Message().
     */
    public String getRequestHL7Message() {
        return getClobValAsString(mRequestHL7Message);
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#setRequestHL7Message().
     */
    public void setRequestHL7Message(String val) throws SQLException {
        char[] requestCharArray = val.toCharArray();
        this.mRequestHL7Message = new SerialClob(requestCharArray);
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#getResponseACKMessage().
     */
    public String getResponseACKMessage() {
        return getClobValAsString(mResponseACKMessage);
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#setResponseACKMessage().
     */
    public void setResponseACKMessage(String val) throws SQLException {
        if (val != null) {
            char[] responseCharArray = val.toCharArray();
            this.mResponseACKMessage = new SerialClob(responseCharArray);
        }
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#getResponseNAKMessage().
     */
    public String getResponseNAKMessage() {
        return getClobValAsString(mResponseNAKMessage);
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#setResponseNAKMessage().
     */
    public void setResponseNAKMessage(String val) throws SQLException {
        if (val != null) {
            char[] responseCharArray = val.toCharArray();
            this.mResponseNAKMessage = new SerialClob(responseCharArray);
        }
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#getStatus().
     */
    public String getStatus() {
        return mStatus;
    }

    /**
     * @see com.stc.connector.hl7.framework.persist.dbo.JournalHL7MessageLogDBO#setStatus().
     */
    public void setStatus(String val) {
        mStatus = val;
    }

    /**
     * fills delete statement with message ID and Application ID
     * 
     * @param stmt PreparedStatement
     * @throws SQLException SQLException
     */
    public void fillDeleteStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getMessageControlId());
        stmt.setString(2, getMode());
        stmt.setString(3, getApplicationId());
    }

    /**
     * fills insert statement
     * 
     * @param stmt PreparedStatement
     * @throws SQLException SQLException
     */
    public void fillInsertStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getApplicationId());
        stmt.setString(2, getMessageControlId());
        stmt.setString(3, getMode());
        stmt.setCharacterStream(4, mRequestHL7Message.getCharacterStream(), (int) mRequestHL7Message.length());
        if (getResponseACKMessage() == null) {
            stmt.setNull(5, java.sql.Types.CLOB);
        } else {
            stmt.setCharacterStream(5, mResponseACKMessage.getCharacterStream(), (int) mResponseACKMessage.length());
        }
        if (getResponseNAKMessage() == null) {
            stmt.setNull(6, java.sql.Types.CLOB);
        } else {
            stmt.setCharacterStream(6, mResponseNAKMessage.getCharacterStream(), (int) mResponseNAKMessage.length());
        }
        stmt.setString(7, getStatus());
    }

    /**
     * fills query statement with message ID and Application ID
     * 
     * @param stmt PreparedStatement
     * @throws SQLException SQLException
     */
    public void fillQueryStmt(PreparedStatement stmt) throws SQLException {
        stmt.setString(1, getMessageControlId());
        stmt.setString(2, getMode());
        stmt.setString(3, getApplicationId());
    }

    /**
     * populate DBO Object
     * 
     * @param rs ResultSet
     * @throws SQLException SQLException
     */
    public void populateDBO(ResultSet rs) throws SQLException {
        mApplicationId = rs.getString(1);
        mMessageControlId = rs.getString(2);
        mMode = rs.getString(3);
        try {
            Clob reqClob = rs.getClob(4);
            mRequestHL7Message = new SerialClob(reqClob);
            mResponseACKMessage = null;
            Clob resACKClob = rs.getClob(5);
            if (resACKClob != null) {
                mResponseACKMessage = new SerialClob(resACKClob);
            }
            mResponseNAKMessage = null;
            Clob resNAKClob = rs.getClob(6);
            if (resNAKClob != null) {
                mResponseNAKMessage = new SerialClob(resNAKClob);
            }
        } catch (SerialException se) {
            throw new SQLException(se.getLocalizedMessage());
        }
        mStatus = rs.getString(7);
    }

    /**
     * gets HL7MessageLogDBO object
     * 
     * @return HL7MessageLogDBOImpl
     */
    public DBObject getNewObject() {
        return new JournalHL7MessageLogDBOImpl();
    }

    private String getClobValAsString(Clob clob) {
        Reader reader = null;
        int clobValLength = -1;
        if (clob != null) {
            try {
                reader = clob.getCharacterStream();
                clobValLength = (int) clob.length();
            } catch (SQLException e) {
                return null;
            }
            char[] msgCharArray = new char[clobValLength];
            try {
                reader.read(msgCharArray);
            } catch (IOException ioe) {
                log.severe(I18n.msg(
                        "E0331: JournalHL7MessageLogDBOImpl.getClobValAsString(): Failed reading data from java.io.reader into charArray; reason is : {0} ",
                        ioe.getLocalizedMessage()));
            }
            return new String(msgCharArray);
        }
        return null;

    }

}
