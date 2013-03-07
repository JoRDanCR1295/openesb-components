package com.sun.jbi.hl7bc.extservice.persist;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.sql.Connection;

import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnection;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObjectFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.HL7MessageLogDBO;
import com.sun.jbi.hl7bc.util.Base64Util;
import com.sun.jbi.hl7bc.I18n;

public class MLLPV1PersistenceHandler {

    private static final Logger logger = Logger.getLogger(MLLPV1PersistenceHandler.class.getName());

    public static final int STATUS_HL7_ACK_SENT = 1;

    public static final int STATUS_HL7_NAK_SENT = 2;

    private DBObjectFactory mDBObjectFactory;

    private String applicationId;

    private DBConnectionFactory mDBConnectionFactory;

    private String response;

    public MLLPV1PersistenceHandler(DBConnectionFactory dbConnectionFactory, DBObjectFactory dbObjectFactory) {
        this.mDBConnectionFactory = dbConnectionFactory;
        this.mDBObjectFactory = dbObjectFactory;
    }

    public boolean storeMessageInHL7MessageLog(String message, String ack) throws Exception {
        DBConnection dbCon = null;
        try {
            dbCon = getDBConnection();
            // ((Connection)dbCon).setAutoCommit(false);
            HL7MessageLogDBO hl7LogDBO = mDBObjectFactory.createHL7MessageLogDBO();

            String digestedContent = digestMessage(message);

            hl7LogDBO.setMessageId(digestedContent);
            hl7LogDBO.setApplicationId(applicationId);
            hl7LogDBO.setRequestMessage(message);
            hl7LogDBO.setResponseMessage(ack);
            hl7LogDBO.setCreatedTime(new Timestamp(System.currentTimeMillis()));
            hl7LogDBO.setLastUpdatedTime(new Timestamp(System.currentTimeMillis()));
            hl7LogDBO.setStatus(STATUS_HL7_ACK_SENT);
            dbCon.insert(hl7LogDBO);
            dbCon.getUnderlyingConnection().commit();
        } catch (Exception ex) {
            dbCon.getUnderlyingConnection().rollback();
            throw ex;
        } finally {
            if (dbCon != null) {
                dbCon.close();
            }
        }
        return true;
    }

    private String digestMessage(String message) throws NoSuchAlgorithmException {
        MessageDigest md = MessageDigest.getInstance("SHA-1");
        String digestedContent = new String(new Base64Util().encode(md.digest()));
        ;
        return digestedContent;
    }

    /**
     * Method checkDuplicateAndsendResponseFromLog used only for MLLPV2.
     * 
     * @param messageId-The messageID extract from the HL7 Message.
     * @param applicationId-Application ID.
     * @param oldStatus-old Status.
     * @param newStatus-New Status.
     * @return <CODE>void</CODE> - None.
     */
    public boolean checkDuplicateAndsendResponseFromLog(String hl7Msg) throws Exception {
        DBConnection dbcon = null;
        boolean found = false;
		ResultSet rs = null;

        try {
            String messageId = digestMessage(hl7Msg);

            HL7MessageLogDBO hl7MsgLogDbo = this.mDBObjectFactory.createHL7MessageLogDBO(messageId, applicationId);

            dbcon = getDBConnection();
            hl7MsgLogDbo.setOldStatus(STATUS_HL7_ACK_SENT);

            rs = dbcon.getRowWithStatus(hl7MsgLogDbo);

            while (rs.next()) {
                hl7MsgLogDbo.populateDBO(rs);
                if (messageId.equals(hl7MsgLogDbo.getMessageId())
                        && applicationId.equals(hl7MsgLogDbo.getApplicationId())
                        && hl7Msg.equals(hl7MsgLogDbo.getRequestMessage())) {
                    response = hl7MsgLogDbo.getResponseMessage(); // It is a duplicate message
                    this.setResponseMessage(response);
                    found = true;
                    break;
                }
            }
        } catch (Exception ex) {
			if(rs != null){
				try{
				 rs.close();
				} catch (SQLException e) {
                logger.log(Level.SEVERE,
                        I18n.msg("E9335: Exception occurred while closing the result set"));
				}
			}
            try {
                dbcon.getUnderlyingConnection().rollback();
            } catch (SQLException e) {
                logger.log(Level.SEVERE,
                        I18n.msg("E0335: Exception occurred while rollback checkDuplicateAndsendResponseFromLog"));
            }
            logger.log(Level.SEVERE,
                    I18n.msg("E0336: Exception occurred while checking duplicate message from HL7MessageLog "), ex);
            throw ex;
        } finally {
			if(rs != null){
				try{
				 rs.close();
				} catch (SQLException e) {
                logger.log(Level.SEVERE,
                        I18n.msg("E9335: Exception occurred while closing the result set"));
				}
			}
            if (dbcon != null) {
                try {
                    dbcon.close();
                } catch (SQLException ex) {
                    logger.log(Level.SEVERE, I18n.msg("E0304: SQLException occurred while closing the connection :"),
                            ex);
                }
            }
        }
        return found;
    }

    /**
     * Returns the DBConnection
     * 
     * @return
     * @throws SQLException
     * @throws Exception
     */
    private DBConnection getDBConnection() throws SQLException, Exception {
        return mDBConnectionFactory.createConnection();
    }

    public void setApplicationId(String applicationId) {
        this.applicationId = applicationId;
    }

    public void setResponseMessage(String response) {
        this.response = response;
    }

    public String getResponseMessage() {
        return this.response;
    }

}
