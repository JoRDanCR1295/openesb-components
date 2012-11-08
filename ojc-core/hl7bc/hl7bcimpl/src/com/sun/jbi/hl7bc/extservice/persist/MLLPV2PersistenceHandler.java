package com.sun.jbi.hl7bc.extservice.persist;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnection;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObjectFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.HL7MessageLogDBO;
import com.sun.jbi.hl7bc.util.Base64Util;
import com.sun.jbi.hl7bc.I18n;


public class MLLPV2PersistenceHandler {

	private static final Logger logger = Logger.getLogger(MLLPV2PersistenceHandler.class.getName());

	
    public static final int STATUS_NEW_MSG_RECEIVED = 1;
    public static final int STATUS_COMMIT_ACK_SENT = 2;
    public static final int STATUS_HL7_ACK_SENT = 3;
    public static final int STATUS_HL7_NAK_SENT = 4;

	private DBObjectFactory mDBObjectFactory;
	private String applicationId;
	private DBConnectionFactory mDBConnectionFactory;
	private String response;
	
	
	public MLLPV2PersistenceHandler(DBConnectionFactory dbConnectionFactory, DBObjectFactory dbObjectFactory){
		this.mDBConnectionFactory = dbConnectionFactory;
		this.mDBObjectFactory = dbObjectFactory;
	}

	public boolean storeMessageInHL7MessageLog(String message) throws Exception{
	    DBConnection dbCon = null; 
	    try { 
		    	dbCon = getDBConnection(); 
		    	HL7MessageLogDBO hl7LogDBO =  mDBObjectFactory.createHL7MessageLogDBO();
		    	
		    	String digestedContent = digestMessage(message); 
		    	
		    	hl7LogDBO.setMessageId(digestedContent);
		    	hl7LogDBO.setApplicationId(applicationId);
		    	hl7LogDBO.setRequestMessage(message);
		    	hl7LogDBO.setResponseMessage(null);
		    	hl7LogDBO.setCreatedTime(new Timestamp(System.currentTimeMillis()));
		    	hl7LogDBO.setLastUpdatedTime(new Timestamp(System.currentTimeMillis()));
		    	hl7LogDBO.setStatus(STATUS_NEW_MSG_RECEIVED);
		    	dbCon.insert(hl7LogDBO);
		    	dbCon.getUnderlyingConnection().commit();
    	} catch(Exception ex) {
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
		String digestedContent = new String(new Base64Util().encode(md.digest()));;
		return digestedContent;
	}
	
    /**
     * Method updateHL7MessageLogStatus used only for MLLPV2.
     * Update the status of HL7 message in the dataBase with messageID taken from HL7 Message.
     * @param messageId-The messageID extract from the HL7 Message.
     * @param applicationId-Application ID.
     * @param oldStatus-old Status.
     * @param newStatus-New Status.
     * @return <CODE>void</CODE> - None.
     */
    public void updateHL7MessageLogStatus(String hl7Msg,
        boolean isAck) {
        DBConnection dbcon = null;

        try {

            HL7MessageLogDBO hl7MsgLogDbo = this.mDBObjectFactory.createHL7MessageLogDBO(digestMessage(hl7Msg),
                    applicationId);

            dbcon = getDBConnection();
            hl7MsgLogDbo.setOldStatus(STATUS_NEW_MSG_RECEIVED);

            ResultSet rs = dbcon.getRowWithStatus(hl7MsgLogDbo);

            if (rs.next()) {
                hl7MsgLogDbo.populateDBO(rs);
            }

            hl7MsgLogDbo.setOldStatus(STATUS_NEW_MSG_RECEIVED);
            hl7MsgLogDbo.setStatus(STATUS_COMMIT_ACK_SENT);
            hl7MsgLogDbo.setLastUpdatedTime(new Timestamp(System.currentTimeMillis()));

            dbcon.updateRowWithStatus(hl7MsgLogDbo);
            dbcon.getUnderlyingConnection().commit();
			if(logger.isLoggable(Level.FINE)){
				logger.fine(I18n.msg("Updated the HL7MessageLog status to : {0} ", STATUS_COMMIT_ACK_SENT));
			}
        } catch (Exception ex) {
        	try {
				dbcon.getUnderlyingConnection().rollback();
			} catch (SQLException e) {
				logger.log(Level.SEVERE,
		                I18n.msg("E0302: Exception occurred while rollback updateHL7MessageLogStatus"));
			}
            logger.log(Level.SEVERE,
                I18n.msg("E0303: Exception occurred while updating HL7MessageLog status "),
                ex);
        } finally {
            if (dbcon != null) {
                try {
                    dbcon.close();
                } catch (SQLException ex) {
                    logger.log(Level.SEVERE,
                        I18n.msg("E0304: SQLException occurred while closing the connection :"),
                        ex);
                }
            }
        }
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
    public boolean checkDuplicateAndsendResponseFromLog(String hl7Msg) throws Exception{
        DBConnection dbcon = null;
		boolean found = false;

        try {
            String messageId = digestMessage(hl7Msg);

            HL7MessageLogDBO hl7MsgLogDbo = this.mDBObjectFactory.createHL7MessageLogDBO(messageId, applicationId);

            dbcon = getDBConnection();
            hl7MsgLogDbo.setOldStatus(STATUS_HL7_ACK_SENT);

            ResultSet rs = dbcon.getRowWithStatus(hl7MsgLogDbo);

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
            try {
                dbcon.getUnderlyingConnection().rollback();
            } catch (SQLException e) {
                logger.log(Level.SEVERE, I18n.msg("E0335: Exception occurred while rollback checkDuplicateAndsendResponseFromLog"));
            }
            logger.log(Level.SEVERE, I18n.msg("E0336: Exception occurred while checking duplicate message from HL7MessageLog "), ex);
            throw ex;
        } finally {
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
     * Method storeResponseMessageInLog used only for MLLPV2 to store received Response ACK or NAK.
     * @param messageId-The messageID extract from the HL7 Message.
     * @param responseMessage-The response message.
     * @param isHL7NAK-The response message is NAK or not.
     * @return <CODE>boolean</CODE> - true or false.
     */
    public boolean updateHL7AckNak(String requestMessage,
        String responseMessage, boolean isAck) {
        DBConnection dbcon = null;

        try {

            dbcon = getDBConnection();

            HL7MessageLogDBO hl7MsgLogDbo = this.mDBObjectFactory.createHL7MessageLogDBO(digestMessage(requestMessage),
                    applicationId);
            hl7MsgLogDbo.setOldStatus(STATUS_COMMIT_ACK_SENT);

            ResultSet rs = dbcon.getRowWithStatus(hl7MsgLogDbo);

            if (rs.next()) {
                hl7MsgLogDbo.populateDBO(rs);
            }

            hl7MsgLogDbo.setOldStatus(STATUS_COMMIT_ACK_SENT);

            hl7MsgLogDbo.setResponseMessage(responseMessage);

            if (isAck) {
                hl7MsgLogDbo.setStatus(STATUS_HL7_ACK_SENT); //3: Status = HL7 Response(ACK) sent
            } else {
                hl7MsgLogDbo.setStatus(STATUS_HL7_NAK_SENT); //4: Status = HL7 Response(NAK) sent
            }

            dbcon.updateRowWithStatus(hl7MsgLogDbo);
            dbcon.getUnderlyingConnection().commit();
        } catch (Exception ex) {
        	try {
				dbcon.getUnderlyingConnection().rollback();
			} catch (SQLException e) {
	            logger.log(Level.SEVERE,I18n.msg("E0305: Exception while rollback updateHL7AckNak"));
			}
            logger.log(Level.SEVERE,I18n.msg("E0306: Exception while trying to store response :"), ex);

            return false;
        } finally {
            if (dbcon != null) {
                try {
                    dbcon.close();
                } catch (SQLException ex) {
                	logger.log(Level.SEVERE,
                        I18n.msg("E0304: SQLException occurred while closing the connection :"),
                        ex);
                }
            }
        }

        return true;
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
    
    public void setApplicationId(String applicationId){
    	this.applicationId = applicationId;
    }

	public void setResponseMessage(String response){
		this.response = response;
	}

	public String getResponseMessage(){
		return this.response;
	}
}
