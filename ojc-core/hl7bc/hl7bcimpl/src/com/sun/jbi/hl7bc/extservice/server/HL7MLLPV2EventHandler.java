package com.sun.jbi.hl7bc.extservice.server;

import java.security.MessageDigest;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.mina.common.CloseFuture;
import org.apache.mina.common.IoSession;

import sun.misc.BASE64Encoder;

import com.sun.jbi.hl7bc.ApplicationException;
import com.sun.jbi.hl7bc.configuration.RuntimeConfiguration;
import com.sun.jbi.hl7bc.extservice.persist.MLLPV2PersistenceHandler;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnection;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObjectFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.HL7MessageLogDBO;
import com.sun.jbi.hl7bc.extservice.server.HL7EventHandler.DefaultHL7Callback;
import com.sun.jbi.hl7bc.I18n;

public class HL7MLLPV2EventHandler extends HL7EventHandler {

    private static final Logger mLog = Logger.getLogger(HL7MLLPV2EventHandler.class.getName());

	
    private int mllpv2RetryCount = 0;
    private long mllpv2RetryInterval = 0;
    
    //These variables are used per message exchange. Once the message exchange is done, these variables are reset to initial values
    private boolean expectAckNak = false;    
    private String hl7Request = null;    
    private String hl7Response = null;
    private int count = 0;
    
   
    private MLLPV2PersistenceHandler persistenceHandler = null;
    public HL7MLLPV2EventHandler(int mllpv2RetriesCount,long mllpv2RetryInterval, 
    		MLLPV2PersistenceHandler persistenceHandler){
    	super();
    	this.mllpv2RetryCount = mllpv2RetriesCount;
    	this.mllpv2RetryInterval = mllpv2RetryInterval;
    	this.persistenceHandler = persistenceHandler;
   	
    }

    public synchronized void messageReceived( IoSession session, Object message ) {
    	
        String hl7Msg = (String)message;
        
    	if(expectAckNak && hl7Msg.length() == 1){
    		
    		if(hl7Msg.getBytes()[0] == (byte) 0x06){ 
    			//MLLPV2 commit ack is received.
    			resetMessageExchange();
    		}else if(hl7Msg.getBytes()[0] == (byte) 0x15){
    			//MLLPV2 negative ack is received
    			if(count < mllpv2RetryCount){
    				mLog.info(I18n.msg("I0161: MLLPV2 nak received. Will retry after {0} milliseconds.",mllpv2RetryInterval));
    				
    				try {
						Thread.sleep(mllpv2RetryInterval);
					} catch (InterruptedException e) {
						//do nothing
					}
					//Write the HL7 ACK here
    				session.write(hl7Response);
    				expectAckNak = true;
    				count++;
    				
    			}else {
    				resetMessageExchange();
    				mLog.log(Level.SEVERE, I18n.msg("E0275: Maximum number of MLLP V2 retry count reached."));
    			}
    		}
    		
    	}else if(!expectAckNak && hl7Msg.length() == 1){ //else if("false".equalsIgnoreCase((String)session.getAttribute(EXPECT_ACK_NAK)) && hl7Msg.length() == 1){
			mLog.log(Level.SEVERE, I18n.msg("E0276: MLLP V2 protocol commit ack/nak received but not expected.")); //MLLP V2 protocol commit ack/nak received but not expected.");
			resetMessageExchange();
    	}else if(expectAckNak && hl7Msg.length() > 1){
    		mLog.log(Level.SEVERE, I18n.msg("E0277: MLLPV2 protococl commit ack/nak expected")); //MLLPV2 protococl commit ack/nak expected
    		resetMessageExchange();
    	}
    	else{
            try {
                doWork(hl7Msg, session);
            } catch (Exception ex) {
                mLog.log(Level.SEVERE, I18n.msg("E0273: An Exception occured while receiving the message from the socket"), ex);
            }
    	}
    }

	private void resetMessageExchange() {
		count = 0;
		expectAckNak = false;
		hl7Request = null;
		hl7Response = null;
	}
	
    public void doWork(String content, IoSession session) throws Exception {
    	this.hl7Request = content;
    	HL7MLLPV2Callback callback = new HL7MLLPV2Callback(session);
        boolean isPersisted = true;

        // check duplicate message
        if (content == null)
            return;
        boolean found = this.persistenceHandler.checkDuplicateAndsendResponseFromLog(content);
        // Duplicate messsage found. Now send the commit ack followed by HL7 response
        if (found && this.persistenceHandler.getResponseMessage() != null) {
            boolean ack = found;
            mLog.fine(I18n.msg("MLLPV2 Exchange Test: Sending MLLPV2 : {0}", (ack ? "Ack" : "Nak")));
            sendMLLPv2TransportLayerACKNAK(callback, found); // Sending the Commit ACK
            sendPersistedACK(callback, this.persistenceHandler.getResponseMessage()); // Sending the HL7
            // Response message from
            // HL7MESSAGELOG. It can
            // be HL7 ACK .
            return;
        }
        // Persist the message in MLLPV2 database
        isPersisted = this.persistenceHandler.storeMessageInHL7MessageLog(content);
        boolean isAck = isPersisted;
        mLog.fine(I18n.msg("MLLPV2 Exchange Test: Sending MLLPV2 : {0}", (isAck ? "Ack" : "Nak") ));
        sendMLLPv2TransportLayerACKNAK(callback, isPersisted);
        this.persistenceHandler.updateHL7MessageLogStatus(content,isAck);
        // Onlly if persisted ..onMessage is invoked, if not an MLLPV2 nak is sent so that the
        // sender resends the message
        if(isPersisted){
        	//getHL7Listener().onMessage(content, callback);
        	HL7Listener hl7Listener = findHL7Listener(content);
            hl7Listener = hl7Listener == null ? (HL7Listener) getHL7Listeners().get(HL7_GENERIC_MESSAGETYPE)
                    : hl7Listener;
        	if(hl7Listener != null){				
        		hl7Listener.onMessage(content, callback);
    		}else{
                String msg = I18n.msg(
                        "E0274: No HL7Listener created for the received message type. Available hl7listeners are : {0}, provide valid message type i.e MSH.9 field value on the operation.",
                        getHL7Listeners().keySet());
                mLog.log(Level.SEVERE, msg);
                throw new Exception(msg);
    		}
            mLog.fine(I18n.msg("MLLPV2 Exchange Test: onMessage completed" ));
        }
    }
    
	private void sendMLLPv2TransportLayerACKNAK(HL7MLLPV2Callback callback, boolean persisted) {
		if (persisted) {
			callback.sendMLLPV2AckNak(new Byte((byte) 0x06)); //mllpv2 ack
		} else {
			callback.sendMLLPV2AckNak(new Byte((byte) 0x15)); //mllpv2 nak
		}
	}
    
    private void sendPersistedACK(HL7MLLPV2Callback callback, String response) {          
        callback.sendPersistedACK(response); //       
	}
	
	class HL7MLLPV2Callback implements HL7Callback {

        private IoSession mSession;

        HL7MLLPV2Callback(IoSession session) {
            mSession = session;
        }

        public void onReply(String hl7MsgAck,boolean isAck) throws ApplicationException, Exception {
        	synchronized (HL7MLLPV2EventHandler.this) {
        		mLog.fine(I18n.msg("MLLPV2 Exchange Test: Reply invoked. Send hl7 ACK") );
                mSession.write(hl7MsgAck);
                expectAckNak = true;
                persistenceHandler.updateHL7AckNak(hl7Request,hl7MsgAck,isAck);
                hl7Response = hl7MsgAck;
                count = 0;
			}
            
        }
        
        public void sendMLLPV2AckNak(Byte ackNak){
        	
        	mSession.write(ackNak);
        }

		
        
        public void sendPersistedACK(String ackMsg){        	
        	mSession.write(ackMsg);
			if(mSession.isConnected()) {
               CloseFuture future =  mSession.close();
            }
        }
        
        public void increaseNakSentCount() {
            long counter = (Long) mSession.getAttribute(NAK_SENT_COUNTER);
            mSession.setAttribute(NAK_SENT_COUNTER, ++counter);
        }

        public long getNakSentCount() {
            return (Long) mSession.getAttribute(NAK_SENT_COUNTER);
        }

        public void increaseCannedNakSentCount() {
            long counter = (Long) mSession.getAttribute(CANNED_NAK_SENT_COUNTER);
            mSession.setAttribute(CANNED_NAK_SENT_COUNTER, ++counter);
        }

        public long getCannedNakSentCount() {
            return (Long) mSession.getAttribute(CANNED_NAK_SENT_COUNTER);
        }
        
        public void closeConnection() {
            if(mSession.isConnected()) {
               CloseFuture future =  mSession.close();
            }
        }
        
        public String getClientInfo() {
            return mSession.getRemoteAddress().toString();
        }
        
    }


}
