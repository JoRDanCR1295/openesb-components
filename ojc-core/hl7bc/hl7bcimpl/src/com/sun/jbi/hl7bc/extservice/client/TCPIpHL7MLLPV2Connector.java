package com.sun.jbi.hl7bc.extservice.client;

import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.hl7bc.ApplicationException;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.I18n;

public class TCPIpHL7MLLPV2Connector extends TCPIpHL7Connector {

    private static final Logger log = Logger.getLogger(TCPIpHL7MLLPV2Connector.class.getName());
	
    private int mllpv2RetryCount = 0;
    private long mllpv2RetryInterval = 0;
    private long mllpv2TimeToWaitForAckNak = 0;
    @Override
	public void connect(ProtocolInfo protocolInfo) throws Exception {
		super.connect(protocolInfo);
		this.mllpv2RetryCount = Integer.parseInt(protocolInfo.get(HL7ProtocolProperties.MLLPV2_RETRIES_COUNT_ON_NAK));
		this.mllpv2RetryInterval = Long.parseLong(protocolInfo.get(HL7ProtocolProperties.MLLPV2_RETRY_INTERVAL));
		this.mllpv2TimeToWaitForAckNak = Long.parseLong(protocolInfo.get(HL7ProtocolProperties.MLLPV2_TIME_TO_WAIT_FOR_ACK_NAK));
	}
    @Override
	public void connect(ProtocolInfo protocolInfo, Endpoint endpoint) throws Exception {
		super.connect(protocolInfo, endpoint);
		this.mllpv2RetryCount = Integer.parseInt(protocolInfo.get(HL7ProtocolProperties.MLLPV2_RETRIES_COUNT_ON_NAK));
		this.mllpv2RetryInterval = Long.parseLong(protocolInfo.get(HL7ProtocolProperties.MLLPV2_RETRY_INTERVAL));
		this.mllpv2TimeToWaitForAckNak = Long.parseLong(protocolInfo.get(HL7ProtocolProperties.MLLPV2_TIME_TO_WAIT_FOR_ACK_NAK));
	}

	public void sendHL7Message(String hl7Msg) throws Exception {
    	int count = 0;
    	for(count=0; count < mllpv2RetryCount + 1 ; count++){
    		if(count == 0)
    			log.fine(I18n.msg("TCPIpHL7MLLPV2Connector: Sending HL7 Message") );
    		else
    			log.info(I18n.msg("I0156: Retrying to send. Attempt no : {0}",count));
	        mSession.write(hl7Msg);
	        String mllpv2AckNak = null;
	        if(mllpv2TimeToWaitForAckNak > 0){
	        	mllpv2AckNak = receiveMLLPV2AckNak(mllpv2TimeToWaitForAckNak);
	        }else{
	        	mllpv2AckNak = receiveMLLPV2AckNak();
	        }
	        byte[] receivedAckNak = mllpv2AckNak.getBytes();
	        //if mllpv2nak is received, retry again.
	        if(receivedAckNak[0] == ((byte) 0x06)){ //ack is received...break the loop.
	        	log.fine(I18n.msg("MLLPV2 Exchange Test - Client: MLLPV2 ACK received"));
	        	log.fine(I18n.msg("TCPIpHL7MLLPV2Connector: MLLPV2 commit ACK received"));
	        	break;
	        }else if(receivedAckNak[0] == ((byte) 0x15)){ //nak is received..retry again.
	        	log.fine(I18n.msg("MLLPV2 Exchange Test - Client: MLLPV2 NAK received. Will retry"));
	        	log.info(I18n.msg("I0157: MLLPV2 negative ACK received. Will retry after {0} milliseconds.",mllpv2RetryInterval));
	        	Thread.sleep(mllpv2RetryInterval);
	        	continue;
	        }else{
	        	log.fine(I18n.msg("MLLPV2 Exchange Test - Client: MLLPV2 ACK/NAK expected. but received other data"));
	        	//MLLPV2 ack/nak expected. But received other data.
	        	throw new ApplicationException(I18n.msg("E0272: MLLP V2 commit acknowledgement or negative acknowledgement expected"));
	        }
    	}
    	if(mllpv2RetryCount != 0 && count == mllpv2RetryCount){
    		throw new ApplicationException(I18n.msg("E0270: Maximum number of MLLPV2 retry count reached for sending message."));
    	}

    }

    public String recvHL7Message() throws Exception {
    	log.fine(I18n.msg("TCPIpHL7MLLPV2Connector: receiveHL7Message..invoked"));
		lock.lock();
		 try {
			 while (getHl7ResponseMsg() == null) 
				ackNotRecvd.await();
		 } catch (Exception ex) {
            log.log(Level.SEVERE, I18n.msg("E0269: An exception occured in HL7 Connector"), ex);
            String errMsg = I18n.msg("E0269: An exception occured in HL7 Connector");
            throw new Exception(errMsg, ex);
        } finally {
		   lock.unlock();
		 }
        String response = getHl7ResponseMsg();
        setHl7ResponseMsg(null); //reset

        storeResponseAndsendMLLPV2AckNak(response);
        log.fine(I18n.msg("TCPIpHL7MLLPV2Connector: returning the received HL7 message : {0}", response));
        return response;
    }

	 public String recvHL7Message(long timeToWait) throws Exception {
		 log.fine(I18n.msg("TCPIpHL7MLLPV2Connector:receiveHL7Message(timeToWait)..invoked"));
		lock.lock();
        boolean timeOut = false;
		 try {
			 while (getHl7ResponseMsg() == null) {
				timeOut = ackNotRecvd.await(timeToWait, TimeUnit.SECONDS);
                if(!timeOut){
                    break;
                }
             }
		 } catch (Exception ex) {
            log.log(Level.SEVERE, I18n.msg("E0269: An exception occured in HL7 Connector"), ex);
            String errMsg = I18n.msg("E0269: An exception occured in HL7 Connector");
            throw new Exception(errMsg, ex);
        } finally {
		   lock.unlock();
		 }
        String response = getHl7ResponseMsg();
        setHl7ResponseMsg(null); //reset
        storeResponseAndsendMLLPV2AckNak(response);
        log.fine(I18n.msg("TCPIpHL7MLLPV2Connector: recvHL7Message().returning the received HL7 message : {0} ", response));
        return response;
    }
	 
	 private String receiveMLLPV2AckNak() throws Exception{
		 log.fine(I18n.msg("TCPIpHL7MLLPV2Connector:receiveMLLPV2AckNak..invoked"));
			lock.lock();
			 try {
				 while (getHl7ResponseMsg() == null) {
					ackNotRecvd.await();
				 }
			 } catch (Exception ex) {
	            log.log(Level.SEVERE, I18n.msg("E0269: An exception occured in HL7 Connector"), ex);
	            String errMsg = I18n.msg("E0269: An exception occured in HL7 Connector");
	            throw new Exception(errMsg, ex);
	        } finally {
			   lock.unlock();
			 }
	        String response = getHl7ResponseMsg();
	        setHl7ResponseMsg(null); //reset
	        
	        log.fine("TCPIpHL7MLLPV2Connector: receiveMLLPV2AckNak().returning MLLPV2 ACK/NAK:" + response);
	        return response;
	 }	 
	 private String receiveMLLPV2AckNak(long timeToWait) throws Exception{
		 log.fine(I18n.msg("TCPIpHL7MLLPV2Connector:receiveMLLPV2AckNak..invoked"));
			lock.lock();
			boolean timeOut = false;
			 try {
				 while (getHl7ResponseMsg() == null) {
					timeOut = ackNotRecvd.await(timeToWait,TimeUnit.MILLISECONDS);
					if(!timeOut)
						throw new ApplicationException(I18n.msg("E0271: Timeout occurred while trying to receive MLLP V2 commit ack / nak."));
				 }
			 } catch (Exception ex) {
	            log.log(Level.SEVERE, I18n.msg("E0269: An exception occured in HL7 Connector"), ex);
	            String errMsg = I18n.msg("E0269: An exception occured in HL7 Connector");
	            throw new Exception(errMsg, ex);
	        } finally {
			   lock.unlock();
			 }
	        String response = getHl7ResponseMsg();
	        setHl7ResponseMsg(null); //reset
	        
	        log.fine(I18n.msg("TCPIpHL7MLLPV2Connector: receiveMLLPV2AckNak().returning MLLPV2 ACK/NAK : {0}", response));
	        return response;
	 }

	 private void storeResponseAndsendMLLPV2AckNak(String hl7Ack) throws Exception {
		 boolean isPersisted = true; //Currently, not storing the HL7 ACK messages in outbound mode.
		 boolean isAck = isPersisted;
		 log.fine(I18n.msg("TCPIpHL7MLLPV2Connector: storeResponseAndsendMLLPV2AckNak: sending MLLPV2 ACK."));
		 if(isAck){
			 mSession.write(new Byte((byte) 0x06));
		 }else{
			 mSession.write(new Byte((byte) 0x15));
		 }
			 
	 }
}
