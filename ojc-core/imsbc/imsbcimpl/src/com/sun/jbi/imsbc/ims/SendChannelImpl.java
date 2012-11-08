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

package com.sun.jbi.imsbc.ims;

import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Vector;

import javax.jbi.component.ComponentContext;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.imsbc.Endpoint;
import com.sun.jbi.imsbc.extensions.IMSOperation;
import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSOutput;
import com.sun.jbi.imsbc.extensions.IMSMessage;
import com.sun.jbi.imsbc.IMSException;
import com.sun.jbi.imsbc.util.AlertsUtil;
import com.sun.jbi.imsbc.IMSBindingComponent;


/**
 * Class representing exact channel for sending message to IMS
 * 
 * 
 * @author Sun Microsystems
 */
public class SendChannelImpl implements Channel {

    private static final Messages mMessages = Messages.getMessages(SendChannelImpl.class);

    private static final Logger mLogger = Messages.getLogger(SendChannelImpl.class);

    private ComponentContext mContext;

    private Endpoint mEndpoint;

    private IMSOperation imsOperation;

    private IMSInput imsInput;

    private IMSOutput imsOutput;

    private IMSMessage imsMessage;
   
    private IMSConfigs imsConfigs;
    
    private IMSClient imsClient;
    
    private String imsServer = null;
    
    private Integer imsPort;
    
    protected int sendCount = 0;
    
    protected long waitTimeout;

    protected String generatedClientID = null;
    
    protected String message = null;
    protected String replyMessage = null;
    protected String TranCodeSrc;
    protected char bandrs;
    protected final static long ACQUIRE_LOCK_TIMEOUT = 1000 * 10; 
    protected static final String EIGHT_SPACES = "        ";
    

    /**
     * Creates a new instance of Channel
     */
    public SendChannelImpl(Endpoint endpoint, IMSOperation operation, ComponentContext context) throws IMSException {
        mEndpoint = endpoint;
        imsOperation = operation;
        mContext = context;
        imsInput = imsOperation.getIMSOperationInput();
        imsMessage = imsInput.getImsMessage();
         
        imsServer = endpoint.getIMSAddress().getServerName();
        imsPort = endpoint.getIMSAddress().getServerPort();
        imsConfigs = new IMSConfigs(imsMessage);
        imsClient = new IMSClient(
        		imsConfigs.datastoreID,
        		imsConfigs.LtermName,
        		imsConfigs.exitID,
        		imsConfigs.clientID,
        		imsConfigs.RacfUserID,
        		imsConfigs.RacfGroupName,
        		imsConfigs.Password,
        		imsConfigs.syncLevel,
        		imsConfigs.commitMode,
        		imsConfigs.prefixLength,
        		imsConfigs.flowCtl,
        		imsConfigs.rcvDelay,
        		imsConfigs.soctType,
        		imsConfigs.mfsMod,
        		imsConfigs.irmHeaderEncoding,
        		imsConfigs.sendDataEncoding,
        		imsConfigs.replyDataEncoding);
        
    }

    /**
     * Establishes the physical connection to the underlying system.
     *
     * @throws application specific IMSException upon error.
     */
    public void connect() throws IMSException {
        try {
            if (isConnected()) {
                disconnect();
            }
            imsClient.open(imsServer, imsPort);
        }
        catch (Exception e) {
        	String errMsg = mMessages.getString("IMSBC-E00837.Connection_Failure", new Object[] { imsServer, imsPort, e.toString()});
			mLogger.log(Level.SEVERE, errMsg);
			AlertsUtil.getAlerter().critical(errMsg, 
									IMSBindingComponent.SHORT_DISPLAY_NAME, 
									null, 
									AlertsUtil.getServerType(),
									AlertsUtil.COMPONENT_TYPE_BINDING,
									NotificationEvent.OPERATIONAL_STATE_RUNNING, 
									NotificationEvent.EVENT_TYPE_ALERT,
									"IMSBC-E00837");
            throw new IMSException(errMsg);
        }
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00838.Connection_Success", new Object[] {imsServer, imsPort}));
    }

    public String send(String inputMessage) throws IMSException {

        if (!isConnected()) {
			if (mLogger.isLoggable(Level.INFO))
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00839.Send_Connect", new Object[] { imsServer }));
            connect();
        }
        if (isConnected()) {
            boolean lockAcquired = false;  // for serial mode (static client ID)
            boolean errorOccurred = false; // flag to indicate send/receive error

            // Generate an unused client ID if necessary
            String clientID = imsConfigs.clientID;
            if (generateClientID()) {
                try {
                    // Don't generate client ID if using SYNC_LEVEL_CONFIRM and
                    // acknowledgement has not been sent yet.
                    if (imsConfigs.syncLevel == 0x01) {
                        if (sendCount == 1) { // send ack next
                            clientID = generatedClientID;
							if (mLogger.isLoggable(Level.INFO))
								mLogger.log(Level.INFO, mMessages.getString("IMCBC-R00840.Send_Ack_ClientId", new Object[] {clientID}));
                        } else { // send request
                            clientID = IMSClientIDManager.generateNextClientID(imsConfigs.clientID);
                            generatedClientID = clientID;
							if (mLogger.isLoggable(Level.INFO))
								mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00841.Send_Request_ClientId", new Object[] {clientID}));
                        }
                    } else { // SYNC_LEVEL_NONE - gen ID on every send
                        clientID = IMSClientIDManager.generateNextClientID(imsConfigs.clientID);
						if (mLogger.isLoggable(Level.INFO))
							mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00841.Send_Request_ClientId", new Object[] {clientID}));
                    }
                } catch (Throwable t) {
                	String errMsg = mMessages.getString("IMSBC-E00842.Generate_ClientId_Failed", new Object[] { t.toString()});
					mLogger.log(Level.SEVERE, errMsg);
					AlertsUtil.getAlerter().critical(errMsg, 
											IMSBindingComponent.SHORT_DISPLAY_NAME, 
											null, 
											AlertsUtil.getServerType(),
											AlertsUtil.COMPONENT_TYPE_BINDING,
											NotificationEvent.OPERATIONAL_STATE_RUNNING, 
											NotificationEvent.EVENT_TYPE_ALERT,
											"IMSBC-E00842");
                    throw new IMSException (errMsg);
                }
                this.imsClient.setClientID(clientID);
            } else {  // Using static client id - need to serialize requests
				if (mLogger.isLoggable(Level.INFO))
            		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00843.Using_Client_Id", new Object[] {clientID}));
                try {
                    if (imsConfigs.syncLevel == 0x01) { // SYNC_LEVEL_CONFIRM
                        if (sendCount == 1) {
                            lockAcquired = true;
							if (mLogger.isLoggable(Level.INFO))
								mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00844.Send_Ack"));
                        } else {
							if (mLogger.isLoggable(Level.INFO))
                        		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00845.Acquire_Lock"));
                            // First obtain lock before proceeding
                            lockAcquired = IMSClientRequestController.acquireLock(clientID, waitTimeout);
                        }
                    } else {  // SYNC_LEVEL_NONE
						if (mLogger.isLoggable(Level.INFO))
                    		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00845.Acquire_Lock"));
                        // First obtain lock before proceeding
                        lockAcquired = IMSClientRequestController.acquireLock(clientID, waitTimeout);
                    }

                    if (lockAcquired) {
                        if (imsConfigs.syncLevel == 0x01) {  // SYNC_LEVEL_CONFIRM
                            if (sendCount != 1) {
                                // open connection only after acknowledgement is sent
                                connect();
                            }
                        } else { // SYNC_LEVEL_NONE - connect and disconnect per send
                            // Next open connection (if using same client ID, always open
                            // and close the connection per request
                            connect();
                        }
                    } else {
                    	String errMsg = mMessages.getString("IMSBC-E00846.Acquire_Lock_Failed", new Object[] { clientID, waitTimeout});
						if (mLogger.isLoggable(Level.INFO))
                    		mLogger.log(Level.INFO, errMsg);
                        throw new IMSException(errMsg);
                    }
                } catch (Exception ex) {
                	String errMsg = mMessages.getString("IMSBC-E00847.Invoke_Ims_Failed", new Object[] { ex.toString()});
                	mLogger.log(Level.SEVERE, errMsg);
					AlertsUtil.getAlerter().critical(errMsg, 
											IMSBindingComponent.SHORT_DISPLAY_NAME, 
											null, 
											AlertsUtil.getServerType(),
											AlertsUtil.COMPONENT_TYPE_BINDING,
											NotificationEvent.OPERATIONAL_STATE_RUNNING, 
											NotificationEvent.EVENT_TYPE_ALERT,
											"IMSBC-E00847");
                    // Release lock incase current thread had acquired it
                    if (lockAcquired) {
                        try {
                            IMSClientRequestController.releaseLock(clientID);
                            lockAcquired = false;
                        }
                        catch (Exception ex2) {
                        	mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00848.Release_Lock_Failed", new Object[] {clientID}));
                        }
                    }
                    
                    throw new IMSException (errMsg);
                }
            }

            String inTranCode = null;
            String inMessage = null;
            Vector replyVector;
            Vector byteVector;

            try {
                /* Test to see if configured to have TranCode come with message,
                 * or to take it from cfg file. */
                if ("MESSAGE".equals(imsConfigs.TranCodeSrc)) {
                    if (inputMessage.length() <= 8) {
                        inTranCode = inputMessage;
                        inMessage = "";
                    }
                    else {
                        inTranCode = inputMessage.substring(0, 8);
                        inMessage = inputMessage.substring(8);
                    }
                }
                else {
                    if (inputMessage.length() == 0) {
                        inMessage = "";
                    }
                    else {
                        inMessage = inputMessage;
                    }
                    inTranCode = imsConfigs.TranCode;
                }

                boolean isAckSend = sendCount == 1;
                replyVector = imsClient.send(inTranCode, inMessage,
                                                 imsConfigs.bandrs,
                                                 isAckSend);
                replyMessage = replyVector.toString();
               // byteVector = myExtDelegate.byteVector;

                // If confirm, increment send count, reset send count if necessary
                if (imsConfigs.syncLevel == 0x01) {
                    sendCount++;
                }
            }
            catch (Exception e) {
                errorOccurred = true;
                String errMsg = mMessages.getString("IMSBC-E00849.Send_Msg_Failed", new Object[] { e.toString()});
                mLogger.log(Level.INFO, errMsg);
				AlertsUtil.getAlerter().critical(errMsg, 
										IMSBindingComponent.SHORT_DISPLAY_NAME, 
										null, 
										AlertsUtil.getServerType(),
										AlertsUtil.COMPONENT_TYPE_BINDING,
										NotificationEvent.OPERATIONAL_STATE_RUNNING, 
										NotificationEvent.EVENT_TYPE_ALERT,
										"IMSBC-E00849");
                throw new IMSException(errMsg);
            } finally {
                if (generateClientID()) { // generated client ID
                    if (errorOccurred || imsConfigs.syncLevel == 0x00) {
                        // If SYNC_LEVEL_NONE or error occurred, release generated ID
                        IMSClientIDManager.collectClientID(clientID);
                        if (errorOccurred) {
							if (mLogger.isLoggable(Level.INFO))
                        		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00850.Release_Client_Id", new Object[] {clientID}));
                        } else {
							if (mLogger.isLoggable(Level.INFO))
                        		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00850.Release_Client_Id", new Object[] {clientID}));
                        }
                    } else { // SYNC_LEVEL_CONFIRM
                        if (sendCount == 2) { // collect if request and ack already sent
                            IMSClientIDManager.collectClientID(clientID);
							if (mLogger.isLoggable(Level.INFO))
								mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00851.Sent_Ack_Release_Lock", new Object[] {clientID}));
                        } else { // reuse client ID for sending acknowledgement
							if (mLogger.isLoggable(Level.INFO))
                        		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00852.Not_Released_Lock", new Object[] {clientID}));
                        }
                    }
                } else { // static client ID, release lock
                    if (errorOccurred || imsConfigs.syncLevel == 0x00) {
                        // If SYNC_LEVEL_NONE or error occurred, release lock on static ID
                        if (errorOccurred) {
							if (mLogger.isLoggable(Level.INFO))
                        		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00853.Disconnect_Release_Lock", new Object[] {clientID}));
                        } else {
							if (mLogger.isLoggable(Level.INFO))
                        		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00854.Sent_Req_Release_Lock", new Object[] {clientID}));
                        }

                        // If using static client id, force a socket close
                        // to avoid duplicate client id
                        try {
                            // close connection first
                            try {
                                disconnect();
                            }
                            catch (Throwable t) {
                            	mLogger.log(Level.SEVERE,  mMessages.getString("IMSBC-E00855.Connection_Close_Failed"));
                            }

                            // Release the acquired lock on client ID
                            if (lockAcquired) {
                                IMSClientRequestController.releaseLock(clientID);
                                lockAcquired = false;
                            }
                        }
                        catch (Exception ex) {
                        	String errMsg = mMessages.getString("IMSBC-E00856.Cleanup_Failed", new Object[] { ex.toString()});
                        	mLogger.log(Level.INFO, errMsg);
                            throw new IMSException(errMsg);
                        }
                    } else { // SYNC_LEVEL_CONFIRM
                        if (sendCount == 2) {
							if (mLogger.isLoggable(Level.INFO))
                        		mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00857.Release_Lock", new Object[] {clientID}));
                            // If using static client id, force a socket close
                            // to avoid duplicate client id
                            try {
                                // close connection first
                                try {
                                    disconnect();
                                }
                                catch (Throwable t) {
									String errMsg = mMessages.getString("IMSBC-E00855.Connection_Close_Failed", t);
									mLogger.log(Level.SEVERE, errMsg);
									AlertsUtil.getAlerter().critical(errMsg, 
															IMSBindingComponent.SHORT_DISPLAY_NAME, 
															null, 
															AlertsUtil.getServerType(),
															AlertsUtil.COMPONENT_TYPE_BINDING,
															NotificationEvent.OPERATIONAL_STATE_RUNNING, 
															NotificationEvent.EVENT_TYPE_ALERT,
															"IMSBC-E00855");
                                }

                                // Release the acquired lock on client ID
                                if (lockAcquired) {
                                    IMSClientRequestController.releaseLock(clientID);
                                    lockAcquired = false;
                                }
                            }
                            catch (Exception ex) {
                            	String errMsg = mMessages.getString("IMSBC-E00856.Cleanup_Failed", new Object[] { ex.toString()});
                            	mLogger.log(Level.INFO, errMsg);
                                throw new IMSException(errMsg);
                            }
                        }
                    }
                }

                // Reset sendCount if already sent request and ack in SYNC_LEVEL_CONFIRM mode
                if (imsConfigs.syncLevel == 0x01) {
                    if (errorOccurred || sendCount == 2) { // reset sendCount if error or sent request and ack
                        sendCount = 0;
                    }
                }
            }
        }
        return replyMessage;
    }

    
    /**
     *  Check to see if connected to server.
     */
    public boolean isConnected() throws IMSException {
        if (imsClient != null) {
            return imsClient.isOpen();
        }
        else {
        	String errMsg = mMessages.getString("IMSBC-E00858.Ims_Instance_Failed");
            throw new IMSException(errMsg);
        }
    }

    /**
     *  Disconnect from server.
     */
    public void disconnect() throws IMSException {

        if (imsClient != null) {
        	imsClient.close();
        } else {
        	String errMsg = mMessages.getString("IMSBC-E00858.Ims_Instance_Failed");
            throw new IMSException(errMsg);
        }
    }


    public String getReturnErrMsg() {
        return imsClient.returnCodeTxt;
    }

    public String getReasonErrMsg() {
        return imsClient.reasonTxt;
    }

    private boolean generateClientID() {
        return (imsConfigs.clientID != null) &&
               (imsConfigs.clientID.indexOf("*") != -1);
    }
        

    public Endpoint getEndpoint() {
        return mEndpoint;
    }

    public IMSOperation getIMSOperation() {
        return imsOperation;
    }

    public IMSMessage getIMSMessage() {
        return imsMessage;
    }
    
    public IMSInput getIMSOperationInput() {
        return imsInput;
    }
    
    public IMSOutput getIMSOperationOutput() {
        return imsOutput;
    }

    private void forceClose() throws IMSException {
        try {
            imsClient.close();
        } catch (Exception e) {
            throw new IMSException(e);
        }
    }
    
    public String receive(){
        return "todo";
    }
}
