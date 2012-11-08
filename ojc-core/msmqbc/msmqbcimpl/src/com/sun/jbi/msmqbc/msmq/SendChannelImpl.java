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
 * @(#)SendChannelImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.msmq;

import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.msmqbc.Endpoint;
import com.sun.jbi.msmqbc.MSMQPayLoad;
import com.sun.jbi.msmqbc.util.MSMQUtil;
import com.sun.jbi.msmqbc.jni.MSMQAPI;
import com.sun.jbi.msmqbc.jni.MSMQHandle;
import com.sun.jbi.msmqbc.jni.MSMQAccessMode;
import com.sun.jbi.msmqbc.jni.MSMQShareMode;
import com.sun.jbi.msmqbc.jni.MSMQAPIReturnCode;
import com.sun.jbi.msmqbc.jni.MSMQMessageTypes;
import com.sun.jbi.msmqbc.jni.MSMQTxMode;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;
import com.sun.jbi.msmqbc.extensions.MSMQInput;
import com.sun.jbi.msmqbc.extensions.MSMQOutput;
import com.sun.jbi.msmqbc.extensions.MSMQMessage;
import com.sun.jbi.msmqbc.extensions.MSMQConstants;
import com.sun.jbi.msmqbc.exception.MSMQException;
import com.sun.jbi.msmqbc.transaction.MSMQXAResourceImpl;

import javax.jbi.component.ComponentContext;

import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;
import javax.transaction.Transaction;

/**
 * Class representing exact channel for sending message to 
 * microsoft message queue.
 * 
 * @author Sun Microsystems
 */
public class SendChannelImpl extends AbstractChannel {

    private static final Messages mMessages = Messages.getMessages(SendChannelImpl.class);

    private static final Logger mLogger = Messages.getLogger(SendChannelImpl.class);

    private ComponentContext mContext;

    private Endpoint mEndpoint;

    private MSMQOperation msmqOperation;

    private MSMQInput msmqInput;

    private MSMQOutput msmqOutput;

    private MSMQMessage msmqMessage;

    private MSMQAPI msmqAPI;

    private MSMQHandle msmqHandle;

    private MSMQHandle msmqCursor;

    private String mConnMode;

    private String mDestination;

    private String mQueueAlias;

    private String mQueueFormatName;

    private String mQueueName;

    private String mServer;

    private String mAccessMode;

    private String mShareMode;

    private String msgType;

    private String mDistributionList;

    private String mMulticastAddress;

    private String mMultipleElementFormatName;

    private String mMSMQHost;

    private String mTransaction;

    private int msmqShareMode;

    private int msmqAccessMode;

    private int msmqPriority;

    private boolean isOpened;

    private MSMQXAResourceImpl mXAResource;

    /**
     * Creates a new instance of Channel
     */
    public SendChannelImpl(Endpoint endpoint, MSMQOperation operation, ComponentContext context, boolean isOutbound) {
        mEndpoint = endpoint;
        msmqOperation = operation;
        mContext = context;
        msmqInput = msmqOperation.getMSMQOperationInput();
        msmqOutput = msmqOperation.getMSMQOperationOutput();

        if (isOutbound) {
            msmqMessage = msmqInput.getMsmqMessage();
        } else {
            msmqMessage = msmqOutput.getMsmqMessage();
        }
    }

    /**
     * Establishes the physical connection to the underlying system.
     *
     * @throws application specific MSMQException upon error.
     */
    synchronized public void open() throws MSMQException {
        if (!isOpened) {
            msmqAPI = new MSMQAPI();
            msmqHandle = new MSMQHandle();
            mConnMode = msmqMessage.getConnectionMode();
            mDestination = msmqMessage.getDestination();

            if (mConnMode.equals("QueueAlias")) {
                mQueueAlias = mDestination;
                mServer = mQueueAlias;
                msmqAPI.setADsPath(mQueueAlias);
            } else if (mConnMode.equals("QueueFormatName")) {
                mQueueFormatName = mDestination;
                mServer = mQueueFormatName;
                msmqAPI.setQueueFormatName(mServer);
            } else if (mConnMode.equals("DistributionList")) {
                mDistributionList = mDestination;
                mServer = mDistributionList;
                msmqAPI.setDistributionList(mDistributionList);
            } else if (mConnMode.equals("MulticastAddress")) {
                mMulticastAddress = mDestination;
                mServer = mMulticastAddress;
                msmqAPI.setMulticastAddress(mMulticastAddress);
            } else if (mConnMode.equals("MultipleElementFormatName")) {
                mMultipleElementFormatName = mDestination;
                mServer = mMultipleElementFormatName;
                msmqAPI.setMultipleElementFormatName(mMultipleElementFormatName);
            } else {
                mMSMQHost = mEndpoint.getMSMQAddress().getHostName();
                mQueueName = mDestination;
                mServer = mMSMQHost + "\\" + mQueueName;
                msmqAPI.setQueuePathName(mServer);
            }

            //Default is SEND_ACCESS
            mAccessMode = msmqMessage.getAccessMode();

            //get Access Mode configuration
            if (mAccessMode.equals("SEND_ACCESS")) {
                msmqAccessMode = MSMQAccessMode.SEND_ACCESS;
                msmqPriority = msmqMessage.getMessagePriority().intValue();
                msmqAPI.setPriority(msmqPriority);
            }

            mShareMode = msmqMessage.getShareMode();

            //get Share Mode configuration
            if (mShareMode.equals("DENY_RECEIVE_SHARE")) {
                msmqShareMode = MSMQShareMode.DENY_RECEIVE_SHARE;
            } else {
                msmqShareMode = MSMQShareMode.DENY_NONE;
            }

            msgType = msmqMessage.getMessageType();

            //get Message Type configuration
            if (msgType != null) {
                msmqAPI.setMessageType(MSMQMessageTypes.getMessageType(msgType));
            } else {
                msmqAPI.setMessageType(MSMQMessageTypes.ARRAY_OF_BYTES_VAL);
            }

            mTransaction = msmqMessage.getTransaction();

            //get Transaction value
            if ((mTransaction != null) && (mTransaction.equals(MSMQConstants.XA_TRANSACTION))) {
                msmqAPI.setTransactionType(MSMQTxMode.XA_TRANSACTION);
            }

            try {
                // Open the queue
                long r = msmqAPI.OpenQueue(
                //MSMQServer, already set through setFormatName or setQueuePathName
                        msmqAccessMode, msmqShareMode, msmqHandle);

                if (r < 0) {
                    mLogger.log(Level.INFO, "SendChannelImpl_CONNECT_EX_OPEN_QUEUE", new Object[] { mServer });
                    mLogger.log(Level.INFO, "SendChannelImpl_CONNECT_ERROR_CODE", new Object[] { Long.toHexString(r),
                            MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    String msg = "MSMQ open command failed error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);

                    throw new MSMQException(msg);
                }
                mLogger.log(Level.INFO, "SendChannelImpl_CONNECT_OPEN_HANDLE");

                //this call is must bez we write handler value to log
                msmqHandle.debugPrintHandle();
                isOpened = true;
            } catch (Exception ex) {
                mLogger.log(Level.INFO, "SendChannelImpl_CONNECT_QUEUE_FAILED", new Object[] { mServer });
                throw new MSMQException(MSMQUtil.getStackTraceAsString(ex));
            }
        }
    }

    public void send(MSMQPayLoad payload) throws MSMQException {

        if (!isOpened) {
            mLogger.log(Level.INFO, "SendChannelImpl_SEND_CONNECT", new Object[] { mServer });
            open();
        }
        if (isOpened) {
            long r = 0;
            String msg = "";
            try {
                if (!mAccessMode.equals("SEND_ACCESS")) {
                    mLogger.log(Level.INFO, "SendChannelImpl_SEND_ACCESS_MODE_FAIL");
                    forceClose();
                    throw new MSMQException("SendChannelImpl_SEND_ACCESS_CODE");
                }
                msmqAPI.setDataOut(payload.getPayLoad());

                mLogger.log(Level.INFO, "SendChannelImpl_SEND_QUEUE_HANDLER");
                //called to print queuehandler
                msmqHandle.debugPrintHandle();
                r = msmqAPI.SendMessage(msmqHandle);

                if (r < 0) {
                    mLogger.log(Level.INFO, "SendChannelImpl_SEND_FAILED", new Object[] { Long.toHexString(r),
                            MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "Fail to execute MSMQ sendMessage command error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    forceClose();
                    throw new MSMQException(msg);
                }
            } catch (Exception e) {
                throw new MSMQException("Fail to send message: " + MSMQUtil.getStackTraceAsString(e));
            }
            mLogger.log(Level.INFO, "SendChannelImpl_SEND_SUCCESS");
        } else {
            mLogger.log(Level.SEVERE, "SendChannelImpl_CHANNEL_STOPPED", new Object[] { mServer, "send" });

            String errMsg = mMessages.getString("SendChannelImpl_CHANNEL_STOPPED", new Object[] { mServer, "send" });
            throw new MSMQException(errMsg);
        }
    }

    public void send(MSMQPayLoad payload, Transaction tx) throws MSMQException {
        if (!isOpened) {
            mLogger.log(Level.INFO, "SendChannelImpl_SEND_CONNECT", new Object[] { mServer });
            open();
        }
        if (isOpened) {
            long r = 0;
            String msg = "";
            XAResource xar = null;
            try {
                xar = getXAResource();
            } catch (Exception e) {
                close();
                String stackTr = MSMQUtil.getStackTraceAsString(e);
                mLogger.log(Level.INFO, "SendChannelImpl_XARESOURCE_UNATTAINABLE", new Object[] { mServer, stackTr });
                throw new MSMQException(e);
            }

            try {
                tx.enlistResource(xar);
                mLogger.log(Level.INFO, "SendChannelImpl_XARESOURCE_ENLIST_SUCCEEDED", new Object[] { xar.toString(),
                        tx.toString() });
            } catch (Exception e) {
                String stackTr = MSMQUtil.getStackTraceAsString(e);
                mLogger.log(Level.INFO, "SendChannelImpl_XARESOURCE_ENLIST_FAILED", new Object[] { mServer, stackTr });
                throw new MSMQException(e);
            }

            try {
                if (!mAccessMode.equals("SEND_ACCESS")) {
                    mLogger.log(Level.INFO, "SendChannelImpl_SEND_ACCESS_MODE_FAIL");
                    forceClose();
                    throw new MSMQException("SendChannelImpl_SEND_ACCESS_CODE");
                }
                msmqAPI.setDataOut(payload.getPayLoad());

                mLogger.log(Level.INFO, "SendChannelImpl_SEND_QUEUE_HANDLER");
                //call to print queuehandler value
                msmqHandle.debugPrintHandle();

                r = msmqAPI.SendMessage(msmqHandle);
                if (r < 0) {
                    mLogger.log(Level.INFO, "SendChannelImpl_SEND_FAILED", new Object[] { Long.toHexString(r),
                            MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "Fail to execute MSMQ sendMessage command error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    forceClose();
                    throw new MSMQException(msg);
                }
                
                try {
                    tx.delistResource(xar, XAResource.TMSUCCESS);
                } catch (Exception e) {
                    String stackTr = MSMQUtil.getStackTraceAsString(e);
                    mLogger.log(Level.INFO, "SendChannelImpl_XARESOURCE_DELIST_FAILED", new Object[] { mServer, stackTr });

                    String errMsg = mMessages.getString("SendChannelImpl_XARESOURCE_DELIST_FAILED", new Object[] { mServer,
                            stackTr });
                    throw new MSMQException(errMsg);
                }
            } catch (MSMQException e) {
                try {
                    tx.setRollbackOnly();
                } catch (Exception ex) {
                    String stackTr = MSMQUtil.getStackTraceAsString(ex);
                    mLogger.log(Level.SEVERE, "SendChannelImpl_TX_SET_ROLLBACK_FAILED", new Object[] { tx.toString(),
                            stackTr });
                }
    
                try {
                    tx.delistResource(xar, XAResource.TMFAIL);
                } catch (Exception ex) {
                    String stackTr = MSMQUtil.getStackTraceAsString(ex);
                    mLogger.log(Level.INFO, "SendChannelImpl_XARESOURCE_DELIST_FAILED", new Object[] { mServer, stackTr });
                }
                throw new MSMQException("Fail to send message: " + MSMQUtil.getStackTraceAsString(e));
            }
            mLogger.log(Level.INFO, "SendChannelImpl_SEND_SUCCESS");
        } else {
            mLogger.log(Level.SEVERE, "SendChannelImpl_CHANNEL_STOPPED", new Object[] { mServer, "send" });

            String errMsg = mMessages.getString("SendChannelImpl_CHANNEL_STOPPED", new Object[] { mServer, "send" });
            throw new MSMQException(errMsg);
        }
    }

    /**
     * Closes(destroys) the physical connection to the underlying system.
     *
     * @throws application specific MSMQException upon error.
     */
    synchronized public void close() throws MSMQException {
        mLogger.log(Level.INFO, "SendChannelImpl_CLOSEOUT_CALLED");

        String msg;
        if (isOpened) {
            try {
                long r = 0;
                if (msmqHandle != null) {
                    r = msmqAPI.CloseQueue(msmqHandle);
                }
                if (r < 0) {
                    mLogger.log(Level.INFO, "SendChannelImpl_CLOSE_EX_CLOSE_QUEUE", new Object[] { mServer });
                    mLogger.log(Level.INFO, "SendChannelImpl_CLOSE_ERROR_CODE", new Object[] { Long.toHexString(r),
                            MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "MSMQ closeQueue command Failed error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    throw new MSMQException(msg);
                }
                if (msmqCursor != null) {
                    msmqAPI.CloseCursor(msmqCursor);
                }
                if (r < 0) {
                    mLogger.log(Level.INFO, "SendChannelImpl_CLOSE_CLOSE_CURSOR_FAIL", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "MSMQ closeCursor command Failed error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    throw new MSMQException(msg);
                }
                isOpened = false;
                msmqHandle = null;
                msmqCursor = null;
            } catch (MSMQException ex) {
                isOpened = false;
                msmqHandle = null;
                msmqCursor = null;
                throw new MSMQException(ex);
            } finally {
                isOpened = false;
                msmqHandle = null;
                msmqCursor = null;
            }
            mLogger.log(Level.INFO, "SendChannelImpl_CLOSE_SUCCESS", new Object[] { mServer });
        } else {
            mLogger.log(Level.INFO, "SendChannelImpl_CLOSE_ALREADY", new Object[] { mServer });
            isOpened = false;
            msmqHandle = null;
            msmqCursor = null;
        }

    }
    public MSMQPayLoad sendAckResponse(MSMQPayLoad payload) throws MSMQException {
        if (!isOpened) {
            mLogger.log(Level.INFO, "SendChannelImpl_SEND_CONNECT", new Object[] { mServer });
            open();
        }
        if (isOpened) {
            long r = 0;
            String errMsg = "";
            try {
                if (!mAccessMode.equals("SEND_ACCESS")) {
                    mLogger.log(Level.INFO, "SendChannelImpl_SEND_ACCESS_MODE_FAIL");
                    forceClose();
                    throw new MSMQException("SendChannelImpl_SEND_ACCESS_CODE");
                }
                msmqAPI.setDataOut(payload.getPayLoad());

                mLogger.log(Level.INFO, "SendChannelImpl_SEND_QUEUE_HANDLER");
                //called to print queuehandler
                msmqHandle.debugPrintHandle();
                if(msmqInput.getMsmqMessage().isAcknowledgement())
                	r = msmqAPI.SendMessageWithAckRequest(msmqHandle);
                else
                	r = msmqAPI.SendMessageWithResponseRequest(msmqHandle);

                if (r < 0) {
                    mLogger.log(Level.INFO, "SendChannelImpl_SEND_FAILED", new Object[] { Long.toHexString(r),
                            MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    errMsg = "Fail to execute MSMQ sendMessage command error:" + Long.toHexString(r)
                    + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    forceClose();
                    throw new MSMQException(errMsg);
                }
            } catch (Exception e) {
                throw new MSMQException("Fail to send message: " + MSMQUtil.getStackTraceAsString(e));
            }
            mLogger.log(Level.INFO, "SendChannelImpl_SEND_SUCCESS");
            byte[] msgId = msmqAPI.getDataIn();
            
            ReceiveChannelImpl receiveChannel = new ReceiveChannelImpl(mEndpoint, msmqOperation, mContext, false);
            Request req = new Request(receiveChannel);
            MSMQPayLoad msg = req.receive(msgId);
            return msg;
        } else {
            mLogger.log(Level.SEVERE, "SendChannelImpl_CHANNEL_STOPPED", new Object[] { mServer, "send" });
            String errMsg = mMessages.getString("SendChannelImpl_CHANNEL_STOPPED", new Object[] { mServer, "send" });
            throw new MSMQException(errMsg);
        }
            
    	//return null;
    }
        

    public Endpoint getEndpoint() {
        return mEndpoint;
    }

    public MSMQOperation getMSMQOperation() {
        return msmqOperation;
    }

    public MSMQMessage getMSMQMessage() {
        return msmqMessage;
    }

    public XAResource getXAResource() throws XAException {
        try {
            if ((mTransaction != null) && (mTransaction.equals(MSMQConstants.XA_TRANSACTION))) {
                if (this.mXAResource == null) {
                    mLogger.log(Level.INFO, "SendChannelImpl_GETXARESOURCE_NEW");
                    mXAResource = new MSMQXAResourceImpl();
                }
                return mXAResource;
            } else {
                mLogger.log(Level.INFO, "SendChannelImpl_GETXARESOURCE_FAILED");
                return null;
            }
        } catch (Exception e) {
            throw new XAException(MSMQUtil.getStackTraceAsString(e));
        }
    }

    private void forceClose() throws MSMQException {
        try {
            close();
        } catch (Exception e) {
            throw new MSMQException(e);
        }
    }
}
