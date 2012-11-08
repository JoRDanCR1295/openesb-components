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
 * @(#)ReceiveChannelImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.msmq;

import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.component.ComponentContext;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.msmqbc.Endpoint;
import com.sun.jbi.msmqbc.MSMQPayLoad;
import com.sun.jbi.msmqbc.util.MSMQUtil;
import com.sun.jbi.msmqbc.jni.MSMQAPI;
import com.sun.jbi.msmqbc.jni.MSMQHandle;
import com.sun.jbi.msmqbc.jni.MSMQAccessMode;
import com.sun.jbi.msmqbc.jni.MSMQShareMode;
import com.sun.jbi.msmqbc.jni.MSMQAPIReturnCode;
import com.sun.jbi.msmqbc.jni.MSMQRcvActionCode;
import com.sun.jbi.msmqbc.jni.MSMQTxMode;
import com.sun.jbi.msmqbc.exception.MSMQException;
import com.sun.jbi.msmqbc.extensions.MSMQConstants;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;
import com.sun.jbi.msmqbc.extensions.MSMQInput;
import com.sun.jbi.msmqbc.extensions.MSMQOutput;
import com.sun.jbi.msmqbc.extensions.MSMQMessage;
import com.sun.jbi.msmqbc.transaction.MSMQXAResourceImpl;

/**
 * Class representing exact channel for receiving message from microsoft message queue.
 * 
 * @author Sun Microsystems
 */
public class ReceiveChannelImpl extends AbstractChannel {

    private static final Messages mMessages = Messages.getMessages(ReceiveChannelImpl.class);

    private static final Logger mLogger = Messages.getLogger(ReceiveChannelImpl.class);

    private ComponentContext mContext;

    private Endpoint mEndpoint;

    private MSMQOperation msmqOperation;

    private MSMQInput msmqInput;

    private MSMQOutput msmqOutput;

    private MSMQMessage msmqMessage;

    private MSMQAPI msmqAPI;

    private MSMQHandle msmqHandle;

    private MSMQHandle msmqCursor;

    private String msmqHost;

    private String mCntMode;

    private String mDestination;

    private String mQueueAlias;

    private String mQueueFormatName;

    private String mQueueName;

    private String mServer;

    private String mAccessMode;

    private String mActionCode;

    private String mShareMode;

    private String mTransaction;

    private int msmqShareMode;

    private int msmqAccessMode;

    private int msmqActionCode;

    private Long mMSMQRecMsgLookupID;

    private boolean isOpened;

    private XAResource mXAResource;

    /**
     * Creates a new instance of Channel
     */
    public ReceiveChannelImpl(Endpoint endpoint, MSMQOperation operation, ComponentContext context, boolean isOutbound) {
        mEndpoint = endpoint;
        msmqOperation = operation;
        mContext = context;
        msmqInput = operation.getMSMQOperationInput();
        msmqOutput = operation.getMSMQOperationOutput();
        if (isOutbound) {
            msmqMessage = msmqInput.getMsmqMessage();
        } else {
        	if (msmqOutput != null){
        		msmqMessage = msmqOutput.getMsmqMessage();
        	} else {
        		msmqMessage = msmqInput.getMsmqMessage();
        	}
        	
        }        
    }

    // /////
    // public methods
    // /////

    /**
     * Establishes the physical connection to the underlying system.
     * 
     * @throws application specific MSMQException upon error.
     */
    synchronized public void open() throws MSMQException {
        if (!isOpened) {
            msmqAPI = new MSMQAPI();
            msmqHandle = new MSMQHandle();
            mCntMode = msmqMessage.getConnectionMode();
            mDestination = msmqMessage.getDestination();
            if (mCntMode.equals("QueueAlias")) {
                mQueueAlias = mDestination;
                mServer = mQueueAlias;
                msmqAPI.setADsPath(mServer);
            } else if (mCntMode.equals("QueueFormatName")) {
                mQueueFormatName = mDestination;
                mServer = mQueueFormatName;
                msmqAPI.setQueueFormatName(mServer);
            } else {
                msmqHost = mEndpoint.getMSMQAddress().getHostName();
                mQueueName = mDestination;
                mServer = msmqHost + "\\" + mQueueName;
                msmqAPI.setQueuePathName(mServer);
            }

            mAccessMode = msmqMessage.getAccessMode();

            // get Access Mode configuration
            if (mAccessMode.equals("PEEK_ACCESS")) {
                msmqAccessMode = MSMQAccessMode.PEEK_ACCESS;
            } else {
                msmqAccessMode = MSMQAccessMode.RECEIVE_ACCESS;
            }

            mShareMode = msmqMessage.getShareMode();

            // get Share Mode configuration
            if (mShareMode.equals("DENY_RECEIVE_SHARE")) {
                msmqShareMode = MSMQShareMode.DENY_RECEIVE_SHARE;
            } else {
                msmqShareMode = MSMQShareMode.DENY_NONE;
            }

            mTransaction = msmqMessage.getTransaction();

            // get Transaction value
            if ((mTransaction != null) && (mTransaction.equals(MSMQConstants.XA_TRANSACTION))) {
                msmqAPI.setTransactionType(MSMQTxMode.XA_TRANSACTION);
            }

            try {
                // Open the queue
                long r = msmqAPI.OpenQueue(
                // MSMQServer, already set through setFormatName or setQueuePathName
                        msmqAccessMode, msmqShareMode, msmqHandle);

                if (r < 0) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_CONNECT_EX_OPEN_QUEUE", new Object[] { mServer });
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_CONNECT_ERROR_CODE", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    String msg = "MSMQ openQueue command Failed error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    mLogger.log(Level.INFO, msg);
                    throw new MSMQException(msg);
                }
                mLogger.log(Level.INFO, "ReceiveChannelImpl_CONNECT_OPEN_HANDLE");
                // call to print handle value
                msmqHandle.debugPrintHandle();
                isOpened = true;
            } catch (Exception ex) {
                mLogger.log(Level.INFO, "ReceiveChannelImpl_CONNECT_QUEUE_FAILED", new Object[] { mServer });
                throw new MSMQException(MSMQUtil.getStackTraceAsString(ex));
            }
        }
    }

    public MSMQPayLoad receive() throws MSMQException {
        if (!isOpened) {
            mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CONNECT", new Object[] { mServer });
            open();
        }
        if (isOpened) {
            long r = 0;
            byte[] byteResp = null;
            MSMQPayLoad mPayload = new MSMQPayLoad();
            String msg = "";
            try {
                if (!isOpened) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CONNECT_EX", new Object[] { mServer });
                    throw new MSMQException("MSMQ Receive call failed due to no connection to queue");
                }
                if (mAccessMode.equals("SEND_ACCESS")) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_SEND_ACCESS");
                    throw new MSMQException("Access Mode cannot be SEND_ACCESS when getting MSMQ Message.");
                }

                mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_QUEUE_HANDLER");
                // call to display handler
                msmqHandle.debugPrintHandle();

                // Create a cursor
                msmqCursor = new MSMQHandle();

                // only read message needs cursor
                r = msmqAPI.CreateCursor(msmqHandle, msmqCursor);
                if (r < 0) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CURSOR_FAILED", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "Fail to create MSMQ Inbound Cursor...error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    close();
                    throw new MSMQException(msg);
                }
                mLogger.log(Level.INFO, "MSMQ Cursor created...");

                mActionCode = msmqMessage.getActionCode();
                if (mActionCode.equals("LOOKUP_PEEK_CURRENT")) {
                    msmqActionCode = MSMQRcvActionCode.LOOKUP_PEEK_CURRENT;
                } else if (mActionCode.equals("LOOKUP_RECEIVE_CURRENT")) {
                    msmqActionCode = MSMQRcvActionCode.LOOKUP_RECEIVE_CURRENT;
                } else if (mActionCode.equals("ACTION_PEEK_CURRENT")) {
                    msmqActionCode = MSMQRcvActionCode.ACTION_PEEK_CURRENT;
                } else {
                    msmqActionCode = MSMQRcvActionCode.ACTION_RECEIVE;
                }
                mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_ACTION_CODE", new Object[] { msmqActionCode });

                // Message Lookup Identifier
                mMSMQRecMsgLookupID = msmqMessage.getMsgLookupID();

                if (mMSMQRecMsgLookupID != null) {
                    // ReceiveMessage by lookup identifier
                    r = msmqAPI.ReceiveMessage(msmqHandle, mMSMQRecMsgLookupID, msmqActionCode, msmqCursor);
                } else {
                    // ReceiveMessage
                    r = msmqAPI.ReceiveMessage(msmqHandle, 0, msmqActionCode, msmqCursor);
                }
                if (r < 0 && r != MSMQAPIReturnCode.MQ_ERROR_IO_TIMEOUT) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_MESSAGE_FAIL", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "Fail to execute MSMQ receiveMessage command... error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    forceClose();
                    throw new MSMQException(msg);
                } else if (r == 0) {
                    // we received message successfully
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_MESSAGE");

                    byteResp = msmqAPI.getDataIn();
                    if (byteResp != null) {
                        mPayload.setPayLoad(byteResp);
                        mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_MESSAGE_LEN",
                                new Object[] { byteResp.length });
                    } else {
                        mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_MESSAGE_NULL");
                    }
                }

                r = msmqAPI.CloseCursor(msmqCursor);
                if (r < 0) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CLOSE_CURSOR_FAILED", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "Fail to close MSMQ Cursor...error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    mLogger.log(Level.INFO, msg);
                    forceClose();
                    throw new MSMQException("Fail to close MSMQ Inbound Cursor..." + msg);
                }
                mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CLOSE_CURSOR_SUCCESS", new Object[] { mServer });
                msmqCursor = null;

            } catch (Exception e) {
                throw new MSMQException(MSMQUtil.getStackTraceAsString(e));
            }
            mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_SUCCESS");
            return mPayload;
        } else {
            mLogger.log(Level.SEVERE, "ReceiveChannelImpl_CHANNEL_STOPPED", new Object[] { mServer, "receive" });

            String errMsg = mMessages.getString("ReceiveChannelImpl_RECEIVE_CHANNEL_STOPPED", new Object[] { mServer,
                    "receive" });
            throw new MSMQException(errMsg);
        }
    }

    public MSMQPayLoad receiveAckResponseMessage(String msgId) throws MSMQException {
        if (!isOpened) {
            mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CONNECT", new Object[] { mServer });
            open();
        }
        if (isOpened) {
            long r = 0;
            byte[] byteResp = null;
            MSMQPayLoad mPayload = new MSMQPayLoad();
            String msg = "";
            try {
                if (!isOpened) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CONNECT_EX", new Object[] { mServer });
                    throw new MSMQException("MSMQ Receive call failed due to no connection to queue");
                }
                if (mAccessMode.equals("SEND_ACCESS")) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_SEND_ACCESS");
                    throw new MSMQException("Access Mode cannot be SEND_ACCESS when getting MSMQ Message.");
                }

                mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_QUEUE_HANDLER");
                // call to display handler
                msmqHandle.debugPrintHandle();

/*                // Create a cursor
                msmqCursor = new MSMQHandle();

                // only read message needs cursor
                r = msmqAPI.CreateCursor(msmqHandle, msmqCursor);
                if (r < 0) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CURSOR_FAILED", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "Fail to create MSMQ Inbound Cursor...error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    close();
                    throw new MSMQException(msg);
                }
                mLogger.log(Level.INFO, "MSMQ Cursor created...");

                mActionCode = msmqMessage.getActionCode();
                if (mActionCode.equals("LOOKUP_PEEK_CURRENT")) {
                    msmqActionCode = MSMQRcvActionCode.LOOKUP_PEEK_CURRENT;
                } else if (mActionCode.equals("LOOKUP_RECEIVE_CURRENT")) {
                    msmqActionCode = MSMQRcvActionCode.LOOKUP_RECEIVE_CURRENT;
                } else if (mActionCode.equals("ACTION_PEEK_CURRENT")) {
                    msmqActionCode = MSMQRcvActionCode.ACTION_PEEK_CURRENT;
                } else {
                    msmqActionCode = MSMQRcvActionCode.ACTION_RECEIVE;
                }
                mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_ACTION_CODE", new Object[] { msmqActionCode });*/

                // Message Lookup Identifier
                //mMSMQRecMsgLookupID = msmqMessage.getMsgLookupID();
                String queueName = this.getMSMQOperationOutput().getMsmqMessage().getDestination();
                r = msmqAPI.GetAckResponseMessages(queueName, msgId, msmqHandle);

                if (r < 0 && r != MSMQAPIReturnCode.MQ_ERROR_IO_TIMEOUT) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_MESSAGE_FAIL", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "Fail to execute MSMQ receiveMessage command... error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    forceClose();
                    throw new MSMQException(msg);
                } else if (r == 0) {
                    // we received message successfully
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_MESSAGE");

                    byteResp = msmqAPI.getDataIn();
                    if (byteResp != null) {
                        mPayload.setPayLoad(byteResp);
                        mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_MESSAGE_LEN",
                                new Object[] { byteResp.length });
                    } else {
                        mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_MESSAGE_NULL");
                    }
                }

/*                r = msmqAPI.CloseCursor(msmqCursor);
                if (r < 0) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CLOSE_CURSOR_FAILED", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "Fail to close MSMQ Cursor...error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    mLogger.log(Level.INFO, msg);
                    forceClose();
                    throw new MSMQException("Fail to close MSMQ Inbound Cursor..." + msg);
                }
                mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_CLOSE_CURSOR_SUCCESS", new Object[] { mServer });
                msmqCursor = null;*/

            } catch (Exception e) {
                throw new MSMQException(MSMQUtil.getStackTraceAsString(e));
            }
            mLogger.log(Level.INFO, "ReceiveChannelImpl_RECEIVE_SUCCESS");
            return mPayload;
        } else {
            mLogger.log(Level.SEVERE, "ReceiveChannelImpl_CHANNEL_STOPPED", new Object[] { mServer, "receive" });

            String errMsg = mMessages.getString("ReceiveChannelImpl_RECEIVE_CHANNEL_STOPPED", new Object[] { mServer,
                    "receive" });
            throw new MSMQException(errMsg);
        }
    }
    /**
     * Closes(destroys) the physical connection to the underlying system.
     * 
     * @throws application specific Exceptions upon error.
     */
    synchronized public void close() throws MSMQException {
        mLogger.log(Level.INFO, "ChannelImpl_CLOSEOUT_CALLED");

        String msg;
        if (isOpened) {
            try {
                long r = 0;
                if (msmqHandle != null) {
                    r = msmqAPI.CloseQueue(msmqHandle);
                }
                if (r < 0) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_CLOSE_EX_CLOSE_QUEUE", new Object[] { mServer });
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_CLOSE_ERROR_CODE", new Object[] { Long.toHexString(r),
                            MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "MSMQ closeQueue command Failed error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    throw new MSMQException(msg);
                }
                if (msmqCursor != null) {
                    msmqAPI.CloseCursor(msmqCursor);
                }
                if (r < 0) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_CLOSE_CLOSE_CURSOR_FAIL", new Object[] {
                            Long.toHexString(r), MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r) });
                    msg = "MSMQ closeCursor command Failed error:" + Long.toHexString(r)
                            + MSMQAPIReturnCode.MapMSMQCodeToStringMsg(r);
                    throw new MSMQException(msg);
                }
                isOpened = false;
                msmqHandle = null;
                msmqCursor = null;
            } catch (Exception e) {
                isOpened = false;
                msmqHandle = null;
                msmqCursor = null;
                throw new MSMQException(MSMQUtil.getStackTraceAsString(e));
            } finally {
                isOpened = false;
                msmqHandle = null;
                msmqCursor = null;
            }
            mLogger.log(Level.INFO, "ReceiveChannelImpl_CLOSE_SUCCESS", new Object[] { mServer });
        } else {
            mLogger.log(Level.INFO, "ReceiveChannelImpl_CLOSE_ALREADY");
            isOpened = false;
            msmqHandle = null;
            msmqCursor = null;
        }

    }

    public Endpoint getEndpoint() {
        return mEndpoint;
    }

    public MSMQOperation getMSMQOperation() {
        return msmqOperation;
    }

    public MSMQInput getMSMQOperationInput() {
        return msmqInput;
    }

    public MSMQMessage getMSMQMessage() {
        return msmqMessage;
    }

    public XAResource getXAResource() throws XAException {
        try {
            if ((mTransaction != null) && (mTransaction.equals(MSMQTxMode.XA_TRANSACTION))) {
                if (this.mXAResource == null) {
                    mLogger.log(Level.INFO, "ReceiveChannelImpl_GETXARESOURCE_NEW");
                    mXAResource = new MSMQXAResourceImpl();
                }
                return mXAResource;
            } else {
                mLogger.log(Level.INFO, "ReceiveChannelImpl_GETXARESOURCE_FAILED");
                return null;
            }
        } catch (Exception e) {
            throw new XAException(MSMQUtil.getStackTraceAsString(e));
        }
    }

    ///////
    // private methods
    ///////
    private void forceClose() throws MSMQException {
        try {
            close();
        } catch (Exception e) {
            throw new MSMQException(e);
        }
    }
}
