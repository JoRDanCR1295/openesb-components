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
 * @(#)MSMQAPI.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.jni;

// This class is the jni shim layer on top of the msmq api
// All calls return an int specifying the HRESULT of the call
// When a call returns a string, it is returned in a StringBuffer
// class by calling the insert method on that class. Thus, it is
// assumed that the stringbuffer passed in is empty. No attempt
// is made to clear it before inserting the return string.

// import org.apache.log4j.Logger;
// import org.apache.log4j.PropertyConfigurator;
// import org.apache.log4j.BasicConfigurator;

// using JVM logger to have the msmqjni.jar have miminum dependency
// not even mport com.stc.connector.logging.Logger.
// because the class is going to be loaded by is class loader v.s.
// connector class loader.
// tomato is using jdk logger will the log message will appear
// webspher and weblogic is log4j logger the jvm logger may be null
// so the mseeage in jni layer may not appear
import java.util.logging.Logger;

/**
 * @author Sun Microsystems
 */

public class MSMQAPI {

    public static final int QUEUE_NAME_IS_PATHNAME = 0;

    public static final int QUEUE_NAME_IS_FORMATNAME = 1;

    public static final int QUEUE_NAME_IS_ADSPATH = 2;

    public static final int QUEUE_NAME_IS_MULTICASTADRESS = 3;

    public static final int QUEUE_NAME_IS_DISTRIBUTIONLIST = 4;

    public static final int QUEUE_NAME_IS_MULTIPLEFORMATNAME = 5;

    private Logger mLogger = Logger.getLogger(getClass().getName());

    /**
     * Data which gets set by the JNI code when data is received from the MSMQ.
     */
    private byte[] _DataIn;

    /**
     * Data which is passed to the JNI code for sending data to the MSMQ.
     */
    private byte[] _DataOut;

    /**
     * String which gives the Path Name of the Queue
     */
    private String _QueuePath;

    /**
     * int which gives the priority of the message
     */
    private int _Priority = 3;

    /**
     * int which gives the active directory path of the message
     */
    private String _ADsPath;

    /**
     * string with gives formated name
     */
    private String _QueueFormatName;

    /**
     * int which gives the transaction type
     */
    private int _TransactionType = 0;

    /**
     * string with gives the message type
     */
    private String _MessageType;

    /**
     * Queue Name Pattern: 0: path name 1: format name 2: ads path 3: multicast address 4:
     * ditribution list 5: multiple element format name
     */
    private int _QNamePattern = 0;

    /**
     * string which gives multicast address name
     */
    private String _MulticastAddress;

    /**
     * string which gives distribution list name
     */
    private String _DistributionList;

    /**
     * string which gives multiple element format name
     */
    private String _MultipleElementFormatName;

    /**
     * Constructor to create MSMQAPI
     */
    public MSMQAPI() {
        _DataIn = null;
        _DataOut = null;
    }

    /**
     * Sets data which gets set by the JNI code when data is received from the partner in the SNA
     * conversation.
     * 
     * @param data The DataIn to be set
     */
    public void setDataIn(byte[] data) {
        this._DataIn = data;
    }

    /**
     * Gets data which gets set by the JNI code when data is received from the partner in the SNA
     * conversation.
     * 
     * @return DataIn as a byte array
     */
    public byte[] getDataIn() {
        return this._DataIn;
    }

    /**
     * Sets data which is passed to the JNI code for sending data to the partner in the SNA
     * conversation.
     * 
     * @param data The DataOut to be set
     */
    public void setDataOut(byte[] data) {
        this._DataOut = data;
    }

    /**
     * Gets data which is passed to the JNI code for sending data to the partner in the SNA
     * conversation.
     * 
     * @return DataOut as a byte array
     */
    public byte[] getDataOut() {
        return this._DataOut;
    }

    public void setQueuePathName(String path) {
        this._QueuePath = path;
        this._QNamePattern = this.QUEUE_NAME_IS_PATHNAME;
    }

    public String getQueuePathName() {
        return this._QueuePath;
    }

    public void setPriority(int priority) {
        this._Priority = priority;
    }

    public int getPriority() {
        return this._Priority;
    }

    public void setTransactionType(String transactionType) {
        if ("NoTransaction".trim().equals(transactionType)) {
            this._TransactionType = 0;
        } else if ("XATransaction".trim().equals(transactionType)) {
            this._TransactionType = 2;
        } else {
            this._TransactionType = 3;
        }
    }

    public String getTransactionType() {
        if (this._TransactionType == 0) {
            return "NoTransaction";
        } else if (this._TransactionType == 2) {
            return "XATransaction";
        } else {
            return "SingleTransaction";
        }
    }

    public void setMessageType(String msgType) {
        this._MessageType = msgType;
    }

    public String getMessageType() {
        return this._MessageType;
    }

    public void setADsPath(String path) {
        this._ADsPath = path;
        this._QNamePattern = this.QUEUE_NAME_IS_ADSPATH;
    }

    public String getADsPath() {
        return this._ADsPath;
    }

    public void setQueueNamePattern(int qnmpattern) {
        this._QNamePattern = qnmpattern;
    }

    public int getQueueNamePattern() {
        return this._QNamePattern;
    }

    public void setQueueFormatName(String varStr) {
        this._QueueFormatName = varStr;
        this._QNamePattern = this.QUEUE_NAME_IS_FORMATNAME;
    }

    public String gettQueueFormatName() {
        return this._QueueFormatName;
    }

    /**
     * Gets Multicast Address associated with the Queue.
     * 
     * @return Multicast Address as String
     */
    public String getMulticastAddress() {
        return this._MulticastAddress;
    }

    /**
     * Sets Multicast Address associated with the Queue.
     * 
     * @param Multicast Address as String
     */
    public void setMulticastAddress(String multicastAddress) {
        this._MulticastAddress = multicastAddress;
        this._QNamePattern = QUEUE_NAME_IS_MULTICASTADRESS;
    }

    /**
     * Gets Distribution List defined in the Active Directory.
     * 
     * @return Distribution List as String
     */
    public String getDistributionList() {
        return this._DistributionList;
    }

    /**
     * Sets Distribution List defined in the Active Directory.
     * 
     * @param Distribution List as String
     */
    public void setDistributionList(String distributionList) {
        this._DistributionList = distributionList;
        this._QNamePattern = QUEUE_NAME_IS_DISTRIBUTIONLIST;
    }

    /**
     * Gets Mulitple Element Format Names of different Queues.
     * 
     * @return Multiple Element Format Names as String
     */
    public String getMultipleElementFormatName() {
        return this._MultipleElementFormatName;
    }

    /**
     * Sets Mulitple Element Format Names of different Queues.
     * 
     * @param Multiple Element Format Names as String
     */
    public void setMultipleElementFormatName(String multipleElementFormatName) {
        this._MultipleElementFormatName = multipleElementFormatName;
        this._QNamePattern = QUEUE_NAME_IS_MULTIPLEFORMATNAME;
    }

    /**
     * javadoc
     */
    public long CreateQueue(int cProp, // [in] # props in the following arrays
                            int[] aPropID, // [in] MSMQPropID values for the propvar array
                            MQPROPVARIANT[] aPropVar, // [in] the properties
                            int[] aStatus, // [out] results for each property
                            StringBuffer formatName) { // [out] Q format name on success
        return jniMQCreateQueue(cProp, aPropID, aPropVar, aStatus, formatName);
    }

    /**
     * javadoc
     */
    public long OpenQueue(int dwAccess, // [in] one of the MSMQAccessMode values
                          int dwShareMode, // [in] one of the MSMQShareMode values
                          MSMQHandle hQueue) { // [out] Q handle
        return jniMQOpenQueue(dwAccess, dwShareMode, hQueue);
    }

    /**
     * javadoc
     */
    public long CloseQueue(MSMQHandle hQueue) {
        return jniMQCloseQueue(hQueue);
    }

    /**
     * javadoc
     */
    public long DeleteQueue(String formatName) {
        return jniMQDeleteQueue(formatName);
    }

    /**
     * javadoc
     */
    public long SendMessage(MSMQHandle hQueue // [in] the queue to send a msg to
    ) { // [in] the message to send
        if (this._DataOut == null || this._DataOut.length == 0) {
            mLogger.info("Send Message no DataOut value");
            return 0;
        }
        if (hQueue == null)
            return jniMQSendMessageToAlias();
        else
            return jniMQSendMessage(hQueue);
    }

    // ReceiveMessage
    public long ReceiveMessage(MSMQHandle hQueue, // [in] Q handle from OpenQueue
                               int timeout, // [in] time in ms, -1 for infinite
                               int action, // [in] one of the MSMQRcvActionCode values
                               MSMQHandle hCursor // [in] the cursor to use - can be null
    // ,byte[] byteAryMsg // [out] the message - Now the return value holds the messg
    ) {
        _DataIn = null;
        if (hQueue == null)
            return jniMQReceiveMessageFromAlias();
        else
            return jniMQReceiveMessage(hQueue, timeout, action, hCursor);
    }

    // ReceiveMessage using message lookup identifier
    public long ReceiveMessage(MSMQHandle hQueue, // [in] Q handle from OpenQueue
                               long msglookupID, // [in] message lookup Identifier value
                               int action, // [in] one of the MSMQRcvActionCode values
                               MSMQHandle hCursor // [in] the cursor to use - can be null
    // ,byte[] byteAryMsg // [out] the message - Now the return value holds the messg
    ) {
        _DataIn = null;
        return jniMQReceiveMessageByLookupId(hQueue, msglookupID, action, hCursor);
    }

    public long SendMessageToAlias() {
        if (this._ADsPath == null || "".equals(_ADsPath)) {
            mLogger.info("Send Message no ADsPath");
            return 0;
        }

        if (this._DataOut == null || this._DataOut.length == 0) {
            mLogger.info("MSMQAPI Send Message no DataOut value");
            return 0;
        }
        return jniMQSendMessageToAlias();
    }

    public long ReceiveMessageFromAlias() {
        if (this._ADsPath == null || "".equals(_ADsPath)) {
            mLogger.info("Send Message no DataOut value");
            return 0;
        }

        return jniMQReceiveMessageFromAlias();
    }
    
	public long SendMessageWithAckRequest(MSMQHandle hQueue) {
		if (this._DataOut == null || this._DataOut.length == 0) {
			mLogger.info("Send Message with Acknowledgement. No data");
			return 0;
		}
		return jniMQSendMessageWithAckRequest(hQueue);
	}

	public long SendMessageWithResponseRequest(MSMQHandle hQueue){
		if(this._DataOut == null || this._DataOut.length == 0){
			mLogger.info("Send Message with Response. No data");
			return 0;
		}
		return jniMQSendMessageWithResponseRequest(hQueue);
	}
	
	public long GetAckResponseMessages(String queueName, String messageId, MSMQHandle hQueue) {
		return jniMQGetAckResponseMessages(queueName, messageId, hQueue);
	}
	
/*	public long GetResponseMessages(String queueName, String messageId, MSMQHandle hQueue) {
		return jniMQGetResponseMessages(queueName, messageId, hQueue);
	}*/

    // CreateCursor
    public long CreateCursor(MSMQHandle hQueue, // [in] Q handle
                             MSMQHandle hCursor) { // [out] the cursor
        return jniMQCreateCursor(hQueue, hCursor);
    }

    // Close Cursor
    public long CloseCursor(MSMQHandle hCursor) { // [in] the cursor to close
        return jniMQCloseCursor(hCursor);
    }

    //
    // Private native methods...
    //

    // HRESULT APIENTRY MQCreateQueue(
    // IN PSECURITY_DESCRIPTOR pSecurityDescriptor,
    // IN OUT MQQUEUEPROPS* pQueueProps,
    // OUT LPWSTR lpwcsFormatName,
    // IN OUT LPDWORD lpdwFormatNameLength);
    //
    // TODO: Add the SECURITY_DESCRIPTOR - currently passing null internally
    // to accept the default.
    // TODO: Add the lpdwFormatNameLength?
    // TODO: If any of the above are done, you'll need to run javah on the
    // class again and modify the jni code.
    //
    // Flattened out the MQQUEUEPROPS structure to make it a lot easier
    // to deal with at the jni level.
    //
    private native long jniMQCreateQueue(int cProp,
                                         int[] aPropID,
                                         MQPROPVARIANT[] aPropVar,
                                         int[] aStatus,
                                         StringBuffer formatName);

    // HRESULT APIENTRY MQOpenQueue(
    // IN LPCWSTR lpwcsFormatName,
    // IN DWORD dwAccess,
    // IN DWORD dwShareMode,
    // OUT QUEUEHANDLE* phQueue);
    private native int jniMQOpenQueue(int dwAccess, int dwShareMode, MSMQHandle hQueue);

    // HRESULT APIENTRY MQCloseQueue(
    // IN HANDLE hQueue);
    private native long jniMQCloseQueue(MSMQHandle hQueue);

    // HRESULT APIENTRY MQDeleteQueue(
    // IN LPCWSTR lpwcsFormatName);
    private native long jniMQDeleteQueue(String formatName);

    // HRESULT APIENTRY MQSendMessage(
    // IN QUEUEHANDLE hDestinationQueue,
    // IN MQMSGPROPS* pMessageProps,
    // IN ITransaction *pTransaction);
    //
    // TODO: No transaction support yet - backburnered to phase two...
    // TODO: MQMSGPROPS not used -
    private native long jniMQSendMessage(MSMQHandle hQueue);
    
	private native long jniMQSendMessageWithAckRequest(MSMQHandle hQueue);
	
	private native long jniMQSendMessageWithResponseRequest(MSMQHandle hQueue);

    // HRESULT APIENTRY MQReceiveMessage(
    // IN QUEUEHANDLE hSource,
    // IN DWORD dwTimeout,
    // IN DWORD dwAction,
    // IN OUT MQMSGPROPS* pMessageProps,
    // IN OUT LPOVERLAPPED lpOverlapped,
    // IN PMQRECEIVECALLBACK fnReceiveCallback,
    // IN HANDLE hCursor,
    // IN ITransaction* pTransaction);
    //
    // TODO: No transaction support yet
    // TODO: No "overlapped" or "rcv callback" support
    // TODO: No mqmsgprops struct support yet - only string msgs supported for now
    private native long jniMQReceiveMessage(MSMQHandle hQueue, int timeout, int action, MSMQHandle hCursor);
    
	private native long jniMQGetAckResponseMessages(String queueName, String messageId, MSMQHandle hQueue);
	
	//private native long jniMQGetResponseMessages(String queueName, String messageId, MSMQHandle hQueue);


    // HRESULT APIENTRY MQReceiveMessageByLookupId(
    // IN QUEUEHANDLE hSource,
    // IN ULONGLONG ullLookupId,
    // IN DWORD dwLookupAction,
    // IN OUT MQMSGPROPS * pMessageProps,
    // IN OUT LPOVERLAPPED lpOverlapped,
    // IN PMQRECEIVECALLBACK fnReceiveCallback,
    // IN ITransaction * pTransaction
    // );
    //
    // TODO: No transaction support yet
    // TODO: No "overlapped" or "rcv callback" support
    // TODO: No mqmsgprops struct support yet - only string msgs supported for now
    private native long jniMQReceiveMessageByLookupId(MSMQHandle hQueue, long lookupID, int action, MSMQHandle hCursor);

    private native long jniMQSendMessageToAlias();

    private native long jniMQReceiveMessageFromAlias();

    // HRESULT APIENTRY MQCreateCursor(
    // IN QUEUEHANDLE hQueue,
    // OUT PHANDLE phCursor);
    private native long jniMQCreateCursor(MSMQHandle hQueue, MSMQHandle hCursor);

    // HRESULT APIENTRY MQCloseCursor(
    //		IN HANDLE hCursor);
    private native long jniMQCloseCursor(MSMQHandle hCursor);

    // Static initializer to load the dll
    static {
        //System.loadLibrary("msmqruntimejni");
    }

}
