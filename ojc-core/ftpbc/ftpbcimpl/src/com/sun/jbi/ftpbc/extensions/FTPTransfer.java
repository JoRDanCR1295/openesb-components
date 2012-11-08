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
 * @(#)FTPTransfer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

import com.sun.jbi.internationalization.Messages;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import java.io.Serializable;
import java.util.logging.Logger;

/**
 * @author jfu
 */
public class FTPTransfer implements ExtensibilityElement, FTPTransferExtension, Serializable {

    private static final long serialVersionUID = 1L;
    private static Messages mMessages =
            Messages.getMessages(FTPTransfer.class);
    private static Logger mLogger = Messages.getLogger(FTPTransfer.class);
    QName fieldElementType = FTPConstants.QNAME_TRANSFER;
    Boolean fieldRequired = Boolean.FALSE;
    public static final String FTP_SENDTO_PROPERTY = "sendTo";
    public static final String FTP_SENDTO_HAS_PATTS_PROPERTY = "sendToHasPatterns";
    public static final String FTP_APPEND_PROPERTY = "append";
    public static final String FTP_RECEIVEFROM_PROPERTY = "receiveFrom";
    public static final String FTP_RECEIVEFROM_HAS_PATTS_PROPERTY = "receiveFromHasRegexs";
    public static final String FTP_PRE_SEND_CMD_PROPERTY = "preSendCommand";
    public static final String FTP_PRE_SEND_LOC_PROPERTY = "preSendLocation";
    public static final String FTP_PRE_SEND_LOC_HAS_PATTS_PROPERTY = "preSendLocationHasPatterns";
    public static final String FTP_PRE_RECEIVE_CMD_PROPERTY = "preReceiveCommand";
    public static final String FTP_PRE_RECEIVE_LOC_PROPERTY = "preReceiveLocation";
    public static final String FTP_PRE_RECEIVE_LOC_HAS_PATTS_PROPERTY = "preReceiveLocationHasPatterns";
    public static final String FTP_POST_SEND_CMD_PROPERTY = "postSendCommand";
    public static final String FTP_POST_SEND_LOC_PROPERTY = "postSendLocation";
    public static final String FTP_POST_SEND_LOC_HAS_PATTS_PROPERTY = "postSendLocationHasPatterns";
    public static final String FTP_POST_RECEIVE_CMD_PROPERTY = "postReceiveCommand";
    public static final String FTP_POST_RECEIVE_LOC_PROPERTY = "postReceiveLocation";
    public static final String FTP_POST_RECEIVE_LOC_HAS_PATTS_PROPERTY = "postReceiveLocationHasPatterns";
//    public static final String FTP_SENDER_USEPASSIVE_PROPERTY = "senderUsePassive";
//    public static final String FTP_RECEIVER_USEPASSIVE_PROPERTY = "receiverUsePassive";
    private String mUse = "literal";
    private String mEncodingStyle = "";
    private String mPart = "";
    private String mSendTo = "";
    private boolean mSendToHasPatterns = false;
    private boolean mAppend;
    private String mReceiveFrom = "";
    private boolean mReceiveFromHasRegexs = false;
    private int mPollInterval = 5000;
    private String mPollIntervalStr;
    private String mPreSendCmd;
    private String mPreSendLoc = "";
    private boolean mPreSendLocHasPatterns = false;
    private String mPreReceiveCmd;
    private String mPreReceiveLoc = "";
    private boolean mPreReceiveLocHasPatterns = false;
    private String mPostSendCmd;
    private String mPostSendLoc = "";
    private boolean mPostSendLocHasPatterns = false;
    private String mPostReceiveCmd;
    private String mPostReceiveLoc = "";
    private boolean mPostReceiveLocHasPatterns = false;
//    private boolean mSenderUsePassive = true;
//    private boolean mReceiverUsePassive = true;
    private boolean mMsgCorrelateEnabled = false; // default to false
    private boolean bForwardAttachment = false; // default to false
    private String mFileType;
    private String mCharacterEncoding;

    public FTPTransfer() {
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    //<ftp:transfer use="literal"
    public String getUse() {
        return this.mUse;
    }

    public void setUse(String use) {
        this.mUse = use;
    }

    //<ftp:transfer encodingStyle=""
    public String getEncodingStyle() {
        return this.mEncodingStyle;
    }

    public void setEncodingStyle(String encodingStyle) {
        this.mEncodingStyle = encodingStyle;
    }

    public String getPart() {
        return this.mPart;
    }

    public void setPart(String part) {
        this.mPart = part;
    }

    public String getSendTo() {
        return mSendTo;
    }

    public void setSendTo(String s) {
        mSendTo = s;
    }

    public boolean getSendToHasPatterns() {
        return mSendToHasPatterns;
    }

    public void setSendToHasPatterns(boolean b) {
        mSendToHasPatterns = b;
    }

    public boolean getAppend() {
        return mAppend;
    }

    public void setAppend(boolean b) {
        mAppend = b;
    }

    public String getReceiveFrom() {
        return mReceiveFrom;
    }

    public void setReceiveFrom(String s) {
        mReceiveFrom = s;
    }

    public boolean getReceiveFromHasRegexs() {
        return mReceiveFromHasRegexs;
    }

    public void setReceiveFromHasRegexs(boolean b) {
        mReceiveFromHasRegexs = b;
    }

    public String getPollIntervalStr() {
        return this.mPollIntervalStr;
    }

    public void setPollIntervalStr(String interval) {
        this.mPollIntervalStr = interval;
    }

    public int getPollInterval() {
        return this.mPollInterval;
    }

    public void setPollInterval(int interval) {
        this.mPollInterval = interval;
    }

    public String getPreSendCommand() {
        return mPreSendCmd;
    }

    public void setPreSendCommand(String s) {
        mPreSendCmd = s;
    }

    public String getPreSendLoc() {
        return mPreSendLoc;
    }

    public void setPreSendLoc(String s) {
        mPreSendLoc = s;
    }

    public boolean getPreSendLocHasPatterns() {
        return mPreSendLocHasPatterns;
    }

    public void setPreSendLocHasPatterns(boolean b) {
        mPreSendLocHasPatterns = b;
    }

    public String getPreReceiveCommand() {
        return mPreReceiveCmd;
    }

    public void setPreReceiveCommand(String s) {
        mPreReceiveCmd = s;
    }

    public String getPreReceiveLoc() {
        return mPreReceiveLoc;
    }

    public void setPreReceiveLoc(String s) {
        this.mPreReceiveLoc = s;
    }

    public boolean getPreReceiveLocHasPatterns() {
        return mPreReceiveLocHasPatterns;
    }

    public void setPreReceiveLocHasPatterns(boolean b) {
        mPreReceiveLocHasPatterns = b;
    }

    public String getPostSendCommand() {
        return mPostSendCmd;
    }

    public void setPostSendCommand(String s) {
        mPostSendCmd = s;
    }

    public String getPostSendLoc() {
        return mPostSendLoc;
    }

    public void setPostSendLoc(String s) {
        mPostSendLoc = s;
    }

    public boolean getPostSendLocHasPatterns() {
        return mPostSendLocHasPatterns;
    }

    public void setPostSendLocHasPatterns(boolean s) {
        mPostSendLocHasPatterns = s;
    }

    public String getPostReceiveCommand() {
        return mPostReceiveCmd;
    }

    public void setPostReceiveCommand(String s) {
        mPostReceiveCmd = s;
    }

    public String getPostReceiveLoc() {
        return mPostReceiveLoc;
    }

    public void setPostReceiveLoc(String s) {
        mPostReceiveLoc = s;
    }

    public boolean getPostReceiveLocHasPatterns() {
        return mPostReceiveLocHasPatterns;
    }

    public void setPostReceiveLocHasPatterns(boolean b) {
        mPostReceiveLocHasPatterns = b;
    }

//    public boolean getSenderUsePassive() {
//        return this.mSenderUsePassive;
//    }
//    
//    public void setSenderUsePassive(boolean usePassive) {
//        this.mSenderUsePassive = usePassive;
//    }
//
//    public boolean getReceiverUsePassive() {
//        return this.mReceiverUsePassive;
//    }
//    
//    public void setReceiverUsePassive(boolean usePassive) {
//        this.mReceiverUsePassive = usePassive;
//    }
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nFTP Transfer (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("Use =" + mUse);
        strBuf.append("Part =" + mPart);
        strBuf.append("EncodingStyle =" + mEncodingStyle);
        strBuf.append("SendTo =" + mSendTo);
        strBuf.append("SendToHasPatterns =" + mSendToHasPatterns);
        strBuf.append("Append =" + mAppend);
        strBuf.append("ReceiveFrom =" + mReceiveFrom);
        strBuf.append("ReceiveFromHasRegexs =" + mReceiveFromHasRegexs);
        strBuf.append("PollInterval =" + mPollIntervalStr);
        strBuf.append("PreSendCmd =" + mPreSendCmd);
        strBuf.append("PreSendLoc =" + mPreSendLoc);
        strBuf.append("PreSendLocHasPatterns =" + mPreSendLocHasPatterns);
        strBuf.append("PreReceiveCmd =" + mPreReceiveCmd);
        strBuf.append("PreReceiveLoc =" + mPreReceiveLoc);
        strBuf.append("PreReceiveLocHasPatterns =" + mPreReceiveLocHasPatterns);
        strBuf.append("PostSendCmd =" + mPostSendCmd);
        strBuf.append("PostSendLoc =" + mPostSendLoc);
        strBuf.append("PostSendLocHasPatterns =" + mPostSendLocHasPatterns);
        strBuf.append("PostReceiveCmd =" + mPostReceiveCmd);
        strBuf.append("PostReceiveLoc =" + mPostReceiveLoc);
        strBuf.append("PostReceiveLocHasPatterns =" + mPostReceiveLocHasPatterns);
//        strBuf.append("SenderUsePassive =" + mSenderUsePassive);
//        strBuf.append("ReceiverUsePassive =" + mReceiverUsePassive);
        return strBuf.toString();
    }

    public Object[] getAttributes() {
        Object[] attrs = new Object[21];
        attrs[0] = getSendTo();
        attrs[1] = new Boolean(getSendToHasPatterns());
        attrs[2] = new Boolean(getAppend());
        attrs[3] = getReceiveFrom();
        attrs[4] = new Boolean(getReceiveFromHasRegexs());
        attrs[5] = this.getPollIntervalStr();
        attrs[6] = getPreSendCommand();
        attrs[7] = getPreSendLoc();
        attrs[8] = new Boolean(getPreSendLocHasPatterns());
        attrs[9] = getPreReceiveCommand();
        attrs[10] = getPreReceiveLoc();
        attrs[11] = new Boolean(getPreReceiveLocHasPatterns());
        attrs[12] = getPostSendCommand();
        attrs[13] = getPostSendLoc();
        attrs[14] = new Boolean(getPostSendLocHasPatterns());
        attrs[15] = getPostReceiveCommand();
        attrs[16] = getPostReceiveLoc();
        attrs[17] = new Boolean(getPostReceiveLocHasPatterns());
        attrs[18] = this.getUse();
        attrs[19] = this.getEncodingStyle();
        attrs[20] = this.getPart();
//        attrs[21] = this.getSenderUsePassive();
//        attrs[22] = this.getReceiverUsePassive();
        return attrs;
    }

    public void validate(String operationName) throws Exception {
        // sendTo and receiveFrom can not both be blank 
        String sendTo = getSendTo();
        String receiveFrom = getReceiveFrom();
        if ((sendTo == null || sendTo.trim().length() == 0) && (receiveFrom == null || receiveFrom.trim().length() == 0)) {
            throw new Exception(mMessages.getString("FTPBC-E001021.FTPTransfer.BOTH_SENDTO_AND_RECEIVEFROM_ARE_NOT_SPECIFIED", operationName));
        }

        // when use=encoded, encoding style is required
        if (mUse != null && mUse.equals(FTPConstants.EXT_ELEM_ATTR_USE_ENCODED) &&
                (this.getEncodingStyle() == null ||
                this.getEncodingStyle().trim().length() == 0)) {
            throw new Exception(mMessages.getString("FTPBC-E001022.FTPTransfer.NO_ENCODING_STYLE_SPECIFIED", operationName));
        }

        if (sendTo != null && sendTo.trim().length() > 0) {
            // must be a file instead of directory
            if (sendTo.endsWith("/")) {
                throw new Exception(mMessages.getString("FTPBC-E001023.FTPTransfer.A_PATH_POINTING_TO_FILE_REQUIRED", new Object[]{"sendTo", sendTo}));
            }
            // validate sendTo related stuff
            if (getPreSendCommand() != null && (getPreSendCommand().equals("RENAME") || getPreSendCommand().equals("COPY"))) {
                if (getPreSendLoc() == null || getPreSendLoc().trim().length() == 0) {
                    throw new Exception(mMessages.getString("FTPBC-E001024.FTPTransfer.PRE_POST_OPERATION_WO_LOCATION", new Object[]{}));
                }
                if (getPreSendLoc().endsWith("/")) {
                    throw new Exception(mMessages.getString("FTPBC-E001023.FTPTransfer.A_PATH_POINTING_TO_FILE_REQUIRED", new Object[]{"preSendLocation", getPreSendLoc()}));
                }
            }
            if (getPostSendCommand() != null && (getPostSendCommand().equals("RENAME") || getPostSendCommand().equals("COPY"))) {
                if (getPostSendLoc() == null) {
                    throw new Exception(mMessages.getString("FTPBC-E001024.FTPTransfer.PRE_POST_OPERATION_WO_LOCATION", new Object[]{}));
                }
                if (getPostSendLoc().endsWith("/")) {
                    throw new Exception(mMessages.getString("FTPBC-E001023.FTPTransfer.A_PATH_POINTING_TO_FILE_REQUIRED", new Object[]{"postSendLocation", getPostSendLoc()}));
                }
            }
        }

        if (receiveFrom != null && receiveFrom.trim().length() > 0) {
            // must be a file instead of directory
            if (receiveFrom.endsWith("/")) {
                throw new Exception(mMessages.getString("FTPBC-E001023.FTPTransfer.A_PATH_POINTING_TO_FILE_REQUIRED", new Object[]{"receiveFrom", receiveFrom}));
            }
            // validate receiveFrom related stuff
            if (getPreReceiveCommand() != null && (getPreReceiveCommand().equals("RENAME") || getPreReceiveCommand().equals("COPY"))) {
                if (getPreReceiveLoc() == null || getPreReceiveLoc().trim().length() == 0) {
                    throw new Exception(mMessages.getString("FTPBC-E001024.FTPTransfer.PRE_POST_OPERATION_WO_LOCATION", new Object[]{}));
                }
                if (getPreReceiveLoc().endsWith("/")) {
                    throw new Exception(mMessages.getString("FTPBC-E001023.FTPTransfer.A_PATH_POINTING_TO_FILE_REQUIRED", new Object[]{"preReceiveLocation", getPreReceiveLoc()}));
                }
            }
            if (getPostReceiveCommand() != null && (getPostReceiveCommand().equals("RENAME") || getPostReceiveCommand().equals("COPY"))) {
                if (getPostReceiveLoc() == null) {
                    throw new Exception(mMessages.getString("FTPBC-E001024.FTPTransfer.PRE_POST_OPERATION_WO_LOCATION", new Object[]{}));
                }
                if (getPostReceiveLoc().endsWith("/")) {
                    throw new Exception(mMessages.getString("FTPBC-E001023.FTPTransfer.A_PATH_POINTING_TO_FILE_REQUIRED", new Object[]{"postReceiveLocation", getPostReceiveLoc()}));
                }
            }
        }

        if (getPollIntervalStr() != null && getPollIntervalStr().trim().length() > 0) {
            int interval = getInt(getPollIntervalStr());
            if (interval <= 0) {

                throw new Exception(mMessages.getString("FTPBC-E001025.FTPTransfer.POLL_INTERVAL_INVALID", new Object[]{getPollIntervalStr()}));
            }
            setPollInterval(interval);
        }
    }

    private int getInt(String t) {
        int val = -1;
        if (t != null && t.trim().length() > 0) {
            try {
                val = Integer.parseInt(t);
            } catch (Exception e) {
            }
        }
        return val;
    }

    public boolean getMessageCorrelate() {
        return mMsgCorrelateEnabled;
    }

    public void setMessageCorrelate(boolean b) {
        mMsgCorrelateEnabled = b;
    }

    public String getMessageName() {
        return "%u";
    }

    public void setMessageName(String s) {
        // can throw invalid op exception
    }

    public String getMessageNamePrefixIB() {
        return FTPConstants.MSG_CORRELATE_REQ_PREFIX;
    }

    public void setMessageNamePrefixIB(String s) {
        // can throw invalid op exception
    }

    public String getMessageNamePrefixOB() {
        return FTPConstants.MSG_CORRELATE_RESP_PREFIX;
    }

    public void setMessageNamePrefixOB(String s) {
        // can throw invalid op exception
    }

    public String getFileType() {
        return mFileType;
    }

    public void setFileType(String s) {
        mFileType = s;
    }

    public boolean getForwardAsAttachment() {
        return bForwardAttachment;
    }

    public void setForwardAsAttachment(boolean b) {
        bForwardAttachment = b;
    }

    public String getCharacterEncoding() {
        return mCharacterEncoding;
    }

    public void setCharacterEncoding(String s) {
        mCharacterEncoding = s;
    }
}
