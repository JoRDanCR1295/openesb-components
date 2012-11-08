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
 * @(#)FTPMessage.java 
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
public class FTPMessage implements ExtensibilityElement, FTPMessageExtension, Serializable {

    private static final long serialVersionUID = 1L;
    private static Messages mMessages =
            Messages.getMessages(FTPMessage.class);
    private static Logger mLogger = Messages.getLogger(FTPMessage.class);
    QName fieldElementType = FTPConstants.QNAME_MESSAGE;
    Boolean fieldRequired = Boolean.FALSE;
    private String mUse = "literal";
    private String mEncodingStyle = "";
    private String mPart = "";
    private String mMessageRepo;
    private String mMessageName = "%u";
    private String mMessageNamePrefixIB = FTPConstants.MSG_CORRELATE_REQ_PREFIX;
    private String mMessageNamePrefixOB = FTPConstants.MSG_CORRELATE_RESP_PREFIX;
    private boolean mProtectEnabled = true;
    private boolean mArchiveEnabled = true;
    private boolean mStagingEnabled = true;
    private boolean mMsgCorrelateEnabled = true;
    private int mPollInterval;
    private boolean bForwardAttachment = false;
    private String mFileType;
    private String mCharacterEncoding;

    public FTPMessage() {
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

    public int getPollInterval() {
        return this.mPollInterval;
    }

    public void setPollInterval(int interval) {
        this.mPollInterval = interval;
    }

    public String getMessageRepo() {
        return mMessageRepo;
    }

    public void setMessageRepo(String s) {
        mMessageRepo = s;
    }

    public boolean getProtectEnabled() {
        return mProtectEnabled;
    }

    public void setProtectEnabled(boolean b) {
        mProtectEnabled = b;
    }

    public boolean getArchiveEnabled() {
        return mArchiveEnabled;
    }

    public void setArchiveEnabled(boolean b) {
        mArchiveEnabled = b;
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nFTPMessage (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nMessageRepository =" + mMessageRepo);
        strBuf.append("\nMessageName =" + mMessageName);
        strBuf.append("\nMessageNamePrefixIB =" + mMessageNamePrefixIB);
        strBuf.append("\nMessageNamePrefixOB =" + mMessageNamePrefixOB);
        strBuf.append("\nMessageCorrelate =" + mMsgCorrelateEnabled);
        strBuf.append("\nProtect =" + mProtectEnabled);
        strBuf.append("\nArchive =" + mArchiveEnabled);
        strBuf.append("\nStage =" + mStagingEnabled);
        strBuf.append("\nPollInterval =" + mPollInterval);
        strBuf.append("\nUse =" + mUse);
        strBuf.append("\nEncodingStyle =" + mEncodingStyle);
        strBuf.append("\nPart =" + mPart);
        return strBuf.toString();
    }

    public Object[] getAttributes() {
        Object[] attrs = new Object[12];
        attrs[0] = getMessageRepo();
        attrs[1] = getMessageName();
        attrs[2] = getMessageNamePrefixIB();
        attrs[3] = getMessageNamePrefixOB();
        attrs[4] = getMessageCorrelate();
        attrs[5] = getProtectEnabled();
        attrs[6] = getArchiveEnabled();
        attrs[7] = getStagingEnabled();
        attrs[8] = new Integer(this.getPollInterval());
        attrs[9] = getUse();
        attrs[10] = getEncodingStyle();
        attrs[11] = getPart();
        return attrs;
    }

    public void validate(String operationName) throws Exception {
        // messageRepo can not be blank
        if (getMessageRepo() == null || getMessageRepo().trim().length() == 0) {
            throw new Exception(mMessages.getString("FTPBC-E001018.FTPMessage.MSG_REPO_ARE_NOT_SPECIFIED", operationName));
        }

        // when use=encoded, encoding style is required
        if (mUse != null && mUse.equals(FTPConstants.EXT_ELEM_ATTR_USE_ENCODED) &&
                (this.getEncodingStyle() == null ||
                this.getEncodingStyle().trim().length() == 0)) {
            throw new Exception(mMessages.getString("FTPBC-E001019.FTPMessage.NO_ENCODING_STYLE_SPECIFIED", operationName));
        }
        if (getMessageNamePrefixIB() != null && getMessageNamePrefixIB().trim().length() > 0) {
            // IB prefix should not contain pattern symbol
            if (getMessageNamePrefixIB().indexOf("%") >= 0) {
                throw new Exception(mMessages.getString("FTPBC-E001020.FTPMessage.MSG_NAME_HAS_PATTERN_SYMB", new Object[]{getMessageNamePrefixIB(), operationName}));
            }
        }
        if (getMessageNamePrefixOB() != null && getMessageNamePrefixOB().trim().length() > 0) {
            // OB prefix should not contain pattern symbol
            if (getMessageNamePrefixOB().indexOf("%") >= 0) {
                throw new Exception(mMessages.getString("FTPBC-E001020.FTPMessage.MSG_NAME_HAS_PATTERN_SYMB", new Object[]{getMessageNamePrefixOB(), operationName}));
            }
        }
    }

    public boolean getMessageCorrelate() {
        return mMsgCorrelateEnabled;
    }

    public void setMessageCorrelate(boolean b) {
        mMsgCorrelateEnabled = b;
    }

    public boolean getStagingEnabled() {
        return mStagingEnabled;
    }

    public void setStagingEnabled(boolean b) {
        mStagingEnabled = b;
    }

    public String getMessageName() {
        return mMessageName;
    }

    public void setMessageName(String s) {
        if (s != null && s.length() > 0) {
            mMessageName = s;
        }
    }

    public String getMessageNamePrefixIB() {
        return mMessageNamePrefixIB;
    }

    public void setMessageNamePrefixIB(String s) {
        if (s != null && s.length() > 0) {
            mMessageNamePrefixIB = s;
        }
    }

    public String getMessageNamePrefixOB() {
        return mMessageNamePrefixOB;
    }

    public void setMessageNamePrefixOB(String s) {
        if (s != null && s.length() > 0) {
            mMessageNamePrefixOB = s;
        }
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
