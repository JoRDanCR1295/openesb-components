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
 * @(#)FTPAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

import com.sun.jbi.ftpbc.ftp.FtpFileConfigConstants;
import com.sun.jbi.internationalization.Messages;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import java.io.Serializable;
import java.nio.charset.Charset;

/**
 *
 * @author jfu
 */
public class FTPAddress implements ExtensibilityElement, Serializable {

    private static Messages mMessages =
            Messages.getMessages(FTPAddress.class);
    public static final String ADDR_URL_PROPERTY = "url";
    public static final String ADDR_USER_PROPERTY = "user";
    public static final String ADDR_PASSWORD_PROPERTY = "password";
    public static final String FTP_DIRLSTSTYLE_PROPERTY = "dirListStyle";
    public static final String FTP_USE_UD_HEURISTICS_PROPERTY = "useUserDefinedHeuristics";
    public static final String FTP_UD_DIRLSTSTYLE_PROPERTY = "userDefDirListStyle";
    public static final String FTP_UD_HEURISTICS_PROPERTY = "userDefDirListHeuristics";
    public static final String FTP_TRANSMODE_PROPERTY = "mode";
    public static final String FTP_CMD_CH_TIMEOUT_PROPERTY = "cmdChannelTimeout";
    public static final String FTP_DATA_CH_TIMEOUT_PROPERTY = "dataChannelTimeout";
    public static final String FTP_CNTRL_CH_ENCODING_PROPERTY = "controlChannelncoding";
    public static final String FTP_PERSIST_BASE_LOC_PROPERTY = "baseLocation";
    // new attrs for FTP/TLS 
    public static final String FTP_SEC_TYPE_PROPERTY = "securedFTP";
    public static final String FTP_ENABLE_CCC_PROPERTY = "enableCCC";
    public static final String FTP_KSTOR_PROPERTY = "keyStore";
    public static final String FTP_KSTOR_PASSWD_PROPERTY = "keyStorePassword";
    public static final String FTP_KEY_ALIAS_PROPERTY = "keyAlias";
    public static final String FTP_KEY_PASSWD_PROPERTY = "keyPassword";
    public static final String FTP_TSTOR_PROPERTY = "trustStore";
    public static final String FTP_TSTOR_PASSWD_PROPERTY = "trustStorePassword";
    public static final String DEFAULT_CONTROL_ENCODING = "ISO-8859-1";
    private static final long serialVersionUID = 1L;
    QName fieldElementType = FTPConstants.QNAME_ADDRESS;
    Boolean fieldRequired = null;
    String mURLStr;
    FTPAddressURL mURL;
    private String mUser;
    private String mPassword;
    private String mListStyle = "UNIX";
    private boolean mUseUDHeuristics = false;
    private String mUDListStyle = "";
    private String mUDListStyleConfigLoc = "";
    private String mTransferMode = "Binary";
    private int mCmdChannelTimeout = 45000;
    private int mDataChannelTimeout = 45000;
    private String mCmdChannelTimeoutStr = "45000";
    private String mDataChannelTimeoutStr = "45000";
    private String mCntrlChannelEncoding;
    private String mPersistenceBaseLocation;
    private String mSecureFTP = FtpFileConfigConstants.FTP_SECURE_NONE;
    private boolean mEnableCCC;
    private String mKeyStore;
    private String mKeyStorePassword;
    private String mKeyAlias;
    private String mKeyPassword;
    private String mTrustStore;
    private String mTrustStorePassword;

    /**
     * 
     */
    public FTPAddress() {
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

    /**
     * 
     * @return 
     */
    public String getURL() {
        return mURLStr;
    }

    /**
     * 
     * @param val 
     */
    public void setURL(String val) {
        mURLStr = val;
    }

    /**
     * 
     * @throws java.lang.Exception 
     */
    public void parse() throws Exception {
        mURL = new FTPAddressURL(mURLStr);
        mURL.parse();
    }

    /**
     * 
     * @return 
     */
    public FTPAddressURL getFTPURL() {
        return mURL;
    }

    /**
     * 
     * @return 
     */
    public String getDirListStyle() {
        return this.mListStyle;
    }

    /**
     * 
     * @param style 
     */
    public void setDirListStyle(String style) {
        this.mListStyle = style;
    }

    /**
     * 
     * @return 
     */
    public boolean getUseUserDefinedHeuristics() {
        return this.mUseUDHeuristics;
    }

    /**
     * 
     * @param useUserDefined 
     */
    public void setUseUserDefinedHeuristics(boolean useUserDefined) {
        this.mUseUDHeuristics = useUserDefined;
    }

    /**
     * 
     * @return 
     */
    public String getUserDefDirListStyle() {
        return this.mUDListStyle;
    }

    /**
     * 
     * @param style 
     */
    public void setUserDefDirListStyle(String style) {
        this.mUDListStyle = style;
    }

    /**
     * 
     * @return 
     */
    public String getUserDefDirListHeuristics() {
        return this.mUDListStyleConfigLoc;
    }

    /**
     * 
     * @param heuristicsLoc 
     */
    public void setUserDefDirListHeuristics(String heuristicsLoc) {
        this.mUDListStyleConfigLoc = heuristicsLoc;
    }

    /**
     * 
     * @return 
     */
    public String getTransferMode() {
        return this.mTransferMode;
    }

    /**
     * 
     * @param mode 
     */
    public void setTransferMode(String mode) {
        this.mTransferMode = mode;
    }

    /**
     * 
     * @return 
     */
    public int getCmdChannelTimeout() {
        return mCmdChannelTimeout;
    }

    /**
     * 
     * @param i 
     */
    public void setCmdChannelTimeout(int i) {
        mCmdChannelTimeout = i;
    }

    /**
     * 
     * @return 
     */
    public int getDataChannelTimeout() {
        return mDataChannelTimeout;
    }

    /**
     * 
     * @param i 
     */
    public void setDataChannelTimeout(int i) {
        mDataChannelTimeout = i;
    }

    /**
     * 
     * @return 
     */
    public String getCmdChannelTimeoutStr() {
        return mCmdChannelTimeoutStr;
    }

    /**
     * 
     * @param s 
     */
    public void setCmdChannelTimeoutStr(String s) {
        mCmdChannelTimeoutStr = s;
    }

    /**
     * 
     * @return 
     */
    public String getControlChannelEncoding() {
        return mCntrlChannelEncoding;
    }

    /**
     * 
     * @param s 
     */
    public void setControlChannelEncoding(String s) {
        if (s == null || s.trim().length() == 0) {
            // default to ISO-8859-1
            mCntrlChannelEncoding = DEFAULT_CONTROL_ENCODING;
        } else {
            if (Charset.isSupported(s)) {
                mCntrlChannelEncoding = s;
            } else {
                throw new IllegalArgumentException("Invalid encoding when setting Charset to [" + s + "], not supported");
            }
        }
    }

    /**
     * 
     * @return 
     */
    public String getDataChannelTimeoutStr() {
        return mDataChannelTimeoutStr;
    }

    /**
     * 
     * @param s 
     */
    public void setDataChannelTimeoutStr(String s) {
        mDataChannelTimeoutStr = s;
    }

    /**
     * 
     * @return 
     */
    public String getUser() {
        return mUser;
    }

    /**
     * 
     * @param val 
     */
    public void setUser(String val) {
        mUser = val;
    }

    /**
     * 
     * @return 
     */
    public String getPassword() {
        return mPassword;
    }

    /**
     * 
     * @param val 
     */
    public void setPassword(String val) {
        mPassword = val;
    }

    // new setters/getters for FTP/TLS
    public String getSecureFTPType() {
        return mSecureFTP;
    }

    public void setSecureFTPType(String s) {
        mSecureFTP = s;
    }

    public boolean getEnableCCC() {
        return mEnableCCC;
    }

    public void setEnableCCC(boolean b) {
        mEnableCCC = b;
    }

    public String getKeyStore() {
        return mKeyStore;
    }

    public void setKeyStore(String s) {
        mKeyStore = s;
    }

    public String getKeyStorePassword() {
        return mKeyStorePassword;
    }

    public void setKeyStorePassword(String s) {
        mKeyStorePassword = s;
    }

    public String getKeyAlias() {
        return mKeyAlias;
    }

    public void setKeyAlias(String s) {
        mKeyAlias = s;
    }

    public String getKeyPassword() {
        return mKeyPassword;
    }

    public void setKeyPassword(String s) {
        mKeyPassword = s;
    }

    public String getTrustStore() {
        return mTrustStore;
    }

    public void setTrustStore(String s) {
        mTrustStore = s;
    }

    public String getTrustStorePassword() {
        return mTrustStorePassword;
    }

    public void setTrustStorePassword(String s) {
        mTrustStorePassword = s;
    }

    public String getPersistenceBaseLocation() {
        return mPersistenceBaseLocation;
    }

    public void setPersistenceBaseLocation(String s) {
        mPersistenceBaseLocation = s;
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nFTP address (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nURL=" + mURLStr);
        strBuf.append("\nUser=" + mUser);
        strBuf.append("\nPassword=" + mPassword);
        strBuf.append("\nListStyle =" + mListStyle);
        strBuf.append("\nUseUDHeuristics =" + mUseUDHeuristics);
        strBuf.append("\nUDListStyle =" + mUDListStyle);
        strBuf.append("\nUDListStyleConfigLoc =" + mUDListStyleConfigLoc);
        strBuf.append("\nTransferMode =" + mTransferMode);
        strBuf.append("\nCmdChannelTimeout =" + mCmdChannelTimeoutStr);
        strBuf.append("\nDataChannelTimeout =" + mDataChannelTimeoutStr);
        strBuf.append("\nControlChannelEncoding =" + mCntrlChannelEncoding);
        strBuf.append("\nPersistenceBaseLocation =" + mPersistenceBaseLocation);
        strBuf.append("\nSecuredFTP =" + mSecureFTP);
        strBuf.append("\nEnableCCC =" + mEnableCCC);
        strBuf.append("\nKeyStore =" + mKeyStore);
        strBuf.append("\nKeyStorePassword =" + mKeyStorePassword);
        strBuf.append("\nKeyAlias =" + mKeyAlias);
        strBuf.append("\nKeyPassword =" + mKeyPassword);
        strBuf.append("\nTrustStore =" + mTrustStore);
        strBuf.append("\nTrustStorePassword =" + mTrustStorePassword);

        return strBuf.toString();
    }

    /**
     * 
     * @param operationName 
     * @throws java.lang.Exception 
     */
    public void validate(QName operationName) throws Exception {
        if (this.mURL == null) {
            // missing ftp url
        }

        this.mURL.validate();
        // when useUserDefinedHeuristics=true, userDefinedHeuristi
        if (getUseUserDefinedHeuristics() &&
                (getUserDefDirListHeuristics() == null || getUserDefDirListHeuristics().trim().length() == 0 || getUserDefDirListStyle() == null || getUserDefDirListStyle().trim().length() == 0)) {
            throw new Exception(mMessages.getString("FTPBC-E001002.FTPAddress.MISSING_UD_DIR_LIST", operationName.toString()));
        }

        String t = getCmdChannelTimeoutStr();

        int timeout = -1;

        if (t != null && t.trim().length() > 0) {
            timeout = getInt(t);
            if (timeout < 0) {
                throw new Exception(mMessages.getString("FTPBC-E001003.FTPAddress.INVALID_FTP_CMD_CH_TIMEOUT",
                        new Object[]{t, operationName.toString()}));
            }
        }

        t = getDataChannelTimeoutStr();

        timeout = -1;

        if (t != null && t.trim().length() > 0) {
            timeout = getInt(t);
            if (timeout < 0) {
                throw new Exception(mMessages.getString("FTPBC-E001004.FTPAddress.INVALID_FTP_DATA_CH_TIMEOUT",
                        new Object[]{t, operationName.toString()}));
            }
        }

        // validate FTP/TLS if it is selected
        t = getSecureFTPType();
        if (t != null && t.trim().length() > 0) {
            if (t.equals(FtpFileConfigConstants.FTP_SECURE_EXPLICITSSL) || t.equals(FtpFileConfigConstants.FTP_SECURE_IMPLICITSSL)) {
                // SSL requires keystore info
                if ((getKeyStore() == null || getKeyStore().trim().length() == 0) && (getTrustStore() == null || getTrustStore().trim().length() == 0)) {
                    // when both k store and t store are blank, there is no way
                    // to do SSL
                    throw new Exception(mMessages.getString("FTPBC-E001052.FTPAddress.REQUIRE_KEYSTORE_INFO",
                            new Object[]{t, operationName.toString()}));
                }
            }
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
}
