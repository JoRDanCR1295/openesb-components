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
 * @(#)FtpClientParameterUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc;

import com.sun.jbi.ftpbc.ftp.FtpFileConfigConstants;
import com.sun.jbi.ftpbc.extensions.FTPAddress;
import com.sun.jbi.ftpbc.extensions.FTPConstants;
import com.sun.jbi.ftpbc.extensions.FTPMessageExtension;
import com.sun.jbi.ftpbc.extensions.FTPTransfer;
import com.sun.jbi.ftpbc.extensions.FTPTransferExtension;
import com.sun.jbi.ftpbc.extensions.ProxyAddressURL;
import com.sun.jbi.ftpbc.ftp.FtpFileConfiguration;
import com.sun.jbi.ftpbc.ftp.namepattern.NamePattern;
import com.sun.jbi.internationalization.Messages;

import java.util.Properties;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import javax.xml.namespace.QName;

/**
 *
 * @author jfu
 */
public class FtpClientParameterGenerator {
    // extensibility element attribute names <==> ftp client parameters mapping
    // ftp:message
    // ftp:transfer
    // ftp:address

    private static final String CONST_NO = "No";
    private static final String CONST_YES = "Yes";
    private static final String TRANS_MODE_ASCII = "Ascii";
    private static final String TRANS_MODE_EBCDIC = "Ebcdic";
    private static final String TRANS_MODE_BINARY = "Binary";

    private FtpClientParameterGenerator() {
    }

    /**
     * isSender - true, will only do pre operation
     * false - will only do post operations
     */
    public static Properties createProperties(
            boolean isSender,
            Properties properties,
            FTPAddress address,
            FTPTransferExtension extElem,
            ProxyAddressURL proxy,
            QName operation,
            String uuid,
            boolean isConsumer,
            Messages mMessages,
            Logger mLogger) throws Exception {
        Properties prop = properties;
        prop.put(FtpFileConfigConstants.P_COLLAB_OID, "placeholder");
        prop.put(FtpFileConfigConstants.P_CONN_NAME, "placeholder");
        // section "General Settings"
        prop.put(FtpFileConfigConstants.P_GEN_TRNS_TYPE, "Non-Transactional");
        prop.put(FtpFileConfigConstants.P_GEN_BASE_LOC, "");
        prop.put(FtpFileConfigConstants.C_P_GEN_SYNC, CONST_NO);
        // section "FTP"
        prop.put(FtpFileConfigConstants.C_P_FTP_LST_STYLE, address.getDirListStyle() != null ? address.getDirListStyle() : "UNIX");
        prop.put(FtpFileConfigConstants.C_P_FTP_HOST, address.getFTPURL() != null && address.getFTPURL().getHost() != null ? address.getFTPURL().getHost() : "");
        prop.put(FtpFileConfigConstants.C_P_FTP_USR, address.getFTPURL() != null && address.getFTPURL().getUser() != null ? address.getFTPURL().getUser() : "anonymous");
        prop.put(FtpFileConfigConstants.C_P_FTP_PASSWD, address.getFTPURL() != null && address.getFTPURL().getPassword() != null ? address.getFTPURL().getPassword() : "");
        if (!address.getUseUserDefinedHeuristics()) {
            prop.put(FtpFileConfigConstants.C_P_FTP_UDH_CFG, "");
            prop.put(FtpFileConfigConstants.C_P_FTP_UDH_LST_STYLE, "");
        } else {
            prop.put(FtpFileConfigConstants.C_P_FTP_UDH_CFG, address.getUserDefDirListHeuristics() != null ? address.getUserDefDirListHeuristics() : "");
            prop.put(FtpFileConfigConstants.C_P_FTP_UDH_LST_STYLE, address.getUserDefDirListStyle() != null ? address.getUserDefDirListStyle() : "");
        }
        int port = 21;
        if (address.getFTPURL() != null && address.getFTPURL().getPort() != null && address.getFTPURL().getPort().trim().length() > 0) {
            try {
                port = Integer.parseInt(address.getFTPURL().getPort());
            } catch (Exception e) {
                // error
                throw new Exception(mMessages.getString("FTPBC-E004001.MP_Invalid_FTP_Port", address.getFTPURL().getPort()));
            }
        } else {
            // warning - default to 21
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W004001.MP_FTP_Port_default", "" + port));
            }
        }

        prop.put(FtpFileConfigConstants.C_P_FTP_PORT, new Long(port));

        if (address.getTransferMode() != null) {
            if (address.getTransferMode().equalsIgnoreCase(TRANS_MODE_ASCII)) {
                prop.put(FtpFileConfigConstants.P_FTP_MODE, TRANS_MODE_ASCII);
            } else if (address.getTransferMode().equalsIgnoreCase(TRANS_MODE_EBCDIC)) {
                prop.put(FtpFileConfigConstants.P_FTP_MODE, TRANS_MODE_EBCDIC);
            } else {
                prop.put(FtpFileConfigConstants.P_FTP_MODE, TRANS_MODE_BINARY);
            }
        } else {
            prop.put(FtpFileConfigConstants.P_FTP_MODE, TRANS_MODE_BINARY);
        }

        prop.put(FtpFileConfigConstants.C_P_SECURE_FTP_TYPE, address.getSecureFTPType() != null ? address.getSecureFTPType() : FtpFileConfigConstants.FTP_SECURE_NONE);
        prop.put(FtpFileConfigConstants.C_P_ENABLE_CCC, address.getEnableCCC() ? "true" : "false");
        prop.put(FtpFileConfigConstants.C_P_KEY_STORE_LOC, address.getKeyStore() != null ? address.getKeyStore() : "");
        prop.put(FtpFileConfigConstants.C_P_KEY_STORE_PASSWORD, address.getKeyStorePassword() != null ? address.getKeyStorePassword() : "");
        prop.put(FtpFileConfigConstants.C_P_KEY_ALIAS, address.getKeyAlias() != null ? address.getKeyAlias() : "");
        prop.put(FtpFileConfigConstants.C_P_KEY_PASSWORD, address.getKeyPassword() != null ? address.getKeyPassword() : "");
        prop.put(FtpFileConfigConstants.C_P_TRUST_STORE_LOC, address.getTrustStore() != null ? address.getTrustStore() : "");
        prop.put(FtpFileConfigConstants.C_P_TRUST_STORE_PASSWORD, address.getTrustStorePassword() != null ? address.getTrustStorePassword() : "");

        int timeout = address.getCmdChannelTimeout() > 0 ? address.getCmdChannelTimeout() : 45000;
        prop.put(FtpFileConfigConstants.P_FTP_CMD_TIMEOUT, new Long(timeout));
        timeout = address.getDataChannelTimeout() > 0 ? address.getDataChannelTimeout() : 45000;
        prop.put(FtpFileConfigConstants.P_FTP_DAT_TIMEOUT, new Long(timeout));

        prop.put(FtpFileConfigConstants.C_P_FTP_CNTRL_ENCODING, address.getControlChannelEncoding());

        String[] paths = null;
        ProxyAddressURL proxyObject = null;

        // setting and validating proxy
        if (proxy != null) {
            // proxy is parsed and validated before this point
            prop.put(FtpFileConfigConstants.C_P_SOC_ON, CONST_YES);
            if (proxy.getHost() == null) {
                throw new Exception(mMessages.getString("FTPBC-E004002.MP_Invalid_No_ProxyHost", proxyObject));
            }
            prop.put(FtpFileConfigConstants.C_P_SOC_HOST, proxy.getHost());
            int proxyPort = 1080;
            if (proxy.getPort() != null && proxy.getPort().trim().length() > 0) {
                try {
                    proxyPort = Integer.parseInt(proxy.getPort());
                } catch (Exception e) {
                    // warning - use default
                }
            }
            prop.put(FtpFileConfigConstants.C_P_SOC_PORT, new Long(proxyPort));
            prop.put(FtpFileConfigConstants.C_P_SOC_USR, proxy.getUser() != null ? proxy.getUser() : "");
            prop.put(FtpFileConfigConstants.C_P_SOC_PASSWD, proxy.getPassword() != null ? proxy.getPassword() : "");
            String socksVer = "Unknown";
            if (proxy.getScheme() != null) {
                if (proxy.getScheme().equalsIgnoreCase("socks4")) {
                    socksVer = "4";
                } else if (proxy.getScheme().equalsIgnoreCase("socks5")) {
                    socksVer = "5";
                } else {
                    throw new Exception(mMessages.getString("FTPBC-E004003.MP_Invalid_ProxyProtocol", proxy.getURL()));
                }
            }
            prop.put(FtpFileConfigConstants.C_P_SOC_VER, socksVer);
        } else {
            prop.put(FtpFileConfigConstants.C_P_SOC_ON, CONST_NO);
            prop.put(FtpFileConfigConstants.C_P_SOC_HOST, "");
            prop.put(FtpFileConfigConstants.C_P_SOC_PORT, new Long(1080));
            prop.put(FtpFileConfigConstants.C_P_SOC_USR, "");
            prop.put(FtpFileConfigConstants.C_P_SOC_PASSWD, "");
            prop.put(FtpFileConfigConstants.C_P_SOC_VER, "Unknown");
        }

        // now all the target/stage/archive dirs and files
        String targetDir = null;
        boolean targetDirIsPattern = false;
        String targetFile = null;
        boolean targetFileIsPattern = false;

        boolean stageEnabled = false;
        String stageDir = null;
        String stageFile = null;

        boolean usePassive = true;
        boolean isAppend = false;

        String preCmd = FtpFileConfiguration.CMD_NONE;
        String postCmd = FtpFileConfiguration.CMD_NONE;

        String preTargetDir = null;
        boolean preTargetDirIsPattern = false;
        String preTargetFile = null;
        boolean preTargetFileIsPattern = false;

        String postTargetDir = null;
        boolean postTargetDirIsPattern = false;
        String postTargetFile = null;
        boolean postTargetFileIsPattern = false;

        String msgName = null;
        String reqPrefix = FTPConstants.MSG_CORRELATE_REQ_PREFIX;
        String respPrefix = FTPConstants.MSG_CORRELATE_RESP_PREFIX;

        if (isSender) {
            if (extElem instanceof FTPTransfer) {
                FTPTransfer extObj = (FTPTransfer) extElem;
                //usePassive = extObj.getSenderUsePassive(); // usePassive moved into BC's runtime configuration
                // section "sendTo" and etc mapped to FTP configuration params
                if (extObj.getSendTo() == null || extObj.getSendTo().trim().length() == 0) {
                    throw new Exception(mMessages.getString("FTPBC-E004004.MP_Invalid_No_FtpTargetFile", operation));
                }

                paths = getPathComponents(extObj.getSendTo());

                if (paths == null) {
                    throw new Exception(mMessages.getString("FTPBC-E004005.MP_Invalid_FtpTarget", new Object[]{extObj.getSendTo(), operation}));
                }

                targetDir = paths[0] != null ? paths[0] : "";
                targetDirIsPattern = extObj.getSendToHasPatterns();
                targetFile = paths[1];
                targetFileIsPattern = extObj.getSendToHasPatterns();
                isAppend = extObj.getAppend();

                String fname = uuid != null ? uuid : extObj.getMessageName();
                String prefix = isConsumer ? extObj.getMessageNamePrefixIB() : extObj.getMessageNamePrefixOB();
                boolean isPattern = (uuid == null);
                // consumer sending request with a given uuid
                // provider sending reponse with a given uuid
                if (extObj.getMessageCorrelate()) {
                    // for ftp:transfer, when correlate is enabled
                    // use default message name and prefixes
                    // to enforce UUID tagging.
                    // if uuid != null -> messageCorrelate is enabled
                    // replace the file name with a generated
                    // name in tagged message name format:
                    // i.e. req.<uuid> for request and resp.<uuid>
                    // for response.
                    targetFile = prefix.concat(fname);
                    targetFileIsPattern = isPattern;
                }

                // section "Pre Transfer"
                if (extObj.getPreSendCommand() != null) {
                    if (extObj.getPreSendCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                        preCmd = FtpFileConfiguration.CMD_COPY;
                    } else if (extObj.getPreSendCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
                        preCmd = FtpFileConfiguration.CMD_RENAME;
                    }
                }

                paths = getPathComponents(extObj.getPreSendLoc());

                if (preCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME) || preCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                    if (paths == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004006.MP_Invalid_No_PreSendTargetSpec", preCmd));
                    }
                    if (extObj.getPreSendLoc() == null || extObj.getPreSendLoc().trim().length() == 0) {
                        // pre file required when pre cmd is CMD_COPY or CMD_RENAME
                        throw new Exception(mMessages.getString("FTPBC-E004006.MP_Invalid_No_PreSendTargetSpec", preCmd));
                    }
                    if (paths[0] == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004007.MP_Invalid_No_Dir_In_PreSendLoc", extObj.getPreSendLoc()));
                    }

                    if (paths[1] == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004008.MP_Invalid_No_File_In_PreSendLoc", extObj.getPreSendLoc()));
                    }

                    preTargetFile = paths[1];
                    preTargetFileIsPattern = extObj.getPreSendLocHasPatterns();
                    if (preTargetFileIsPattern && preTargetFile != null && preTargetFile.indexOf("%p") >= 0) {
                        // pre op file can not have "%p" as pattern
                        throw new Exception(mMessages.getString("FTPBC-E004054.MP_Invalid_Path_Symb_In_Pre_Post_File", preTargetFile));
                    }
                    preTargetDir = paths[0];
                    preTargetDirIsPattern = extObj.getPreSendLocHasPatterns();
                    if (extObj.getMessageCorrelate()) {
                        preTargetFile = prefix.concat(fname);
                        preTargetFileIsPattern = isPattern;
                    }
                }

                // section "Post Transfer"
                if (extObj.getPostSendCommand() != null) {
                    if (extObj.getPostSendCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_DELETE)) {
                        postCmd = FtpFileConfiguration.CMD_DELETE;
                    } else if (extObj.getPostSendCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
                        postCmd = FtpFileConfiguration.CMD_RENAME;
                    }
                }

                paths = getPathComponents(extObj.getPostSendLoc());

                if (postCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME) || postCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                    if (paths == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004009.MP_Invalid_No_PostSendTargetSpec", preCmd));
                    }

                    if (extObj.getPostSendLoc() == null || extObj.getPostSendLoc().trim().length() == 0) {
                        // post file required when post cmd is not CMD_NONE
                        throw new Exception(mMessages.getString("FTPBC-E004009.MP_Invalid_No_PostSendTargetSpec", postCmd));
                    }

                    if (paths[0] == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004010.MP_Invalid_No_Dir_In_PostSendLoc", extObj.getPostSendLoc()));
                    }

                    if (paths[1] == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004011.MP_Invalid_No_File_In_PostSendLoc", extObj.getPostSendLoc()));
                    }

                    postTargetFile = paths[1];
                    postTargetFileIsPattern = extObj.getPostSendLocHasPatterns();
                    if (postTargetFileIsPattern && postTargetFile != null && postTargetFile.indexOf("%p") >= 0) {
                        // pre op file can not have "%p" as pattern
                        throw new Exception(mMessages.getString("FTPBC-E004054.MP_Invalid_Path_Symb_In_Pre_Post_File", postTargetFile));
                    }
                    postTargetDir = paths[0];
                    postTargetDirIsPattern = extObj.getPostSendLocHasPatterns();
                    if (extObj.getMessageCorrelate()) {
                        postTargetFile = prefix.concat(fname);
                        postTargetFileIsPattern = isPattern;
                    }
                }
            } else if (extElem instanceof FTPMessageExtension) {
                FTPMessageExtension extObj = (FTPMessageExtension) extElem;
                String baseDir = extObj.getMessageRepo();
                if (baseDir == null || baseDir.trim().length() == 0) {
                    throw new Exception(mMessages.getString("FTPBC-E004012.MP_Invalid_No_MessageRepo", operation));
                }
                if (extObj.getMessageNamePrefixIB() != null && extObj.getMessageNamePrefixIB().trim().length() > 0) {
                    reqPrefix = extObj.getMessageNamePrefixIB();
                }
                if (extObj.getMessageNamePrefixOB() != null && extObj.getMessageNamePrefixOB().trim().length() > 0) {
                    respPrefix = extObj.getMessageNamePrefixOB();
                }
                if (extObj.getMessageName() != null && extObj.getMessageName().trim().length() > 0) {
                    msgName = extObj.getMessageName();
                } else {
                    msgName = "%u";
                }
                // consumer sending request
                // provider sending response
                targetDir = baseDir + (baseDir.endsWith(FTPConstants.MSG_REPO_PATH_SEP) ? "" : FTPConstants.MSG_REPO_PATH_SEP) + (isConsumer ? FTPConstants.MSG_IN_BOX : FTPConstants.MSG_OUT_BOX);
                // now, messageName, messageNamePrefix IB/OB can override
                // defaults
                String prefix = isConsumer ? reqPrefix : respPrefix;
                String fname = null;
                boolean isPattern = (uuid == null);
                if (uuid != null && extElem.getMessageCorrelate()) {
                    // uuid not NULL - must be correlate, msgName must contain %u as first chars
                    if (uuid.length() >= 36) {
                        try {
                            UUID.fromString(uuid.substring(0, 36));
                        } catch (IllegalArgumentException e) {
                            throw new Exception(mMessages.getString("FTPBC-E004013.MP_Tag_Not_Starts_With_UUID", new Object[]{uuid, msgName, operation}));
                        }
                    } else {
                        throw new Exception(mMessages.getString("FTPBC-E004014.MP_Invalid_UUID_Instance", new Object[]{uuid, msgName, operation}));
                    }
                    if (msgName.startsWith("%u")) {
                        if (msgName.length() > "u%".length()) {
                            msgName = msgName.substring("u%".length());
                            // there are extra patterns
                            if (uuid.length() > 36) {
                                fname = prefix.concat(uuid);
                            } else {
                                // extra pattern need to expand later
                                fname = prefix.concat(uuid).concat(msgName);
                                isPattern = true;
                            }
                        } else {
                            // message name is "%u" and uuid length is 36
                            if (uuid.length() == 36) {
                                fname = prefix.concat(uuid);
                            } else {
                                throw new Exception(mMessages.getString("FTPBC-E004014.MP_Invalid_UUID_Instance", new Object[]{uuid, msgName, operation}));
                            }
                        }
                    } else {
                        // uuid is not a UUID
                        throw new Exception(mMessages.getString("FTPBC-E004015.MP_Invalid_Message_Name", new Object[]{uuid, msgName, operation}));
                    }
                } else {
                    // use the msgName
                    fname = prefix.concat(msgName);
                }

                targetFile = fname;
                targetFileIsPattern = hasPattern(fname);

                // actually when it is GUID tagged message - it is unlikely to happen
                // i.e. found a file existing with the identical name
                // but just in case.
                if (extObj.getProtectEnabled()) {
                    preCmd = FtpFileConfiguration.CMD_RENAME; // move away the existing from the target (IN_BOX) -> protect area IN_PROTECT
                    preTargetDir = baseDir + (baseDir.endsWith(FTPConstants.MSG_REPO_PATH_SEP) ? "" : FTPConstants.MSG_REPO_PATH_SEP) + (isConsumer ? FTPConstants.MSG_IN_PROTECT : FTPConstants.MSG_OUT_PROTECT);
                    preTargetFile = "%f"; // keep the name
                    preTargetFileIsPattern = true;
                }

                stageEnabled = extObj.getStagingEnabled();
                if (stageEnabled) {
                    // stage file should always be a UUID
                    // whatever the message name is;
                    stageDir = baseDir + (baseDir.endsWith(FTPConstants.MSG_REPO_PATH_SEP) ? "" : FTPConstants.MSG_REPO_PATH_SEP) + (isConsumer ? FTPConstants.MSG_IN_STAGE : FTPConstants.MSG_OUT_STAGE);
                    stageFile = "%u";
                }
            } else {
                throw new Exception(mMessages.getString("FTPBC-E004016.MP_Invalid_Ext_Elem", operation));
            }
        } else {
            // receiver
            if (extElem instanceof FTPTransfer) {
                FTPTransfer extObj = (FTPTransfer) extElem;
                // usePassive = extObj.getReceiverUsePassive(); // usePassive moved into BC's runtime configuration
                // section "receiveFrom" and etc mapped to FTP configuration params;
                if (extObj.getReceiveFrom() == null || extObj.getReceiveFrom().trim().length() == 0) {
                    throw new Exception(mMessages.getString("FTPBC-E004004.MP_Invalid_No_FtpTargetFile", operation));
                }

                paths = getPathComponents(extObj.getReceiveFrom());
                if (paths == null) {
                    throw new Exception(mMessages.getString("FTPBC-E004005.MP_Invalid_FtpTarget", new Object[]{extObj.getReceiveFrom(), operation}));
                }

                targetDir = paths[0] != null ? paths[0] : "";
                targetDirIsPattern = extObj.getReceiveFromHasRegexs();
                targetFile = paths[1];
                targetFileIsPattern = extObj.getReceiveFromHasRegexs();

                // consumer receiving response using given uuid
                // provider receiving request using "%u"

                String prefix = isConsumer ? extObj.getMessageNamePrefixOB() : extObj.getMessageNamePrefixIB();
                // if uuid != null -> messageCorrelate is enabled
                // replace the file name with a generated
                // name in tagged message name format:
                // i.e. req.<uuid> for request and resp.<uuid>
                // for response.
                if (extObj.getMessageCorrelate()) {
                    if (isConsumer) {
                        if (uuid == null) {
                            throw new Exception(mMessages.getString("FTPBC-E004017.MP_No_UUID_When_Consumer_Receiving_With_MsgCorrelate"));
                        }
                        targetFile = prefix.concat(uuid);
                        targetFileIsPattern = false;
                    } else {
                        // [0-9[abcdef]]{8}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{12}
                        targetFile = "req\\.[0-9[abcdef]]{8}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{12}";
                        targetFileIsPattern = true;
                    }
                }

                // section "Pre Transfer"
                if (extObj.getPreReceiveCommand() != null) {
                    if (extObj.getPreReceiveCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                        preCmd = FtpFileConfiguration.CMD_COPY;
                    } else if (extObj.getPreReceiveCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
                        preCmd = FtpFileConfiguration.CMD_RENAME;
                    }
                }

                paths = getPathComponents(extObj.getPreReceiveLoc());

                if (preCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME) || preCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                    if (paths == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004018.MP_Invalid_No_PreReceiveTargetSpec", preCmd));
                    }

                    if (extObj.getPreReceiveLoc() == null || extObj.getPreReceiveLoc().trim().length() == 0) {
                        // pre file required when pre cmd is CMD_COPY or CMD_RENAME
                        throw new Exception(mMessages.getString("FTPBC-E004018.MP_Invalid_No_PreReceiveTargetSpec", preCmd));
                    }

                    if (paths[0] == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004019.MP_Invalid_No_Dir_In_PreReceiveLoc", extObj.getPreReceiveLoc()));
                    }

                    if (paths[1] == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004020.MP_Invalid_No_File_In_PreReceiveLoc", extObj.getPreReceiveLoc()));
                    }

                    preTargetFile = paths[1];
                    preTargetFileIsPattern = extObj.getPreReceiveLocHasPatterns();
                    if (preTargetFileIsPattern && preTargetFile != null && preTargetFile.indexOf("%p") >= 0) {
                        // pre op file can not have "%p" as pattern
                        throw new Exception(mMessages.getString("FTPBC-E004054.MP_Invalid_Path_Symb_In_Pre_Post_File", preTargetFile));
                    }
                    preTargetDir = paths[0];
                    preTargetDirIsPattern = extObj.getPreReceiveLocHasPatterns();
                    if (uuid != null) {
                        preTargetFile = prefix.concat(uuid);
                        preTargetFileIsPattern = false;
                    }
                }
                // section "Post Transfer"
                if (extObj.getPostReceiveCommand() != null) {
                    if (extObj.getPostReceiveCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_DELETE)) {
                        postCmd = FtpFileConfiguration.CMD_DELETE;
                    } else if (extObj.getPostReceiveCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
                        postCmd = FtpFileConfiguration.CMD_RENAME;
                    }
                }

                paths = getPathComponents(extObj.getPostReceiveLoc());

                if (postCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME) || postCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                    if (paths == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004021.MP_Invalid_No_PostReceiveTargetSpec", postCmd));
                    }
                    if (extObj.getPostReceiveLoc() == null || extObj.getPostReceiveLoc().trim().length() == 0) {
                        throw new Exception(mMessages.getString("FTPBC-E004021.MP_Invalid_No_PostReceiveTargetSpec", postCmd));
                    }
                    if (paths[0] == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004022.MP_Invalid_No_Dir_In_PostReceiveLoc", extObj.getPostReceiveLoc()));
                    }
                    if (paths[1] == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004023.MP_Invalid_No_File_In_PostReceiveLoc", extObj.getPostReceiveLoc()));
                    }

                    postTargetFile = paths[1];
                    postTargetFileIsPattern = extObj.getPostReceiveLocHasPatterns();
                    if (postTargetFileIsPattern && postTargetFile != null && postTargetFile.indexOf("%p") >= 0) {
                        // pre op file can not have "%p" as pattern
                        throw new Exception(mMessages.getString("FTPBC-E004054.MP_Invalid_Path_Symb_In_Pre_Post_File", postTargetFile));
                    }
                    postTargetDir = paths[0];
                    postTargetDirIsPattern = extObj.getPostReceiveLocHasPatterns();
                    if (uuid != null) {
                        postTargetFile = prefix.concat(uuid);
                        postTargetFileIsPattern = false;
                    }
                }
            } else if (extElem instanceof FTPMessageExtension) {
                FTPMessageExtension extObj = (FTPMessageExtension) extElem;
                String baseDir = extObj.getMessageRepo();
                if (baseDir == null || baseDir.trim().length() == 0) {
                    throw new Exception(mMessages.getString("FTPBC-E004012.MP_Invalid_No_MessageRepo", operation));
                }
                if (extObj.getMessageNamePrefixIB() != null && extObj.getMessageNamePrefixIB().trim().length() > 0) {
                    reqPrefix = extObj.getMessageNamePrefixIB();
                }
                if (extObj.getMessageNamePrefixOB() != null && extObj.getMessageNamePrefixOB().trim().length() > 0) {
                    respPrefix = extObj.getMessageNamePrefixOB();
                }
                if (extObj.getMessageName() != null && extObj.getMessageName().trim().length() > 0) {
                    msgName = extObj.getMessageName();
                } else {
                    msgName = "%u";
                }

                // consumer receiving response
                // provider receiving request
                targetDir = baseDir + (baseDir.endsWith(FTPConstants.MSG_REPO_PATH_SEP) ? "" : FTPConstants.MSG_REPO_PATH_SEP) + (isConsumer ? FTPConstants.MSG_OUT_BOX : FTPConstants.MSG_IN_BOX);

                String prefix = isConsumer ? respPrefix : reqPrefix;
                String fname = null;
//                boolean isPattern  = (uuid == null);
//                UUID msgID = null;
                // so far for inbound must be uuid length = 36
                if (uuid != null) {
                    // uuid not NULL - must be correlate, msgName must contain %u as first chars
                    if (uuid.length() >= 36) {
                        try {
                            //msgID = UUID.fromString(uuid.substring(0, 36));
                            UUID.fromString(uuid.substring(0, 36));
                        } catch (IllegalArgumentException e) {
                            throw new Exception(mMessages.getString("FTPBC-E004013.MP_Tag_Not_Starts_With_UUID", new Object[]{uuid, msgName, operation}));
                        }
                    } else {
                        throw new Exception(mMessages.getString("FTPBC-E004014.MP_Invalid_UUID_Instance", new Object[]{uuid, msgName, operation}));
                    }
                    if (msgName.startsWith("%u")) {
                        if (msgName.length() > "u%".length()) {
                            msgName = msgName.substring("u%".length());
                            // there are extra patterns
                            if (uuid.length() > 36) {
                                fname = prefix.concat(uuid);
                            } else {
                                // extra pattern need to expand later
                                fname = prefix.concat(uuid).concat(msgName);
                                //isPattern = true;
                            }
                        } else {
                            // message name is "%u" and uuid length is 36
                            if (uuid.length() == 36) {
                                fname = prefix.concat(uuid);
                            } else {
                                throw new Exception(mMessages.getString("FTPBC-E004014.MP_Invalid_UUID_Instance", new Object[]{uuid, msgName, operation}));
                            }
                        }
                    } else {
                        // uuid is not a UUID
                        throw new Exception(mMessages.getString("FTPBC-E004015.MP_Invalid_Message_Name", new Object[]{uuid, msgName, operation}));
                    }
                } else {
                    // use the msgName
                    fname = prefix.concat(msgName);
                }

                targetFile = fname;
                targetFileIsPattern = hasPattern(fname);

                //
                // poller staging - move the matched file 
                // to a staging dir:
                // message_repo_dir/inselect for inbound
                // message_repo_dir/outselect for outbound
                // is converted as a pre GET operation RENAME
                if (extObj.getStagingEnabled()) {
                    preCmd = FtpFileConfiguration.CMD_RENAME;
                    preTargetDir = baseDir + (baseDir.endsWith(FTPConstants.MSG_REPO_PATH_SEP) ? "" : FTPConstants.MSG_REPO_PATH_SEP) + (isConsumer ? FTPConstants.MSG_OUT_SELECTED : FTPConstants.MSG_IN_SELECTED);
                    preTargetFile = "%f";
                    preTargetFileIsPattern = true;
                }

                // move processed to an archive area
                if (extObj.getArchiveEnabled()) {
                    // use post to do staging
                    postCmd = FtpFileConfiguration.CMD_RENAME;
                    postTargetDir = baseDir + (baseDir.endsWith(FTPConstants.MSG_REPO_PATH_SEP) ? "" : FTPConstants.MSG_REPO_PATH_SEP) + (isConsumer ? FTPConstants.MSG_OUT_ARCHIVE : FTPConstants.MSG_IN_ARCHIVE);
                    postTargetFile = "%f"; // keep the name
                    postTargetFileIsPattern = true;
                } else {
                    postCmd = FtpFileConfiguration.CMD_DELETE;
                }
            } else {
                throw new Exception(mMessages.getString("FTPBC-E004016.MP_Invalid_Ext_Elem", operation));
            }
        }

        prop.put(FtpFileConfigConstants.P_STAGE_ENABLED, stageEnabled ? CONST_YES : CONST_NO);

        if (stageEnabled) {
            // sender + ftp:message etc
            if (stageDir == null || stageDir.trim().length() == 0) {
                throw new Exception(mMessages.getString("FTPBC-E004024.MP_MISSING_STAGE_DIR", operation));
            }
            if (stageFile == null || stageFile.trim().length() == 0) {
                throw new Exception(mMessages.getString("FTPBC-E004025.MP_MISSING_STAGE_FILE", operation));
            }
            prop.put(FtpFileConfigConstants.P_STAGE_DIR, stageDir);
            prop.put(FtpFileConfigConstants.P_STAGE_FILE, stageFile);
        }

        // with the usePassiveFTP moved into BC's runtime configuration, C_P_FTP_PASSIVE_ON 
        // should be set after this method returned;
        // i.e. the value set here should be overwritten by the usePassiveFTP value from current 
        // runtime configuration
        prop.put(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON, usePassive ? CONST_YES : CONST_NO);

        prop.put(FtpFileConfigConstants.P_TGT_DIR, targetDir != null ? targetDir : "");
        prop.put(FtpFileConfigConstants.P_TGT_DIR_PATT, targetDirIsPattern ? CONST_YES : CONST_NO);
        prop.put(FtpFileConfigConstants.P_TGT_FILE, targetFile);
        prop.put(FtpFileConfigConstants.P_TGT_FILE_PATT, targetFileIsPattern ? CONST_YES : CONST_NO);
        prop.put(FtpFileConfigConstants.P_TGT_APPND, isAppend ? CONST_YES : CONST_NO);

        prop.put(FtpFileConfigConstants.P_PRE_CMD, preCmd);
        if (preCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME) || preCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
            prop.put(FtpFileConfigConstants.P_PRE_FILE, preTargetFile);
            prop.put(FtpFileConfigConstants.P_PRE_FILE_PATT, preTargetFileIsPattern ? CONST_YES : CONST_NO);
            prop.put(FtpFileConfigConstants.P_PRE_DIR, preTargetDir);
            prop.put(FtpFileConfigConstants.P_PRE_DIR_PATT, preTargetDirIsPattern ? CONST_YES : CONST_NO);
        }

        prop.put(FtpFileConfigConstants.P_POST_CMD, postCmd);
        if (postCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME) || postCmd.equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
            prop.put(FtpFileConfigConstants.P_POST_FILE, postTargetFile);
            prop.put(FtpFileConfigConstants.P_POST_FILE_PATT, postTargetFileIsPattern ? CONST_YES : CONST_NO);
            prop.put(FtpFileConfigConstants.P_POST_DIR, postTargetDir);
            prop.put(FtpFileConfigConstants.P_POST_DIR_PATT, postTargetDirIsPattern ? CONST_YES : CONST_NO);
        }

        // section "FTP Raw Commands"
        prop.put(FtpFileConfigConstants.P_RAW_PRE_CMD, "");
        prop.put(FtpFileConfigConstants.P_RAW_POST_CMD, "");

        // section "Sequence Numbering"
        prop.put(FtpFileConfigConstants.P_SEQ_START, new Long(1));
        prop.put(FtpFileConfigConstants.P_SEQ_MAX, new Long(999999));

        prop.put(FtpFileConfigConstants.P_EXTENSION_PROVIDER_CLAZZ, "com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl");
        prop.put(FtpFileConfigConstants.P_EXTENSION_CLIENT_CLAZZ, "com.sun.jbi.ftpbc.ftp.FtpFileClientImpl");

        prop.put(FtpFileConfigConstants.C_P_GEN_CONN_MODE, "Manual");

        // no retry
        prop.put(FtpFileConfigConstants.CONNRETRY_MAXRETRIES, new Long(0));
        prop.put(FtpFileConfigConstants.CONNRETRY_INTERVAL, new Long(1000));

        return prop;
    }

    /**
     * result[0] - the parent dir path
     * result[1] - the file name
     */
    private static String[] getPathComponents(String s) {
        String[] result = new String[2];
        if (s != null && !s.endsWith("/")) {
            int lastSepPos = s.lastIndexOf("/");
            if (lastSepPos >= 0) {
                if (lastSepPos > 0) {
                    result[0] = s.substring(0, lastSepPos);
                } else {
                    result[0] = "/";
                }
                result[1] = s.substring(lastSepPos + 1);
            } else {
                // file name only
                result[1] = s;
            }
        }
        return result;
    }

    public static boolean isEmpty(String s) {
        if (s == null || s.trim().length() == 0) {
            return true;
        } else {
            return false;
        }
    }

    private static boolean hasPattern(String fname) {
        Matcher m = NamePattern.NAME_PATT.matcher(fname);
        return m.find();
    }
}
