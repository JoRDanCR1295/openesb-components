/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.util;

import com.sun.jbi.ftpbc.FTPBCComponentContext;
import com.sun.jbi.ftpbc.extensions.FTPAddress;
import com.sun.jbi.ftpbc.extensions.FTPMessage;
import com.sun.jbi.ftpbc.extensions.FTPTransfer;
import com.sun.jbi.ftpbc.extensions.FTPTransferExtension;
import com.sun.jbi.ftpbc.ftp.TransferNamesAndCommands;
import com.sun.jbi.ftpbc.ftp.exception.FtpInterfaceException;

import javax.jbi.messaging.NormalizedMessage;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 *
 * @author jfu
 */
public class NMPropertyUtil {

    private NMPropertyUtil() {
    }

    public static Map extractNMProperties(FTPAddress address, FTPTransferExtension extElem) {
        HashMap p = new HashMap();
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_HOST, address.getFTPURL().getHost() != null ? address.getFTPURL().getHost() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_PORT, address.getFTPURL().getPort() != null ? address.getFTPURL().getPort() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_USERID, address.getUser() != null ? address.getUser() : (address.getFTPURL().getUser() != null ? address.getFTPURL().getUser() : ""));
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_PASSWORD, address.getPassword() != null ? address.getPassword() : (address.getFTPURL().getPassword() != null ? address.getFTPURL().getPassword() : ""));

        p.put(FTPBCComponentContext.NM_PROP_FTPBC_DIR_LIST_TYLE, address.getDirListStyle() != null ? address.getDirListStyle() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_USE_UD_LIST_STYLE, new Boolean(address.getUseUserDefinedHeuristics()));
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_UD_STYLE_CFG_LOC, address.getUserDefDirListHeuristics() != null ? address.getUserDefDirListHeuristics() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_UD_STYLE_NAME, address.getUserDefDirListStyle() != null ? address.getUserDefDirListStyle() : "NULL");

        p.put(FTPBCComponentContext.NM_PROP_FTPBC_MODE, address.getTransferMode() != null ? address.getTransferMode() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_CNTRL_ENCODING, address.getControlChannelEncoding() != null ? address.getControlChannelEncoding() : "NULL");

        p.put(FTPBCComponentContext.NM_PROP_FTPBC_CNTRL_CH_TIMEOUT, address.getCmdChannelTimeoutStr() != null ? address.getCmdChannelTimeoutStr() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_DATA_CH_TIMEOUT, address.getDataChannelTimeoutStr() != null ? address.getDataChannelTimeoutStr() : "NULL");

        p.put(FTPBCComponentContext.NM_PROP_FTPBC_SECURE_FTP_TYPE, address.getSecureFTPType() != null ? address.getSecureFTPType() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_ENABLE_CCC, new Boolean(address.getEnableCCC()));
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_KEYALIAS, address.getKeyAlias() != null ? address.getKeyAlias() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_KEYPASSWORD, address.getKeyPassword() != null ? address.getKeyPassword() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_KEYSTORE_LOC, address.getKeyStore() != null ? address.getKeyStore() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_KEYSTORE_PASSWORD, address.getKeyStorePassword() != null ? address.getKeyStorePassword() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRUSTSTORE_LOC, address.getTrustStore() != null ? address.getTrustStore() : "NULL");
        p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRUSTSTORE_PASSWORD, address.getTrustStorePassword() != null ? address.getTrustStorePassword() : "NULL");

        if (extElem instanceof FTPMessage) {
            // message
            FTPMessage message = (FTPMessage) extElem;
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_NAME, message.getMessageName() != null ? message.getMessageName() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_NAME_PREFIX, message.getMessageNamePrefixIB() != null ? message.getMessageNamePrefixIB() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_REPO, message.getMessageRepo() != null ? message.getMessageRepo() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_REQ_RESP_CORRELATE, new Boolean(message.getMessageCorrelate()));
        } else {
            // transfer
            FTPTransfer transfer = (FTPTransfer) extElem;
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_SEND_TO, transfer.getSendTo() != null ? transfer.getSendTo() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_APPEND, new Boolean(transfer.getAppend()));
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_DEST_PATT, new Boolean(transfer.getSendToHasPatterns()));
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_SEND_CMD, transfer.getPreSendCommand() != null ? transfer.getPreSendCommand() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_SEND_LOC, transfer.getPreSendLoc() != null ? transfer.getPreSendLoc() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_SEND_LOC_HAS_PATT, new Boolean(transfer.getPreSendLocHasPatterns()));
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_SEND_CMD, transfer.getPostSendCommand() != null ? transfer.getPostSendCommand() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_SEND_LOC, transfer.getPostSendLoc() != null ? transfer.getPostSendLoc() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_SEND_LOC_HAS_PATT, new Boolean(transfer.getPostSendLocHasPatterns()));
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_RECV_FROM, transfer.getReceiveFrom() != null ? transfer.getReceiveFrom() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_SRC_REGEX, new Boolean(transfer.getReceiveFromHasRegexs()));
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_RECV_CMD, transfer.getPreReceiveCommand() != null ? transfer.getPreReceiveCommand() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_RECV_LOC, transfer.getPreReceiveLoc() != null ? transfer.getPreReceiveLoc() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_RECV_LOC_HAS_PATT, new Boolean(transfer.getPreReceiveLocHasPatterns()));
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_RECV_CMD, transfer.getPostReceiveCommand() != null ? transfer.getPostReceiveCommand() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_RECV_LOC, transfer.getPostReceiveLoc() != null ? transfer.getPostReceiveLoc() : "NULL");
            p.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_RECV_LOC_HAS_PATT, new Boolean(transfer.getPostReceiveLocHasPatterns()));
        }
        return p;
    }

    /**
     * check if the normalized message has its "Enable Dynamic Endpoint" set to true
     * i.e. - the use of dynamic endpoint can be per message.
     * @param msg
     * @return
     */
    public static boolean useDynamicEndpoint(NormalizedMessage msg) {
        Object p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_USE_DYN_EP_BINDING);
        boolean b = false;
        if (p != null) {
            String s = p.toString();
            if (s != null && s.equalsIgnoreCase("true")) {
                b = true;
            }
        }
        return b;
    }

    /**
     * 
     * @param msg - normalized message whose NM properties wiil be extracted
     * @param curAddress - FTPAddress object which contains value that fabricated FTPAddress object
     * will fall back to if the NM property does not present
     * @return the FTPAddress object which contains binding info from NM properties of the message
     */
    public static FTPAddress fabricateAddress(NormalizedMessage msg, FTPAddress cur) throws Exception {
        FTPAddress address = new FTPAddress();
        Object p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_CNTRL_CH_TIMEOUT);
        address.setCmdChannelTimeoutStr(p != null ? p.toString() : cur.getCmdChannelTimeoutStr());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_CNTRL_ENCODING);
        address.setControlChannelEncoding(p != null ? p.toString() : cur.getControlChannelEncoding());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_DATA_CH_TIMEOUT);
        address.setDataChannelTimeoutStr(p != null ? p.toString() : cur.getDataChannelTimeoutStr());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_DIR_LIST_TYLE);
        address.setDirListStyle(p != null ? p.toString() : cur.getDirListStyle());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_ENABLE_CCC);
        address.setEnableCCC(p != null ? p.toString().equals("true") : cur.getEnableCCC());

        String url = "ftp://";

        String host = (String) msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_HOST);
        cur.parse();
        host = host != null ? host : cur.getFTPURL().getHost();
        String port = (String) msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_PORT);
        port = port != null ? port : cur.getFTPURL().getPort();

        address.setURL(url.concat(host).concat(":").concat(port));

        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_USERID);

        address.setUser(p != null ? p.toString() : cur.getUser());

        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_PASSWORD);

        address.setPassword(p != null ? p.toString() : cur.getPassword());

        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_SECURE_FTP_TYPE);

        if (p == null) {
            p = cur.getSecureFTPType();
            p = p != null ? p.toString() : "None";
        }

        address.setSecureFTPType(p.toString());

        if (!p.equals("None")) {
            // FTP/SSL
            p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_KEYALIAS);
            address.setKeyAlias(p != null ? p.toString() : cur.getKeyAlias());
            p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_KEYPASSWORD);
            address.setKeyPassword(p != null ? p.toString() : cur.getKeyPassword());
            p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_KEYSTORE_LOC);
            address.setKeyStore(p != null ? p.toString() : cur.getKeyStore());
            p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_KEYSTORE_PASSWORD);
            address.setKeyStorePassword(p != null ? p.toString() : cur.getKeyStorePassword());
            p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRUSTSTORE_LOC);
            address.setTrustStore(p != null ? p.toString() : cur.getTrustStore());
            p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRUSTSTORE_PASSWORD);
            address.setTrustStorePassword(p != null ? p.toString() : cur.getTrustStorePassword());
        }

        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_MODE);
        address.setTransferMode(p != null ? p.toString() : cur.getTransferMode());

        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_USE_UD_LIST_STYLE);
        address.setUseUserDefinedHeuristics(p != null ? p.toString().equals("true") : cur.getUseUserDefinedHeuristics());

        if (address.getUseUserDefinedHeuristics()) {
            p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_UD_STYLE_CFG_LOC);
            address.setUserDefDirListHeuristics(p != null ? p.toString() : cur.getUserDefDirListHeuristics());
            p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_UD_STYLE_NAME);
            address.setUserDefDirListStyle(p != null ? p.toString() : cur.getUserDefDirListStyle());
        }

        return address;
    }

    public static FTPTransferExtension fabricateTransfer(NormalizedMessage msg, FTPTransfer cur) {
        FTPTransfer transfer = new FTPTransfer();
        Object p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_SEND_TO);
        transfer.setSendTo(p != null ? p.toString() : cur.getSendTo());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_APPEND);
        transfer.setAppend(p != null ? p.toString().equals("true") : cur.getAppend());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_DEST_PATT);
        transfer.setSendToHasPatterns(p != null ? p.toString().equals("true") : cur.getSendToHasPatterns());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_SEND_CMD);
        transfer.setPreSendCommand(p != null ? p.toString() : cur.getPreSendCommand());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_SEND_LOC);
        transfer.setPreSendLoc(p != null ? p.toString() : cur.getPreSendLoc());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_SEND_LOC_HAS_PATT);
        transfer.setPreSendLocHasPatterns(p != null ? p.toString().equals("true") : cur.getPreSendLocHasPatterns());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_SEND_CMD);
        transfer.setPostSendCommand(p != null ? p.toString() : cur.getPostSendCommand());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_SEND_LOC);
        transfer.setPostSendLoc(p != null ? p.toString() : cur.getPostSendLoc());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_SEND_LOC_HAS_PATT);
        transfer.setPostSendLocHasPatterns(p != null ? p.toString().equals("true") : cur.getPostSendLocHasPatterns());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_RECV_FROM);
        transfer.setReceiveFrom(p != null ? p.toString() : cur.getReceiveFrom());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_RECV_CMD);
        transfer.setPreReceiveCommand(p != null ? p.toString() : cur.getPreReceiveCommand());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_RECV_LOC);
        transfer.setPreReceiveLoc(p != null ? p.toString() : cur.getPreReceiveLoc());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_RECV_LOC_HAS_PATT);
        transfer.setPreReceiveLocHasPatterns(p != null ? p.toString().equals("true") : cur.getPreReceiveLocHasPatterns());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_RECV_CMD);
        transfer.setPostReceiveCommand(p != null ? p.toString() : cur.getPostReceiveCommand());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_RECV_LOC);
        transfer.setPostReceiveLoc(p != null ? p.toString() : cur.getPostReceiveLoc());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_RECV_LOC_HAS_PATT);
        transfer.setPostReceiveLocHasPatterns(p != null ? p.toString().equals("true") : cur.getPostReceiveLocHasPatterns());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_SRC_REGEX);
        transfer.setReceiveFromHasRegexs(p != null ? p.toString().equals("true") : cur.getReceiveFromHasRegexs());
        // the followings are always fall back to existing ext element
        transfer.setMessageCorrelate(cur.getMessageCorrelate());
        transfer.setEncodingStyle(cur.getEncodingStyle());
        transfer.setUse(cur.getUse());
        return transfer;
    }

    public static FTPTransferExtension fabricateMessage(NormalizedMessage msg, FTPMessage cur) {
        FTPMessage message = new FTPMessage();
        Object p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_NAME);
        message.setMessageName(p != null ? p.toString() : cur.getMessageName());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_NAME_PREFIX);
        message.setMessageNamePrefixIB(p != null ? p.toString() : cur.getMessageNamePrefixIB());
        message.setMessageNamePrefixOB(p != null ? p.toString() : cur.getMessageNamePrefixOB());
        p = msg.getProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_REPO);
        message.setMessageRepo(p != null ? p.toString() : cur.getMessageRepo());
        // the followings are always fall back to existing ext element
        message.setMessageCorrelate(cur.getMessageCorrelate());
        message.setArchiveEnabled(cur.getArchiveEnabled());
        message.setStagingEnabled(cur.getStagingEnabled());
        message.setProtectEnabled(cur.getProtectEnabled());
        message.setEncodingStyle(cur.getEncodingStyle());
        message.setUse(cur.getUse());
        return message;
    }

    public static void setNMProperties(NormalizedMessage nmsg, Map nmProperties) {
        //populate N MSG properties if they are set
        if (nmProperties != null) {
            Set<Entry<Object, Object>> setNM = nmProperties.entrySet();
            Iterator<Entry<Object, Object>> itrNM = setNM.iterator();
            while (itrNM.hasNext()) {
                Entry<Object, Object> entryNM = itrNM.next();
                nmsg.setProperty((String) entryNM.getKey(), entryNM.getValue());
            }
        }
    }

    public static void mergeNMPropertiesResolvedParams4Get(TransferNamesAndCommands tnc, Map nmProperties, boolean isTransferExt) throws FtpInterfaceException {
        if (isTransferExt) {
            // ftp:transfer
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_GET_DIR,
                    tnc.getTargetDirectoryName() != null ? tnc.getTargetDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_GET_FILE,
                    tnc.getTargetFileName() != null ? tnc.getTargetFileName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_GET_DIR,
                    tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_GET_FILE,
                    tnc.getPreFileName() != null ? tnc.getPreFileName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_GET_DIR,
                    tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_GET_FILE,
                    tnc.getPostFileName() != null ? tnc.getPostFileName()
                    : "");
        } else {
            // ftp:message
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_GET_DIR,
                    tnc.getTargetDirectoryName() != null ? tnc.getTargetDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_GET_FILE,
                    tnc.getTargetFileName() != null ? tnc.getTargetFileName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_PRE_GET_DIR,
                    tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_PRE_GET_FILE,
                    tnc.getPreFileName() != null ? tnc.getPreFileName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_POST_GET_DIR,
                    tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_POST_GET_FILE,
                    tnc.getPostFileName() != null ? tnc.getPostFileName()
                    : "");
        }
    }

    public static void addNMPropertiesResolvedParams4Get(NormalizedMessage nmsg, TransferNamesAndCommands tnc, boolean isTransferExt) throws FtpInterfaceException {
        if (isTransferExt) {
            // ftp:transfer
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_GET_DIR,
                    tnc.getTargetDirectoryName() != null ? tnc.getTargetDirectoryName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_GET_FILE,
                    tnc.getTargetFileName() != null ? tnc.getTargetFileName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_GET_DIR,
                    tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_GET_FILE,
                    tnc.getPreFileName() != null ? tnc.getPreFileName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_GET_DIR,
                    tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_GET_FILE,
                    tnc.getPostFileName() != null ? tnc.getPostFileName()
                    : "");
        } else {
            // ftp:message
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_GET_DIR,
                    tnc.getTargetDirectoryName() != null ? tnc.getTargetDirectoryName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_GET_FILE,
                    tnc.getTargetFileName() != null ? tnc.getTargetFileName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_PRE_GET_DIR,
                    tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_PRE_GET_FILE,
                    tnc.getPreFileName() != null ? tnc.getPreFileName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_POST_GET_DIR,
                    tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName()
                    : "");
            nmsg.setProperty(FTPBCComponentContext.NM_PROP_FTPBC_MSG_POST_GET_FILE,
                    tnc.getPostFileName() != null ? tnc.getPostFileName()
                    : "");
        }
    }

    public static void mergeNMPropertiesResolvedParams4Put(TransferNamesAndCommands tnc, Map nmProperties, boolean isTransferExt) throws FtpInterfaceException {
        if (isTransferExt) {
            // ftp:transfer
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PUT_DIR,
                    tnc.getTargetDirectoryName() != null ? tnc.getTargetDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PUT_FILE,
                    tnc.getTargetFileName() != null ? tnc.getTargetFileName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_PUT_DIR,
                    tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_PRE_PUT_FILE,
                    tnc.getPreFileName() != null ? tnc.getPreFileName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_PUT_DIR,
                    tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_TRANS_POST_PUT_FILE,
                    tnc.getPostFileName() != null ? tnc.getPostFileName()
                    : "");
        } else {
            // ftp:message
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_PUT_DIR,
                    tnc.getTargetDirectoryName() != null ? tnc.getTargetDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_PUT_FILE,
                    tnc.getTargetFileName() != null ? tnc.getTargetFileName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_PRE_PUT_DIR,
                    tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_PRE_PUT_FILE,
                    tnc.getPreFileName() != null ? tnc.getPreFileName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_POST_PUT_DIR,
                    tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName()
                    : "");
            nmProperties.put(FTPBCComponentContext.NM_PROP_FTPBC_MSG_POST_PUT_FILE,
                    tnc.getPostFileName() != null ? tnc.getPostFileName()
                    : "");
        }
    }
}
