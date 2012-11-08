/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.util;

import com.sun.jbi.ftpbc.InboundMessageProcessor;
import com.sun.jbi.ftpbc.ftp.FtpFileClient;
import com.sun.jbi.ftpbc.ftp.FtpFileProvider;
import com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands;
import com.sun.jbi.ftpbc.persistence.FTPBCPersistStore;

import javax.xml.namespace.QName;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.UUID;

/**
 *
 * @author jfu
 */
public class Utils {
    private Utils() {}
    public static final String getStackTrace(Throwable t) {
        String result = null;
        if (t != null) {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            PrintWriter pw = new PrintWriter(bos);
            t.printStackTrace(pw);
            pw.flush();
            result = new String(bos.toByteArray());
        }
        return result;
    }

    public static final File locateJBIDirectory(String suPath) {
        File jbiDir = null;
        if (suPath != null) {
            File parent = new File(suPath);
            int count = 0;
            while (parent != null && parent.exists() && parent.isDirectory() && count < 4) {
                parent = parent.getParentFile();
                count++;
            }
            if (parent != null && parent.getName().equals("jbi")) {
                jbiDir = parent;
            }
        }
        return jbiDir;
    }

    public static final File getLocalJBIPersistenceLocation(String suPath) {
        File localPersistenceLocation = null;
        File jbiDir = locateJBIDirectory(suPath);
        if (jbiDir != null) {
            localPersistenceLocation = new File(jbiDir, "persistence"); //NOI18N
        } else {
            try {
                localPersistenceLocation =
                        makeFallbackPersistenceDirectory(suPath);
            } catch (UnsupportedEncodingException usee) {
                // ignore, UTF-8 is supported
            }
        }
        return localPersistenceLocation;
    }

    public static final void saveMessageNormalizationFailureInfo(File base, FtpFileTransferNamesAndCommands tnc, String uniqMsgID, Exception exception) throws Exception {
        if (base != null && base.exists()) {
            PrintWriter pw = null;
            try {
                pw = new PrintWriter(new File(base, uniqMsgID.concat(FTPBCPersistStore.MESSAGE_ERROR_LOG_SUFFIX)));
                if (tnc != null) {
                    pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_PRE_DIR.concat(tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName() : ""));
                    pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_PRE_FILE.concat(tnc.getPreFileName() != null ? tnc.getPreFileName() : ""));
                    pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_DIR.concat(tnc.getTargetDirectoryName() != null ? tnc.getTargetDirectoryName() : ""));
                    pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_FILE.concat(tnc.getTargetFileName() != null ? tnc.getTargetFileName() : ""));
                    pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_POST_DIR.concat(tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName() : ""));
                    pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_POST_FILE.concat(tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName() : ""));
                    pw.println("Message causing exception is moved to:");
                    pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_PRE_DIR.concat(tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName() : ""));
                    pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_PRE_FILE.concat(tnc.getPreFileName() != null ? tnc.getPreFileName().concat(FTPBCPersistStore.MESSAGE_ERROR_LOG_SUFFIX) : ""));
                }
                pw.println(">>>>>>================================");
                exception.printStackTrace(pw);
                pw.println("<<<<<<================================");
            } finally {
                if (pw != null) {
                    pw.close();
                }
            }
        }
    }

    public static final void prepareFTPInterface(FtpFileClient client, FtpFileProvider provider, InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES subtype) {
        client.setWarningOff(true);
        provider.setWarningOff(true);

        if (subtype == InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES.TRANSFER) {
            // ftp:transfer still uses regex as receiveFrom value
            // tell the logic in the provider about it
            provider.setUseRegexAsMatcher(true);
        }
    }

    public static final String genPrefix(QName mOperationName) {
        String localName = mOperationName.getLocalPart();
        if (localName.length() > 16) {
            localName = localName.substring(0, 16);
        }
        return localName.concat("_");
    }

    /**
     * Makes a fallback persistence baseLocation directory if not running in
     * a JBI container.
     *
     * @param suPath Service Unit path.
     * @return Fallback persistence directory.
     * @throws java.lang.Exception
     * @since 2.1
     */
    public static File makeFallbackPersistenceDirectory(String suPath)
            throws UnsupportedEncodingException {
        if ((null == suPath) || (suPath.trim().length() == 0)) {
            return null;
        }
        File fallbackDir = null;
        String pathHash = UUID.nameUUIDFromBytes(
                suPath.getBytes("UTF-8")).toString();                   //NOI18N
        if ((pathHash != null) && (pathHash.length() > 0)) {
            pathHash = "persistence-" + pathHash;                       //NOI18N
            String userHome = System.getProperty("user.home");          //NOI18N
            File testDir = new File(userHome, pathHash);
            int ring = 1;
            while (testDir.exists() && !testDir.isDirectory()) {
                testDir = new File(userHome, pathHash + "-" + (ring++));//NOI18N
            }
            fallbackDir = testDir;
        }
        return fallbackDir;
    }
}
