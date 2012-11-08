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
 * @(#)BatchFtpTester.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.ftp;

import java.io.File;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

/*
 *
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class BatchFtpTester {
    
    public BatchFtpTester() {
    }
    
    public static void main(String args[]) {
        System.out.println("Start of BatchFtpTester.");
        try {
            //storeProperties2File(null, null, null);

            Properties props = new Properties();
            props.put("FTP/Host Name", "localhost");
            BatchFtp ftp = new BatchFtp();
            ftp.initialize(props);
            
            //ftp.getConfiguration().setSequenceNumberPersistenceMedia("DB");
            
            ftp.getConfiguration().setDirectoryListingStyle("NT 4.0");
            ftp.getConfiguration().setTargetDirectoryName("%8");
            ftp.getConfiguration().setTargetDirectoryNameIsPattern(true);
            ftp.getConfiguration().setTargetFileName("file_%0.%1.%9.txt");
            if (false) {
                ftp.getConfiguration().setStageEnabled(true);
                ftp.getConfiguration().setStageDirectoryName("BatchFtpStage");
                ftp.getConfiguration().setStageFileName("%f.processing");
            } else {
                ftp.getConfiguration().setPostTransferCommand("None");
                ftp.getConfiguration().setPostDirectoryName("%f");
                ftp.getConfiguration().setPostDirectoryNameIsPattern(true);
                ftp.getConfiguration().setPostFileName("%f.rename");
            }
            ftp.getClient().setPayload("this is a payload for a ftp file".getBytes());
            ftp.getClient().put();
            ftp.getClient().setPayload(null);
            //ftp.getConfiguration().setTargetFileName("file_.*.txt");
            ftp.getConfiguration().setPostTransferCommand("Rename");
            ftp.getConfiguration().setPostDirectoryName("%f");
            ftp.getConfiguration().setPostFileName("%f.rename");
            ftp.getClient().get();
            System.out.println("End of BatchFtpTester successfully :-)");
            return;
            
        } catch(Exception e) {
            e.printStackTrace();
            System.out.println("End of BatchFtpTester with exception :-(");
            return;
        }
    }
    
    public static Properties extractProperties(FtpFileConfiguration config)
    throws Exception {
        if (null == config) {
            BatchFtp ftp = new BatchFtp();
            ftp.initialize(new Properties());
            config = ftp.getConfiguration();
        }
        Properties props = new Properties();
        props.setProperty("General Settings/Connection Mode", config.getConnectionEstablishmentMode());
        props.setProperty("General Settings/State Persistence Base Location", config.getStatePersistenceBaseLocation());
        props.setProperty("General Settings/Synchronized", config.getSynchronized() ? "Yes" : "No");
        props.setProperty("General Settings/Transaction Type", config.getTransactionType());
        props.setProperty("connection-retry-settings/ConnectionRetries", (new StringBuilder()).append(config.getMaxRetry()).append("").toString());
        props.setProperty("connection-retry-settings/ConnectionRetryInterval", (new StringBuilder()).append(config.getRetryInterval()).append("").toString());
        props.setProperty("connector/Connection Inactivity Timeout", (new StringBuilder()).append(config.getConnectionInactivityTimeout()).append("").toString());
        props.setProperty("connector/Connection Verification Interval", (new StringBuilder()).append(config.getConnectionVerificationInterval()).append("").toString());
        props.setProperty("FTP Raw Commands/Post Transfer Raw Commands", config.getPostTransferRawCommands());
        props.setProperty("FTP Raw Commands/Pre Transfer Raw Commands", config.getPreTransferRawCommands());
        props.setProperty("FTP/Command Connection Timeout", (new StringBuilder()).append(config.getCommandConnectionTimeout()).append("").toString());
        props.setProperty("FTP/Data Connection Timeout", (new StringBuilder()).append(config.getDataConnectionTimeout()).append("").toString());
        props.setProperty("FTP/Directory Listing Style", config.getDirectoryListingStyle());
        props.setProperty("FTP/Host Name", config.getHostName());
        props.setProperty("FTP/Mode", config.getMode());
        props.setProperty("FTP/Password", config.getPassword());
        props.setProperty("FTP/Server Port", (new StringBuilder()).append(config.getServerPort()).append("").toString());
        props.setProperty("FTP/Use PASV", config.getUsePASV() ? "Yes" : "No");
        props.setProperty("FTP/User Defined Directory Listing Style", config.getUserDefinedDirectoryListingStyle());
        props.setProperty("FTP/User Defined Heuristics Configuration File", config.getUserHeuristicsLocation());
        props.setProperty("FTP/User Name", config.getUserName());
        props.setProperty("Pre Transfer/Pre Directory Name Is Pattern", config.getPreDirectoryNameIsPattern() ? "Yes" : "No");
        props.setProperty("Pre Transfer/Pre Directory Name", config.getPreDirectoryName());
        props.setProperty("Pre Transfer/Pre File Name Is Pattern", config.getPreFileNameIsPattern() ? "Yes" : "No");
        props.setProperty("Pre Transfer/Pre File Name", config.getPreFileName());
        props.setProperty("Pre Transfer/Pre Transfer Command", config.getPreTransferCommand());
        props.setProperty("Target Location/Append", config.getAppend() ? "Yes" : "No");
        props.setProperty("Target Location/Target Directory Name Is Pattern", config.getTargetDirectoryNameIsPattern() ? "Yes" : "No");
        props.setProperty("Target Location/Target Directory Name", config.getTargetDirectoryName());
        props.setProperty("Target Location/Target File Name Is Pattern", config.getTargetFileNameIsPattern() ? "Yes" : "No");
        props.setProperty("Target Location/Target File Name", config.getTargetFileName());
        props.setProperty("Stage Transfer/Enabled", config.getStageEnabled() ? "Yes" : "No");
        props.setProperty("Stage Transfer/Stage Directory Name Is Pattern", config.getStageDirectoryNameIsPattern() ? "Yes" : "No");
        props.setProperty("Stage Transfer/Stage Directory Name", config.getStageDirectoryName());
        props.setProperty("Stage Transfer/Stage File Name Is Pattern", config.getStageFileNameIsPattern() ? "Yes" : "No");
        props.setProperty("Stage Transfer/Stage File Name", config.getStageFileName());
        props.setProperty("Post Transfer/Post Directory Name Is Pattern", config.getPostDirectoryNameIsPattern() ? "Yes" : "No");
        props.setProperty("Post Transfer/Post Directory Name", config.getPostDirectoryName());
        props.setProperty("Post Transfer/Post File Name Is Pattern", config.getPostFileNameIsPattern() ? "Yes" : "No");
        props.setProperty("Post Transfer/Post File Name", config.getPostFileName());
        props.setProperty("Post Transfer/Post Transfer Command", config.getPostTransferCommand());
        props.setProperty("Sequence Numbering/Max Sequence Number", (new StringBuilder()).append(config.getMaxSequenceNumber()).append("").toString());
        props.setProperty("Sequence Numbering/Starting Sequence Number", (new StringBuilder()).append(config.getStartingSequenceNumber()).append("").toString());
        props.setProperty("Sequence Numbering/Sequence Number Persistence Media", config.getSequenceNumberPersistenceMedia());
        props.setProperty("Extensions/Client Class Name", config.getClientClassName());
        props.setProperty("Extensions/Provider Class Name", config.getProviderClassName());
        props.setProperty("Extensions/User Property File", config.getUserPropertyFile());
        props.setProperty("Dynamic Configuration/Action on Malformed Command", config.getActionOnMalformedCommand());
        props.setProperty("Dynamic Configuration/Include Order Record in Error Record", config.getIncludeOrderRecordInErrorRecord() ? "Yes" : "No");
        props.setProperty("Dynamic Configuration/Include Payload in Error Record", config.getIncludePayloadInErrorRecord() ? "Yes" : "No");
        props.setProperty("Dynamic Configuration/Publish Status Record on Error", config.getPublishStatusRecordOnError() ? "Yes" : "No");
        props.setProperty("Dynamic Configuration/Publish Status Record on Success", config.getPublishStatusRecordOnSuccess() ? "Yes" : "No");
        props.setProperty("SOCKS/Socks Enabled", config.getSocksEnabled() ? "Yes" : "No");
        props.setProperty("SOCKS/Socks Host Name", config.getSocksHostName());
        props.setProperty("SOCKS/Socks Password", config.getSocksPassword());
        props.setProperty("SOCKS/Socks Server Port", (new StringBuilder()).append(config.getSocksServerPort()).append("").toString());
        props.setProperty("SOCKS/Socks User Name", config.getSocksUserName());
        props.setProperty("SOCKS/Socks Version", -1 != config.getSocksVersion() ? (new StringBuilder()).append(config.getSocksVersion()).append("").toString() : "Unknown");
        props.setProperty("SSH Tunneling/SSH Channel Established", config.getSSHChannelEstablished() ? "Yes" : "No");
        props.setProperty("SSH Tunneling/SSH Command Line", config.getSSHCommandLine());
        props.setProperty("SSH Tunneling/SSH Listen Host", config.getSSHListenHost());
        props.setProperty("SSH Tunneling/SSH Listen Port", (new StringBuilder()).append(config.getSSHListenPort()).append("").toString());
        props.setProperty("SSH Tunneling/SSH Password", config.getSSHPassword());
        props.setProperty("SSH Tunneling/SSH Tunneling Enabled", config.getSSHTunnelingEnabled() ? "Yes" : "No");
        props.setProperty("SSH Tunneling/SSH User Name", config.getSSHUserName());
        return props;
    }
    
    public static void storeProperties2File(Properties props, String dirName, String fileName)
    throws Exception {
        if(null == props) {
            props = extractProperties(null);
        }
        if(null == dirName || 0 == dirName.length()) {
            dirName = System.getProperty("user.home", "/temp");
        }
        if(null == fileName || 0 == fileName.length()) {
            fileName = "BatchFtp." + (new SimpleDateFormat("yyyyMMddhhmmSSSS")).format(new Date());
        }
        FileOutputStream fos = null;
        fos = new FileOutputStream(new File(dirName, fileName + ".properties"));
        props.store(fos, "Generated Properties file from test ...");
        fos.flush();
        fos = new FileOutputStream(new File(dirName, fileName + ".xml"));
        props.storeToXML(fos, "Generated XML file from test ... available since JDK 1.5");
        fos.flush();
        fos.close();
    }
    
    /*
    private static Properties convert2Properties()
    throws Exception {
        Properties props = new Properties();
        String oid = null;
        String name = null;
        if(oid != null && oid.trim().length() > 0 && name != null && name.trim().length() > 0) {
            props.put("conn-props.collaboration.oid", oid);
            props.put("conn-props.connection.name", name);
        }
        props.put("General Settings/Transaction Type", "Non-Transactional");
        props.put("General Settings/State Persistence Base Location", "");
        props.put("General Settings/Synchronized", "Yes");
        props.put("FTP/Directory Listing Style", "UNIX");
        props.put("FTP/Host Name", "");
        props.put("FTP/User Name", "anonymous");
        props.put("FTP/Password", "");
        props.put("FTP/User Defined Heuristics Configuration File", "");
        props.put("FTP/User Defined Directory Listing Style", "");
        props.put("FTP/Server Port", "21");
        props.put("FTP/Mode", "Binary");
        props.put("FTP/Use PASV", "Yes");
        props.put("FTP/Command Connection Timeout", "45000");
        props.put("FTP/Data Connection Timeout", "45000");
        props.put("Target Location/Target Directory Name", "");
        props.put("Target Location/Target Directory Name Is Pattern", "No");
        props.put("Target Location/Target File Name", "");
        props.put("Target Location/Target File Name Is Pattern", "Yes");
        props.put("Target Location/Append", "No");
        props.put("Pre Transfer/Pre Transfer Command", "None");
        props.put("Pre Transfer/Pre Directory Name", "");
        props.put("Pre Transfer/Pre Directory Name Is Pattern", "No");
        props.put("Pre Transfer/Pre File Name", "");
        props.put("Pre Transfer/Pre File Name Is Pattern", "Yes");
        props.put("Post Transfer/Post Transfer Command", "None");
        props.put("Post Transfer/Post Directory Name", "");
        props.put("Post Transfer/Post Directory Name Is Pattern", "No");
        props.put("Post Transfer/Post File Name", "");
        props.put("Post Transfer/Post File Name Is Pattern", "Yes");
        props.put("FTP Raw Commands/Pre Transfer Raw Commands", "");
        props.put("FTP Raw Commands/Post Transfer Raw Commands", "");
        props.put("Sequence Numbering/Starting Sequence Number", "1");
        props.put("Sequence Numbering/Max Sequence Number", "999999");
        props.put("SOCKS/Socks Enabled", "No");
        props.put("SOCKS/Socks Host Name", "");
        props.put("SOCKS/Socks Server Port", "1080");
        props.put("SOCKS/Socks User Name", "");
        props.put("SOCKS/Socks Password", "");
        props.put("SOCKS/Socks Version", "Unknown");
        props.put("SSH Tunneling/SSH Tunneling Enabled", "No");
        props.put("SSH Tunneling/SSH Channel Established", "No");
        props.put("SSH Tunneling/SSH Command Line", "");
        props.put("SSH Tunneling/SSH Listen Host", "localhost");
        props.put("SSH Tunneling/SSH Listen Port", "4567");
        props.put("SSH Tunneling/SSH User Name", "");
        props.put("SSH Tunneling/SSH Password", "");
        props.put("Extensions/Provider Class Name", "com.sun.jbi.batchext.ftp.FtpFileProviderImpl");
        props.put("Extensions/Client Class Name", "com.sun.jbi.batchext.ftp.FtpFileClientImpl");
        props.put("Extensions/User Property File", "");
        props.put("connector/Connection Inactivity Timeout", "0");
        props.put("connector/Connection Verification Interval", "60000");
        props.put("Dynamic Configuration/Publish Status Record on Success", "No");
        props.put("Dynamic Configuration/Publish Status Record on Error", "No");
        props.put("Dynamic Configuration/Include Order Record in Error Record", "No");
        props.put("Dynamic Configuration/Include Payload in Error Record", "No");
        props.put("Dynamic Configuration/Action on Malformed Command", "Exit");
        props.put("General Settings/Connection Mode", "Automatic");
        props.put("connection-retry-settings/ConnectionRetries", "0");
        props.put("connection-retry-settings/ConnectionRetryInterval", "1000");
        return props;
    }
    
    private static Properties initialConfigValues()
    throws Exception {
        Properties props = new Properties();
        props.put("connection-retry-settings/ConnectionRetries", Integer.valueOf(1));
        props.put("connection-retry-settings/ConnectionRetryInterval", Integer.valueOf(10000));
        props.put("General Settings/Transaction Type", "Non-Transactional");
        props.put("General Settings/State Persistence Base Location", "");
        props.put("General Settings/Synchronized", "Yes");
        props.put("FTP/Directory Listing Style", "UNIX");
        props.put("FTP/Host Name", "");
        props.put("FTP/User Name", "anonymous");
        props.put("FTP/Password", "");
        props.put("FTP/Server Port", Integer.valueOf(21));
        props.put("FTP/Mode", "Binary");
        props.put("FTP/Use PASV", "Yes");
        props.put("FTP/Command Connection Timeout", Integer.valueOf(45000));
        props.put("FTP/Data Connection Timeout", Integer.valueOf(45000));
        props.put("FTP/User Defined Heuristics Configuration File", "");
        props.put("FTP/User Defined Directory Listing Style", "");
        props.put("Target Location/Target Directory Name", "");
        props.put("Target Location/Target Directory Name Is Pattern", "No");
        props.put("Target Location/Target File Name", "");
        props.put("Target Location/Target File Name Is Pattern", "Yes");
        props.put("Target Location/Append", "No");
        props.put("Pre Transfer/Pre Transfer Command", "None");
        props.put("Pre Transfer/Pre Directory Name", "");
        props.put("Pre Transfer/Pre Directory Name Is Pattern", "No");
        props.put("Pre Transfer/Pre File Name", "");
        props.put("Pre Transfer/Pre File Name Is Pattern", "Yes");
        props.put("Post Transfer/Post Transfer Command", "None");
        props.put("Post Transfer/Post Directory Name", "");
        props.put("Post Transfer/Post Directory Name Is Pattern", "No");
        props.put("Post Transfer/Post File Name", "");
        props.put("Post Transfer/Post File Name Is Pattern", "Yes");
        props.put("FTP Raw Commands/Pre Transfer Raw Commands", "");
        props.put("FTP Raw Commands/Post Transfer Raw Commands", "");
        props.put("Sequence Numbering/Starting Sequence Number", Integer.valueOf(1));
        props.put("Sequence Numbering/Max Sequence Number", Integer.valueOf(0xf423f));
        props.put("SOCKS/Socks Enabled", "No");
        props.put("SOCKS/Socks Host Name", "");
        props.put("SOCKS/Socks Server Port", Integer.valueOf(1080));
        props.put("SOCKS/Socks User Name", "");
        props.put("SOCKS/Socks Password", "");
        props.put("SOCKS/Socks Version", "Unknown");
        props.put("SSH Tunneling/SSH Tunneling Enabled", "No");
        props.put("SSH Tunneling/SSH Channel Established", "No");
        props.put("SSH Tunneling/SSH Command Line", "");
        props.put("SSH Tunneling/SSH Listen Host", "localhost");
        props.put("SSH Tunneling/SSH Listen Port", Integer.valueOf(4567));
        props.put("SSH Tunneling/SSH User Name", "");
        props.put("SSH Tunneling/SSH Password", "");
        props.put("Extensions/Provider Class Name", "com.sun.jbi.batchext.ftp.FtpFileProviderImpl");
        props.put("Extensions/Client Class Name", "com.sun.jbi.batchext.ftp.FtpFileClientImpl");
        props.put("Extensions/User Property File", "");
        props.put("connector/Connection Inactivity Timeout", Integer.valueOf(0));
        props.put("connector/Connection Verification Interval", Integer.valueOf(60000));
        props.put("Dynamic Configuration/Publish Status Record on Success", "No");
        props.put("Dynamic Configuration/Publish Status Record on Error", "No");
        props.put("Dynamic Configuration/Include Order Record in Error Record", "No");
        props.put("Dynamic Configuration/Include Payload in Error Record", "No");
        props.put("Dynamic Configuration/Action on Malformed Command", "Exit");
        props.put("General Settings/Connection Mode", "Automatic");
        return props;
    }
    
    private static Properties getFromConfigTemplate()
    throws Exception {
        Properties props = new Properties();
        String oid = null;
        String name = null;
        if(oid != null && oid.trim().length() > 0 && name != null && name.trim().length() > 0) {
            props.put("conn-props.collaboration.oid", oid);
            props.put("conn-props.connection.name", name);
        }
        props.put("General Settings/Transaction Type", "Non-Transactional");
        props.put("General Settings/State Persistence Base Location", "");
        props.put("General Settings/Synchronized", "Yes");
        props.put("FTP/Directory Listing Style", "UNIX");
        props.put("FTP/Host Name", "localhost");
        props.put("FTP/User Name", "anonymous");
        props.put("FTP/Password", "");
        props.put("FTP/User Defined Heuristics Configuration File", "");
        props.put("FTP/User Defined Directory Listing Style", "");
        props.put("FTP/Server Port", new Long(21L));
        props.put("FTP/Mode", "Binary");
        props.put("FTP/Use PASV", "Yes");
        props.put("FTP/Command Connection Timeout", new Long(45000L));
        props.put("FTP/Data Connection Timeout", new Long(45000L));
        props.put("Target Location/Target Directory Name", "");
        props.put("Target Location/Target Directory Name Is Pattern", "No");
        props.put("Target Location/Target File Name", "");
        props.put("Target Location/Target File Name Is Pattern", "Yes");
        props.put("Target Location/Append", "No");
        props.put("Pre Transfer/Pre Transfer Command", "None");
        props.put("Pre Transfer/Pre Directory Name", "");
        props.put("Pre Transfer/Pre Directory Name Is Pattern", "Yes");
        props.put("Pre Transfer/Pre File Name", "");
        props.put("Pre Transfer/Pre File Name Is Pattern", "Yes");
        props.put("Post Transfer/Post Transfer Command", "None");
        props.put("Post Transfer/Post Directory Name", "");
        props.put("Post Transfer/Post Directory Name Is Pattern", "Yes");
        props.put("Post Transfer/Post File Name", "");
        props.put("Post Transfer/Post File Name Is Pattern", "Yes");
        props.put("FTP Raw Commands/Pre Transfer Raw Commands", "");
        props.put("FTP Raw Commands/Post Transfer Raw Commands", "");
        props.put("Sequence Numbering/Starting Sequence Number", new Long(1L));
        props.put("Sequence Numbering/Max Sequence Number", new Long(0xf423fL));
        props.put("SOCKS/Socks Enabled", "No");
        props.put("SOCKS/Socks Host Name", "");
        props.put("SOCKS/Socks Server Port", new Long(1080L));
        props.put("SOCKS/Socks User Name", "");
        props.put("SOCKS/Socks Password", "");
        props.put("SOCKS/Socks Version", "Unknown");
        props.put("SSH Tunneling/SSH Tunneling Enabled", "No");
        props.put("SSH Tunneling/SSH Channel Established", "No");
        props.put("SSH Tunneling/SSH Command Line", "");
        props.put("SSH Tunneling/SSH Listen Host", "localhost");
        props.put("SSH Tunneling/SSH Listen Port", new Long(4567L));
        props.put("SSH Tunneling/SSH User Name", "");
        props.put("SSH Tunneling/SSH Password", "");
        props.put("Extensions/Provider Class Name", "com.sun.jbi.batchext.ftp.FtpFileProviderImpl");
        props.put("Extensions/Client Class Name", "com.sun.jbi.batchext.ftp.FtpFileClientImpl");
        props.put("Extensions/User Property File", "");
        props.put("connector/Connection Inactivity Timeout", new Long(0L));
        props.put("connector/Connection Verification Interval", new Long(60000L));
        props.put("Dynamic Configuration/Publish Status Record on Success", "No");
        props.put("Dynamic Configuration/Publish Status Record on Error", "No");
        props.put("Dynamic Configuration/Include Order Record in Error Record", "No");
        props.put("Dynamic Configuration/Include Payload in Error Record", "No");
        props.put("Dynamic Configuration/Action on Malformed Command", "Exit");
        props.put("General Settings/Connection Mode", "Automatic");
        props.put("connection-retry-settings/ConnectionRetries", new Long(0L));
        props.put("connection-retry-settings/ConnectionRetryInterval", new Long(1000L));
        return props;
    }
    */
}
