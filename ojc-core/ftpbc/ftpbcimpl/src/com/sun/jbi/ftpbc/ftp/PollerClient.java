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
 * @(#)PollerClient.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.ftpbc.ftp;

import com.sun.jbi.ftpbc.RemoteTargetSemaphoreManager;
import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.ftpbc.ftp.connection.FTPBCConnectionManager;
import com.sun.jbi.ftpbc.ftp.exception.ConfigurationException;
import com.sun.jbi.ftpbc.ftp.exception.FtpInterfaceException;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import com.sun.jbi.internationalization.Messages;
import java.util.Properties;
import java.util.concurrent.Semaphore;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * simulator of one ftp client polling a target directory for a target file
 * with a specified name pattern;
 * e.g. polling any file with name like: my_data_[0-5]\.dat
 *
 * @author jfu
 */
public class PollerClient implements Runnable, Stoppable {
    private static final Messages mMessages =
            Messages.getMessages(PollerClient.class);
    private static Logger mLogger = Messages.getLogger(PollerClient.class);

    public static final String P_POLL_INTERVAL = "poll_interval_millis";
    private Properties mParams;
    private long mPollInterval;
    private int mLeasePeriodMillis;
    private String mThreadDesc;
    private boolean mVerbose;
    private boolean mStop;
    
    /**
     * Creates a new instance of SequenceClient
     */
    public PollerClient(
            Properties ftpParams,
            Properties simulationParams)
            throws Exception {
        mParams = simulationParams;
        
        String str = simulationParams.getProperty(P_POLL_INTERVAL);
        if ( Driver.isEmpty(str) ) {
            Driver.throwMissingParam(P_POLL_INTERVAL);
        } else {
            mPollInterval = Driver.getLong(str);
            if ( mPollInterval <= 0 ) {
                Driver.throwInvalidParam(P_POLL_INTERVAL, str);
            }
        }
        
        str = simulationParams.getProperty(Driver.P_LEASE_PERIOD);
        if ( Driver.isEmpty(str) ) {
            Driver.throwMissingParam(Driver.P_LEASE_PERIOD);
        } else {
            mLeasePeriodMillis = Driver.getInteger(str);
            if ( mLeasePeriodMillis <= 0 ) {
                Driver.throwInvalidParam(Driver.P_LEASE_PERIOD, str);
            }
        }
        
        str = simulationParams.getProperty(Driver.P_VERBOSE);
        mVerbose = (str != null && str.equalsIgnoreCase("YES") ) ? true : false;
        
//        mFtp = new FtpInterface();
//        
//        try {
//            mFtp.initialize(ftpParams);
//        } catch (ConfigurationException ex) {
//            ex.printStackTrace();
//            mFtp = null;
//        } catch (FtpInterfaceException ex) {
//            ex.printStackTrace();
//            mFtp = null;
//        }
    }
    
    public void run() {
        mStop = false;
        mThreadDesc = Thread.currentThread().getName() + ":" + Thread.currentThread().getId();

        while ( true ) {
            if ( mStop ) {
                // stop polling as instructed
                if ( mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Stop the PollerClient =" + mThreadDesc);
                }
                break;
            }
            try {
                Connection ftpConn = FTPBCConnectionManager.getConnection(mParams);

                assert ftpConn != null : "Can not obtain ftp connection in Response PollerClient =" + mThreadDesc;

                FtpInterface ftp = (FtpInterface)ftpConn.getClientObject();
                FtpFileClient client = ftp.getClient();
                FtpFileProvider provider = ftp.getProvider();
                client.setWarningOff(true);
                provider.setWarningOff(true);

                String targetDir = ftp.getConfiguration().getTargetDirectoryName();
                String connKey = ftpConn.getKey();
                String semaKey = connKey.concat(targetDir); // when target dir is a pattern, the synch range covers all derived dirs

                provider.setUseRegexAsMatcher(true);

                Semaphore sema = null;
        
                try {
                    sema = RemoteTargetSemaphoreManager.get(semaKey);
                    assert sema != null : "Can not allocate a semaphore for remote target : key = " + semaKey;
                    sema.acquire();
                    if ( !client.isConnected() )
                        client.connect();
                } catch (FtpFileException ex) {
                    ex.printStackTrace();
                    return;
                }
                finally {
                    sema.release();
                }
        
                String[] content = new String[1];
                String[] target = new String[1];
                int retCode = client.getFile(mLeasePeriodMillis, content, target);
                if ( retCode < 0 ) {
                    // did not get the target, need to retry
                } else {
                    // successfully down loaded the file and the remote target removed or renamed
                }
                Thread.sleep(mPollInterval);
            } catch (Exception e) {
                e.printStackTrace();
                break;
            }
        }
    }
    
    public void stopSimulation() {
        mStop = true;
    }
    
//    private void printReason(int seq) {
//        if ( seq == -1 || mVerbose )
//            System.out.println(getReasonText(seq) + ": last FTP code:" + mFtp.getProvider().getReplyCode() + " FTP text:" + mFtp.getProvider().getReplyString());
//    }
//    
//    private String getReasonText(int seq) {
//        String reason = "Unknown";
//        switch (seq) {
//            case -1:
//                reason = "Error occurred obtaining sequence";
//                break;
//            case -2:
//                reason = "Still leased by other";
//                break;
//            case -3:
//                reason = "Can not tag the sequence file, could be already tagged by other";
//                break;
//            case -4:
//                reason = "Failed to commit the sequence increment and end the lease";
//                break;
//            case -5:
//                reason = "Can not find the sequence file";
//                break;
//            default:
//        }
//        return reason;
//    }
}
