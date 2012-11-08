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
 * @(#)SequenceClient.java 
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

import com.sun.jbi.ftpbc.ftp.exception.ConfigurationException;
import com.sun.jbi.ftpbc.ftp.exception.FtpInterfaceException;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import java.util.Properties;

/**
 *
 * @author jfu
 */
public class SequenceClient implements Runnable, Stoppable {
    public static final String P_MAX_RETRY = "max_retry";
    public static final String P_RETRY_INTERVAL = "retry_interval_millis";
    public static final String P_FETCH_INTERVAL = "fetch_interval_millis";
    
    private FtpInterface mFtp;
    private String mSeqDir;
    private String mSeqName;
    private long mRetryInterval;
    private int mRetryMax;
    private long mFetchInterval;
    private int mLeasePeriodMillis;
    private String mThreadDesc;
    private boolean mVerbose;
    private boolean mStop;
    
    /**
     * Creates a new instance of SequenceClient
     */
    public SequenceClient(
            Properties ftpParams,
            Properties simulationParams)
            throws Exception {
        mSeqDir = ftpParams.getProperty(FtpFileConfigConstants.P_TGT_DIR);
        mSeqName = ftpParams.getProperty(FtpFileConfigConstants.P_TGT_FILE);
        
        String str = simulationParams.getProperty(P_MAX_RETRY);
        if ( Driver.isEmpty(str) ) {
            Driver.throwMissingParam(P_MAX_RETRY);
        } else {
            mRetryMax = Driver.getInteger(str);
            if ( mRetryMax <= 0 ) {
                Driver.throwInvalidParam(P_MAX_RETRY, str);
            }
        }
        
        str = simulationParams.getProperty(P_RETRY_INTERVAL);
        if ( Driver.isEmpty(str) ) {
            Driver.throwMissingParam(P_RETRY_INTERVAL);
        } else {
            mRetryInterval = Driver.getLong(str);
            if ( mRetryInterval <= 0 ) {
                Driver.throwInvalidParam(P_RETRY_INTERVAL, str);
            }
        }
        
        str = simulationParams.getProperty(P_FETCH_INTERVAL);
        if ( Driver.isEmpty(str) ) {
            Driver.throwMissingParam(P_FETCH_INTERVAL);
        } else {
            mFetchInterval = Driver.getLong(str);
            if ( mFetchInterval <= 0 ) {
                Driver.throwInvalidParam(P_FETCH_INTERVAL, str);
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
        
        mFtp = new FtpInterface();
        try {
            mFtp.initialize(ftpParams);
        } catch (ConfigurationException ex) {
            ex.printStackTrace();
            mFtp = null;
        } catch (FtpInterfaceException ex) {
            ex.printStackTrace();
            mFtp = null;
        }
    }
    
    public void run() {
        mStop = false;
        mThreadDesc = Thread.currentThread().getName() + ":" + Thread.currentThread().getId();
        int seq = -1;
        int retry = 0;
        
        if ( mFtp == null ) {
            System.out.println("Failed to initialize client in [" + mThreadDesc + "]");
            return;
        }

        mFtp.getProvider().setUseRegexAsMatcher(true);
        FtpFileClient client = mFtp.getClient();
        
        try {
            client.connect();
        } catch (FtpFileException ex) {
            ex.printStackTrace();
            return;
        }
        
        while ( true ) {
            seq = -1;
            retry = 0;
            if ( mStop )
                break;
            try {
                while ( seq < 0 ) {
                    seq = client.getSequence(mLeasePeriodMillis);
                    if ( seq >= 0 ) {
                        // successfully obtained seq
                        System.out.println(mThreadDesc + "==>" + mSeqName + "=" + seq);
                        break;
                    } else if ( seq == -1 ) {
                        // error
                        printReason(seq);
                        break;
                    } else {
                        printReason(seq);
                        if ( retry >= mRetryMax ) {
                            System.out.println(mThreadDesc + ": Failed to obtain seq number, seq name=[" + mSeqName + "] seq=[" + seq + "] : Exceed retryMax = [" + mRetryMax + "]");
                            break;
                        }
                        Thread.sleep(mRetryInterval);
                    }
                }
                
                mFtp.reset();
                Thread.sleep(mFetchInterval);
            } catch (Exception e) {
                e.printStackTrace();
                System.out.println(mThreadDesc + "==> Exit");
                break;
            }
        }
        client.disconnect();
    }
    
    public void stopSimulation() {
        mStop = true;
    }
    
    private void printReason(int seq) {
        if ( seq == -1 || mVerbose )
            System.out.println(getReasonText(seq) + ": last FTP code:" + mFtp.getProvider().getReplyCode() + " FTP text:" + mFtp.getProvider().getReplyString() + "seqName=" + mSeqName + " seqVal=" + seq);
    }
    
    private String getReasonText(int seq) {
        String reason = "Unknown";
        switch (seq) {
            case -1:
                reason = "Error occurred obtaining sequence";
                break;
            case -2:
                reason = "Still leased by other";
                break;
            case -3:
                reason = "Can not tag the sequence file, could be already tagged by other";
                break;
            case -4:
                reason = "Failed to commit the sequence increment and end the lease";
                break;
            case -5:
                reason = "Can not find the sequence file";
                break;
            default:
        }
        return reason;
    }
}
