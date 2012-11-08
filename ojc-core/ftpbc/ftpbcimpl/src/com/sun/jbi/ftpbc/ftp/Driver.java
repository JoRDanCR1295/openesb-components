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
 * @(#)Driver.java 
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

import java.io.FileInputStream;
import java.util.Iterator;
import java.util.Properties;
import java.util.Vector;

/**
 *
 * @author jfu
 */
public class Driver {
    public static final String P_SIMULATION_DURATION = "simulation_duration_millis";
    public static final String P_HOST = "host";
    public static final String P_PORT = "port";
    public static final String P_USER = "user";
    public static final String P_PASSWORD = "password";
    public static final String P_LIST_STYLE = "list_style";
    public static final String P_TARGET_DIR = "targetdir";
    public static final String P_TARGET_FILE = "targetfile";
    public static final String P_LEASE_PERIOD = "lease_period_millis";
    public static final String P_THREAD_COUNT = "thread_count";
    public static final String P_VERBOSE = "verbose";
    
    public Driver() {
    }
    
    public static void main(String[] args) {
        if ( args.length != 2 ) {
            printUsage();
            System.exit(0);
        }
        
        boolean simulatePoller = false;
        long duration = 0;
        int port = 21;
        int threadCount = 0;
        long leasePeriod = 0;
        
        Properties simulationParams = new Properties();
        
        String option = args[0];
        String paramFilePath = args[1];
        
        if ( option.equalsIgnoreCase("-P") ) {
            simulatePoller = true;
        } else if ( option.equalsIgnoreCase("-S") ) {
            simulatePoller = false;
        } else {
            System.out.println("Invalid option :" + option);
            printUsage();
            System.exit(0);
        }
        
        System.out.println("Loading parameters for FTP client...from file: " + paramFilePath);
        
        try {
            simulationParams.load(new FileInputStream(paramFilePath));
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(0);
        }
        
        Properties ftpParams = getDefaultParameters();
        
        ftpParams.setProperty(FtpFileConfigConstants.C_P_FTP_HOST, simulationParams.getProperty(Driver.P_HOST));
        ftpParams.setProperty(FtpFileConfigConstants.C_P_FTP_PORT, simulationParams.getProperty(Driver.P_PORT));
        ftpParams.setProperty(FtpFileConfigConstants.C_P_FTP_USR, simulationParams.getProperty(Driver.P_USER));
        ftpParams.setProperty(FtpFileConfigConstants.C_P_FTP_PASSWD, simulationParams.getProperty(Driver.P_PASSWORD));
        ftpParams.setProperty(FtpFileConfigConstants.C_P_FTP_LST_STYLE, simulationParams.getProperty(Driver.P_LIST_STYLE));
        
        String str = simulationParams.getProperty(Driver.P_SIMULATION_DURATION);
        if ( isEmpty(str) ) {
            reportMissingParam(Driver.P_SIMULATION_DURATION);
            System.exit(0);
        } else {
            duration = getLong(str);
            if ( duration <= 0 ) {
                reportInvalidParam(Driver.P_SIMULATION_DURATION, str);
                System.exit(0);
            }
        }
        
        str = simulationParams.getProperty(Driver.P_TARGET_DIR);
        if ( isEmpty(str) ) {
            reportMissingParam(Driver.P_TARGET_DIR);
            System.exit(0);
        } else {
            ftpParams.setProperty(FtpFileConfigConstants.P_TGT_DIR, simulationParams.getProperty(Driver.P_TARGET_DIR));
        }
        
        str = simulationParams.getProperty(Driver.P_TARGET_FILE);
        if ( isEmpty(str) ) {
            reportMissingParam(Driver.P_TARGET_FILE);
            System.exit(0);
        } else {
            ftpParams.setProperty(FtpFileConfigConstants.P_TGT_FILE, simulationParams.getProperty(Driver.P_TARGET_FILE));
        }
        
        str = simulationParams.getProperty(Driver.P_LEASE_PERIOD);
        if ( isEmpty(str) ) {
            reportMissingParam(Driver.P_LEASE_PERIOD);
            System.exit(0);
        } else {
            leasePeriod = getLong(str);
            if ( leasePeriod <= 0 ) {
                reportInvalidParam(Driver.P_LEASE_PERIOD, str);
                System.exit(0);
            }
        }
        str = simulationParams.getProperty(Driver.P_THREAD_COUNT);
        if ( isEmpty(str) ) {
            reportMissingParam(Driver.P_THREAD_COUNT);
            System.exit(0);
        } else {
            threadCount = getInteger(str);
            if ( threadCount <= 0 ) {
                reportInvalidParam(Driver.P_THREAD_COUNT, str);
                System.exit(0);
            }
        }
        
        Vector clients = new Vector();
        Vector simulators = new Vector();
        if ( option.equalsIgnoreCase("-S") ) {
            // simulate concurrent sequence requesters
            for ( int i = 0; i < threadCount; i++ ) {
                try {
                    // set parameters
                    Runnable target =
                            option.equalsIgnoreCase("-S") ?
                                new SequenceClient(ftpParams, simulationParams) : new PollerClient(ftpParams, simulationParams);
                    simulators.add(target);
                    clients.add(new Thread(target));
                } catch (Exception ex) {
                    ex.printStackTrace();
                    System.exit(0);
                }
            }
            Iterator it = clients.iterator();
            while ( it.hasNext() )
                ((Thread)it.next()).start();
            
        } else {
            // simulate concurrent pollers
        }
        
        
        long startStamp = System.currentTimeMillis();
        while ( true ) {
            try {
                Thread.sleep(5000);
            } catch (InterruptedException ex) {
                ex.printStackTrace();
            }
            if ( System.currentTimeMillis() - startStamp > duration ) {
                System.out.println("Duration of simulation exceeded, notifying simulation threads ......");
                Iterator it = simulators.iterator();
                while ( it.hasNext() )
                    ((Stoppable)it.next()).stopSimulation();
                // watch them go away ...
                it = clients.iterator();
                while ( it.hasNext() ) {
                    Thread t = (Thread)it.next();
                    if ( !t.isAlive() )
                        clients.remove(t);
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException ex) {
                        ex.printStackTrace();
                    }
                }
                System.out.println("All simulation threads stopped, main thread exiting ......");
                break;
            }
        }
    }
    
    public static long getLong(String s) {
        long l = -1;
        try {
            l = Long.parseLong(s);
        } catch (Exception e) {
        }
        return l;
    }
    
    public static int getInteger(String s) {
        int i = -1;
        try {
            i = Integer.parseInt(s);
        } catch (Exception e) {
            
        }
        return i;
    }
    
    private static Properties getDefaultParameters() {
        Properties prop = new Properties();
        prop.put(FtpFileConfigConstants.P_COLLAB_OID, "placeholder");
        prop.put(FtpFileConfigConstants.P_CONN_NAME, "placeholder");
        // section "General Settings"
        prop.put(FtpFileConfigConstants.P_GEN_TRNS_TYPE, "Non-Transactional");
        prop.put(FtpFileConfigConstants.P_GEN_BASE_LOC, "");
        prop.put(FtpFileConfigConstants.C_P_GEN_SYNC, "No");
        
        // section "FTP"
        
        /** FileZilla
         * prop.put(FtpFileConfigConstants.C_P_FTP_LST_STYLE, "UNIX");
         * prop.put(FtpFileConfigConstants.C_P_FTP_HOST, "jfu-d600xp.stc.com");
         * prop.put(FtpFileConfigConstants.C_P_FTP_USR, "anonymous");
         * prop.put(FtpFileConfigConstants.C_P_FTP_PASSWD, "abc@yahoo.com");
         */
        
        /* Windows IIS ftp service
        prop.put(FtpFileConfigConstants.C_P_FTP_LST_STYLE, "NT 4.0");
        prop.put(FtpFileConfigConstants.C_P_FTP_HOST, "jfu-d600xp.stc.com");
        prop.put(FtpFileConfigConstants.C_P_FTP_USR, "anonymous");
        prop.put(FtpFileConfigConstants.C_P_FTP_PASSWD, "abc@yahoo.com");
         */
        
        prop.put(FtpFileConfigConstants.C_P_FTP_LST_STYLE, "UNIX");
        prop.put(FtpFileConfigConstants.C_P_FTP_HOST, "jfu-gx270xp.stc.com");
        prop.put(FtpFileConfigConstants.C_P_FTP_USR, "user1");
        prop.put(FtpFileConfigConstants.C_P_FTP_PASSWD, "user1");
        
        prop.put(FtpFileConfigConstants.C_P_FTP_UDH_CFG, "");
        prop.put(FtpFileConfigConstants.C_P_FTP_UDH_LST_STYLE, "");
        
        prop.put(FtpFileConfigConstants.C_P_FTP_PORT, new Long(21));
        prop.put(FtpFileConfigConstants.P_FTP_MODE, "ASCII");
        
        prop.put(FtpFileConfigConstants.P_FTP_CMD_TIMEOUT, new Long(45000));
        prop.put(FtpFileConfigConstants.P_FTP_DAT_TIMEOUT, new Long(45000));
        
        prop.put(FtpFileConfigConstants.C_P_SOC_ON, "No");
        prop.put(FtpFileConfigConstants.C_P_SOC_HOST, "");
        prop.put(FtpFileConfigConstants.C_P_SOC_PORT, new Long(1080));
        prop.put(FtpFileConfigConstants.C_P_SOC_USR, "");
        prop.put(FtpFileConfigConstants.C_P_SOC_PASSWD, "");
        prop.put(FtpFileConfigConstants.C_P_SOC_VER, "Unknown");
        
        prop.put(FtpFileConfigConstants.P_STAGE_ENABLED,  "No");
        
        prop.put(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON, "Yes");
        
        prop.put(FtpFileConfigConstants.P_TGT_DIR, "placeholder");
        prop.put(FtpFileConfigConstants.P_TGT_FILE, "placeholder");
        prop.put(FtpFileConfigConstants.P_TGT_APPND, "No");
        
        prop.put(FtpFileConfigConstants.P_PRE_CMD, "None");
        prop.put(FtpFileConfigConstants.P_POST_CMD, "None");
        
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
    
    public static boolean isEmpty(String s) {
        if ( s == null || s.trim().length() == 0 )
            return true;
        return false;
    }
    
    public static void reportMissingParam(String s) {
        System.out.println("Missing parameter: " + s);
    }
    
    public static void reportInvalidParam(String s, String v) {
        System.out.println("Invalid parameter: " + s + " value=" + v);
    }
    
    public static void throwMissingParam(String s) throws Exception {
        throw new Exception("Missing parameter: " + s);
    }
    
    public static void throwInvalidParam(String s, String v) throws Exception {
        throw new Exception("Invalid parameter: " + s + " value=" + v);
    }
    
    private static void printUsage() {
        System.out.println("client.Driver <option> <configuration-file>");
        System.out.println("where:");
        System.out.println("<option> is either -s (simulation of concurrent sequence requesters) or -p (simulation of concurrent file pollers).");
        System.out.println("<configuration-file> contains configurable parameters for the corresponding simulation option.");
        System.out.println("Note, different simulation option requires different set of parameters,");
        System.out.println("check out provided sample configuration files for details for each simulation option.");
    }
}
