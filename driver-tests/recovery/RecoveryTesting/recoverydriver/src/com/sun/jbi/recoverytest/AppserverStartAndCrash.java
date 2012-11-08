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
 * @(#)AppserverStartAndCrash.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.recoverytest;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.management.ManagementFactory;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Iterator;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import sun.jvmstat.monitor.HostIdentifier;
import sun.jvmstat.monitor.MonitoredHost;
import sun.jvmstat.monitor.MonitoredVm;
import sun.jvmstat.monitor.MonitoredVmUtil;
import sun.jvmstat.monitor.VmIdentifier;

/**
 *
 * @author kbhumana, mbhasin, pbhagat
 */
public class AppserverStartAndCrash extends Thread {
    
    private static Logger LOGGER = Logger.getLogger(AppserverStartAndCrash.class.getName());   
    private Properties properties;
    private Random mRandom = new Random();
    
    private int waitTimeBeforeASStartUp;
    private int waitTimeAfterASStartUp;
    private int maxRandomWaitTimeBeforeASCrash;
    
    /**
     * 
     * @param properties 
     */
    public AppserverStartAndCrash(Properties properties) {
        this.properties = properties;
        
        waitTimeBeforeASStartUp = Integer.parseInt(properties.getProperty("WaitTimeBeforeASStart"));
        waitTimeAfterASStartUp = Integer.parseInt(properties.getProperty("WaitTimeAfterASStart"));
        maxRandomWaitTimeBeforeASCrash = Integer.parseInt(properties.getProperty("MaxRandomWaitTimeBeforeASCrash"));
    }
    
    public void run() {
        do {
            crashAndStartAppserver();
        } while (true);
    }
    
    private void crashAndStartAppserver() {
        
        crashAppServerUsingServlet();
        LOGGER.log(Level.INFO, "Waiting " + waitTimeBeforeASStartUp + " secs before restarting the AppServer");
        try {
            Thread.sleep(waitTimeBeforeASStartUp * 1000);
        } catch(InterruptedException e) {
            LOGGER.log(Level.SEVERE, "Exception occurred while the thread was sleeping.", e);            
        }
        try {
            boolean killDatabase = properties.getProperty("KillDatabase").equalsIgnoreCase("yes");
            if (killDatabase) {
                Utility.startDBServer(properties);
            }
            
            Utility.startAppServer(properties);
            
            LOGGER.log(Level.INFO, "Waiting " + waitTimeAfterASStartUp + " secs for the AppServer to complete startup");
            try {
                Thread.sleep(waitTimeAfterASStartUp * 1000);
            } catch(InterruptedException e) {
                LOGGER.log(Level.SEVERE, "Exception occurred while the thread was sleeping.", e);
            }            
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, "Exception ocurred while starting the Appserver or the database server.", ex);
        }
        
        int randomWaitTimeBeforeASCrash = mRandom.nextInt(maxRandomWaitTimeBeforeASCrash);
        
        LOGGER.log(Level.INFO, "Waiting randomly for " + randomWaitTimeBeforeASCrash 
                + " secs before crashing the appserver");
        try {
            Thread.sleep(randomWaitTimeBeforeASCrash * 1000);
        } catch(InterruptedException e) {
            LOGGER.log(Level.SEVERE, "Exception occurred while the thread was sleeping.", e);
        }                
    }
    
    /*
     *Currently this is the only method that works reliably 
     */
    private void crashAppServerUsingServlet() {

        String serveletURL = "http://localhost:8080/KillServerWebApp/KillServer";
        try {

            URL url = new URL(serveletURL);
            HttpURLConnection con = (HttpURLConnection)url.openConnection();
            con.setRequestMethod("GET");
            // This call kills the appserver
            LOGGER.log(Level.INFO, "Killing Appserver from servlet");
            con.getInputStream();

        } catch(ConnectException e) {
        	//Since we are killing the appserver, the con.getInputStream() is going to
        	//throw a ConnectException, which we ignore.
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, "Exception ocurred while crashing AppServer using servlet", e);
        }
    }


    /*
     * This one does not work reliably on all platforms
     */
    private void crashAppServer1() {
        String appSrvPid = null;
        String dbSrvPid = null;
        
        try {
        	String hostId = "localhost";
            MonitoredHost localhost = MonitoredHost.getMonitoredHost(hostId);
            Set ids = new TreeSet(localhost.activeVms());
            Iterator iter = ids.iterator();
            for (; iter.hasNext();) {
                String pid = ((Integer)iter.next()).toString();
                MonitoredVm vm = null;
                try {
                    vm = localhost.getMonitoredVm(new VmIdentifier("//" + pid));
                    String name = MonitoredVmUtil.mainClass(vm, false);
                    System.out.println("MonitoredVmUtil.mainClass(vm, false) = " + name);
                    if (name.equals("NetworkServerControl")) {
                        if (properties.getProperty("DatabaseUrl").startsWith("jdbc:derby://")) {
                            dbSrvPid = pid;
                        }     
                    } else if (name.equals("PELaunch")) {
                        appSrvPid = pid;
                    }
                    
                } catch (Exception e) {
                    LOGGER.log(Level.WARNING, "Exception ocurred while getting JVM details.", e);
                }
            }
            
            if (appSrvPid != null) {
                Runtime.getRuntime().exec(new String [] {properties.getProperty("KillProcessScript"), appSrvPid});
                LOGGER.log(Level.INFO, "Killed Appserver (JVM started by class PELaunch and having pid " + appSrvPid + ")");
            } 
            
            if (dbSrvPid != null) {
                boolean killDatabase = properties.getProperty("KillDatabase").equalsIgnoreCase("yes");
                if (killDatabase) {
                    Runtime.getRuntime().exec(new String [] {properties.getProperty("KillProcessScript"), dbSrvPid});
                    LOGGER.log(Level.INFO, "Killed Derby Database (JVM started by class NetworkServerControl and having pid " + dbSrvPid + ")");
                }
            }
            
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, "Exception ocurred while crashing AppServer or the database server.", e);
        }
    }
    
    /*
     * This one does not work reliably on all platforms
     */
    private void crashAppServer2() {
    	
        String appSrvPid = null;
        String dbSrvPid = null;
    	
        try {
            File jpsStartScriptFile = new File(properties.getProperty("StartJpsScript"));
            Process jps = Runtime.getRuntime().exec(jpsStartScriptFile.getAbsolutePath());
            InputStream stdin = jps.getInputStream();
            InputStreamReader isr = new InputStreamReader(stdin);
            BufferedReader br = new BufferedReader(isr);
            String line = null;
            while ( (line = br.readLine()) != null) {
                if (line.endsWith("NetworkServerControl")) {
                    if(("yes".equalsIgnoreCase(properties.getProperty("KillDatabaseServer")))){
                        dbSrvPid = line.substring(0, line.length()-"NetworkServerControl".length()-1);
                        //Runtime.getRuntime().exec(new String [] {properties.getProperty("KillProcessScript"), appSrvPid});
                    }
                } else if (line.endsWith("PELaunch")) {
                    appSrvPid = line.substring(0, line.length()-"PELaunch".length()-1);
                    //Runtime.getRuntime().exec(new String [] {properties.getProperty("KillProcessScript"), appSvrProcId});
                }
            }
            
            if (appSrvPid != null) {
                Runtime.getRuntime().exec(new String [] {properties.getProperty("KillProcessScript"), appSrvPid});
                LOGGER.log(Level.INFO, "Killed Appserver (JVM started by class PELaunch and having pid " + appSrvPid + ")");
            } 
            
            if (dbSrvPid != null) {
                boolean killDatabase = properties.getProperty("KillDatabase").equalsIgnoreCase("yes");
                if (killDatabase) {
                    Runtime.getRuntime().exec(new String [] {properties.getProperty("KillProcessScript"), dbSrvPid});
                    LOGGER.log(Level.INFO, "Killed Derby Database (JVM started by class NetworkServerControl and having pid " + dbSrvPid + ")");
                }
            }
            
            br.close();
            isr.close();
            stdin.close();
            
        } catch (Exception e) {
            System.out.println("Exception ocurred while killing the Appserver or the database server. Cause: ");
            e.printStackTrace();
        }
    }
}
