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
 * @(#)CleanUp.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.recoverytest;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;


/**
 * @author Sun Inc
 * Nov 1, 2006
 */
public class CleanUp {
    private static Properties properties;
    private static Properties testCaseProperties;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Exception {
        if (args == null || args.length < 1) {
            System.out.println("Usage: CleanUp <properties file>");
            System.exit(0);
        }
        
        properties = new Properties();
        testCaseProperties = new Properties();
        
        properties.load(new File(args[0]).toURL().openStream());
        testCaseProperties.load(new File(properties.getProperty("TestCasePropertyFile")).toURL().openStream());
        
        cleanUpJms();
        cleanUpBpelseDB();
        cleanUpJavaService1DB();
        cleanUpOutFiles();
        
        System.out.println("Clean up completed ..");
    }
    
    private static void cleanUpOutFiles(){
        
        if (!"yes".equalsIgnoreCase(properties.getProperty("CleanUpOutputFiles"))) {
            return;
        }
        
        int i=1;
        String fileToDelete = properties.getProperty("OutputFile" + i);
        
        Utility.print("Cleaning up the output files ..");
        
        while(fileToDelete != null) {
            
            File file = new File(fileToDelete);
            try {
                file.delete();
            } catch (Exception e) {
                System.out.println("Exception occured while deleting the file " + fileToDelete);
                e.printStackTrace();
            }
            
            fileToDelete = properties.getProperty("OutputFile" + ++i);
        }
        
    }
    
    private static void cleanUpJms() throws Exception {
        if (!"yes".equalsIgnoreCase(properties.getProperty("CleanUpJms"))) {
            return;
        }
        
        String cleanUpJmsScript = properties.getProperty("CleanUpJmsScript");
        String producerDestName = testCaseProperties.getProperty("PublisherQueue");
        String consumerDestName = testCaseProperties.getProperty("ConsumerQueue");
        
        Utility.print("Cleaning all messages from the queue : " + producerDestName);
        //Process p = Runtime.getRuntime().exec(cleanUpJmsScript + " " + producerDestName);
        Process p = Runtime.getRuntime().exec(new String [] {cleanUpJmsScript, producerDestName});
        //Process p = Runtime.getRuntime().exec(cleanUpJmsScript + " " + producerDestName);
        
        OutputStream stdout = null;
        OutputStreamWriter osw = null;
        BufferedWriter bw = null;
        
        
        //TODO: The Jms server is not behaving; it gets stuck when the cleabup cammand
        //is issued. Need to figure out why.
        
        InputStream stdin = null;
        InputStreamReader isr = null;
        BufferedReader br = null;
        
        stdin = p.getInputStream();
        isr = new InputStreamReader(stdin);
        br = new BufferedReader(isr);
        
        String line = null;
        while ( (line = br.readLine()) != null) {
            System.out.println(line);
        
        }
        
        osw = new OutputStreamWriter(stdout);
        bw = new BufferedWriter(osw);
        bw.write("y");
        bw.flush();
        
        //String input = "y\n";
        //ByteArrayInputStream in = new ByteArrayInputStream(input.getBytes());
        // create threads to handle I/O streams
        //Thread th = (new IO("stdin", in, p.getOutputStream()));
        //Thread.sleep(1000);
        //th.start();
        //th.join();
        
        Utility.print("Cleaning all messages from the queue : " + consumerDestName);
        //p = Runtime.getRuntime().exec(cleanUpJmsScript + " " + consumerDestName);
        p = Runtime.getRuntime().exec(new String [] {cleanUpJmsScript, consumerDestName});
        
        stdout = p.getOutputStream();
        osw = new OutputStreamWriter(stdout);
        bw = new BufferedWriter(osw);
        bw.write("y");
        bw.flush();
        
        //p = Runtime.getRuntime().exec(cleanUpJmsScript + " " +  consumerDestName);
        //input = "y\n";
        //in = new ByteArrayInputStream(input.getBytes());
        // create threads to handle I/O streams
        //th = (new IO("stdin", in, p.getOutputStream()));
        //Thread.sleep(1000);
        //th.start();
        //th.join();
    }
    
    static class IO extends Thread {
        private InputStream in;
        private OutputStream out;
        
        IO(String name, InputStream inStream, OutputStream outStream) {
            this.in = inStream;
            this.out = outStream;
            setName(name);
        }
        
        public void run() {
            try {
                int c;
                
                while ((c = in.read()) != -1) {
                    out.write(c);
                }
                
                out.flush();
            } catch (IOException e) {
            } finally {
                if (!System.out.equals(out) && !System.err.equals(out)) {
                    // Note: in order to get an exec'd java process to
                    // see EOF on input, it is necessary to close stdin
                    if (out != null) {
                        try {
                            out.close();
                        } catch (Exception e) {
                        }
                    }
                }
            }
        }
    }
    
    private static void cleanUpBpelseDB() throws Exception {
        
        if (!"yes".equalsIgnoreCase(properties.getProperty("CleanUpDatabase"))) {
            return;
        }
        
        Statement stmt = null;
        Connection dbConn = null;
        try {
            
            dbConn = getConnection(properties.getProperty("DatabaseUrl"), properties.getProperty("DatabaseUsername"),
                    properties.getProperty("DatabasePassword"));
            
            stmt = dbConn.createStatement();
            
            Utility.print("Cleaning up the BPEL-SE database ..");
            
            stmt.execute("DELETE FROM Scope");
            stmt.execute("DELETE FROM LastCheckPoint");
            stmt.execute("DELETE FROM EngineCorrelation");
            stmt.execute("DELETE FROM InstanceCorrelation");
            stmt.execute("DELETE FROM ForEach");
            stmt.execute("DELETE FROM Variable");
            stmt.execute("DELETE FROM SimpleVariable");
            stmt.execute("DELETE FROM WaitingIMA");
            stmt.execute("DELETE FROM EventHandler");
            stmt.execute("DELETE FROM State");
            //Don't delete the engine entry
            //stmt.execute("DELETE FROM Engine");
            stmt.execute("DELETE FROM PARTNERLINK");
            stmt.execute("DELETE FROM OutstandingMsgEx");            
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (dbConn != null) {
                    dbConn.close();
                }
            } catch (SQLException ex) {
                // TODO Auto-generated catch block
                ex.printStackTrace();
            }
        }
    }
    
    private static void cleanUpJavaService1DB() throws Exception {
        
        if (!"yes".equalsIgnoreCase(properties.getProperty("CleanUpJavaService1Database"))) {
            return;
        }
        
        Statement stmt = null;
        Connection dbConn = null;
        try {
            
            dbConn = getConnection(properties.getProperty("JavaService1DatabaseUrl"),
                    properties.getProperty("JavaService1DatabaseUsername"),
                    properties.getProperty("JavaService1DatabasePassword"));
            
            stmt = dbConn.createStatement();
            
            Utility.print("Cleaning up the JavaService1DB database ..");
            
            stmt.execute("DELETE FROM JAVA_SERVICE1");
            
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (dbConn != null) {
                    dbConn.close();
                }
            } catch (SQLException ex) {
                // TODO Auto-generated catch block
                ex.printStackTrace();
            }
        }
    }
    
    private static Connection getConnection(String dbUrl, String dbUserName, String dbPassWord) throws Exception{
        if(dbUrl.startsWith("jdbc:derby://")) {
            Class driverClass = Class.forName("org.apache.derby.jdbc.ClientDriver");
            Driver driver = (Driver) driverClass.newInstance();
            DriverManager.registerDriver(driver);
            return DriverManager.getConnection(dbUrl, dbUserName, dbPassWord);
        } else if (dbUrl.startsWith("jdbc:oracle:thin:@")){
            Class driverClass = Class.forName("oracle.jdbc.driver.OracleDriver");
            Driver driver = (Driver) driverClass.newInstance();
            DriverManager.registerDriver(driver);
            return DriverManager.getConnection(dbUrl, dbUserName, dbPassWord);
        }
        return null;
    }
}
