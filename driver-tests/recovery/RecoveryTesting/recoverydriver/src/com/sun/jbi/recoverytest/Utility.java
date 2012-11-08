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
 * @(#)Utility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.recoverytest;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;


public class Utility {

    private static Logger LOGGER = Logger.getLogger(Utility.class.getName());

    public static void startAppServer(Properties props) throws Exception {
        LOGGER.log(Level.INFO, "Starting AppsServer ...");
        File startAppServerScriptFile = new File(props.getProperty("StartAppServerScript"));
        Runtime.getRuntime().exec(startAppServerScriptFile.getAbsolutePath() +
        " >> " + props.getProperty("StartAppServerStatus"));
    }

    public static void startDBServer(Properties props) throws Exception {
        LOGGER.log(Level.INFO, "Starting database server ...");
        File startDatabaseServerScriptFile = new File(props.getProperty("StartDatabaseServerScript"));
        Runtime.getRuntime().exec(startDatabaseServerScriptFile.getAbsolutePath() +
        " >> " + props.getProperty("StartDatabaseServerStatus"));        
    }

    public static void startJMSServer(Properties props) throws IOException {
        LOGGER.log(Level.INFO, "Starting JMS server ...");
        File startDatabaseServerScriptFile = new File(props.getProperty("StartJmsServerScript"));
        Runtime.getRuntime().exec(startDatabaseServerScriptFile.getAbsolutePath() +
        " >> " + props.getProperty("StartJmsServerStatus"));
    }

    /**
     * @param dbURL
     * @param dbUserName
     * @param dbPassWord
     */
    public static void deleteDataFromDB(String dbURL, String dbUserName, String dbPassWord) {
        
        //Currently the Derby database cannot be cleaned up because of some bug
        //in Derby
        if (dbURL.startsWith("jdbc:derby://")) {
            //return;
        }        
        
        Connection conn = null;
        Statement stmt = null;
        ResultSet selectStateDoneRS = null;
        PreparedStatement deleteCrmp = null;
        PreparedStatement deleteFHD = null;
        PreparedStatement deleteLCP = null;
        PreparedStatement deleteEngineCorr = null;
        PreparedStatement deleteInstCorr = null;
        PreparedStatement deleteScopeSnapshot = null;
        PreparedStatement deleteForEach = null;
        PreparedStatement deleteVar = null;
        PreparedStatement deleteWaitingIMA = null;
        PreparedStatement deleteState = null;
        
        try {
            
            if(dbURL.startsWith("jdbc:derby://")) {
                Class driverClass = Class.forName("org.apache.derby.jdbc.ClientDriver");
                Driver driver = (Driver) driverClass.newInstance();
                DriverManager.registerDriver(driver);
            } else if (dbURL.startsWith("jdbc:oracle:thin:@")){
                Class driverClass = Class.forName("oracle.jdbc.driver.OracleDriver");
                Driver driver = (Driver) driverClass.newInstance();
                DriverManager.registerDriver(driver);
            }
            
            conn = DriverManager.getConnection(dbURL, dbUserName, dbPassWord);
            conn.setAutoCommit(false);
            
            stmt = conn.createStatement();
            selectStateDoneRS = stmt.executeQuery("Select StateId from State where State.Status='DONE'");
            
            deleteCrmp = conn.prepareStatement("Delete from Crmp where Crmp.StateId = ?");
            deleteFHD = conn.prepareStatement("Delete from FaultHandlerData where FaultHandlerData.StateId = ?");
            deleteLCP = conn.prepareStatement("Delete from LastCheckPoint where LastCheckPoint.StateId = ?");
            deleteEngineCorr = conn.prepareStatement("Delete from EngineCorrelation where EngineCorrelation.BpelId = ?");
            deleteInstCorr = conn.prepareStatement("Delete from InstanceCorrelation where InstanceCorrelation.StateId = ?");
            deleteScopeSnapshot = conn.prepareStatement("Delete from ScopeSnapshot where ScopeSnapshot.StateId = ?");
            deleteForEach = conn.prepareStatement("Delete from ForEach where ForEach.StateId = ?");
            deleteVar = conn.prepareStatement("Delete from Variable where Variable.StateId = ?");
            deleteVar = conn.prepareStatement("Delete from SimpleVariable where SimpleVariable.StateId = ?");
            deleteWaitingIMA = conn.prepareStatement("Delete from WaitingIMA where WaitingIMA.StateId = ?");
            deleteState = conn.prepareStatement("Delete from State where State.StateId = ?");
            
            while(selectStateDoneRS.next()){
                
                String doneId = selectStateDoneRS.getString("StateId");
                
                deleteCrmp.setString(1, doneId);
                deleteCrmp.execute();
                
                deleteFHD.setString(1, doneId);
                deleteFHD.execute();
                
                deleteLCP.setString(1, doneId);
                deleteLCP.execute();
                
                deleteEngineCorr.setString(1, doneId);
                deleteEngineCorr.execute();
                
                deleteInstCorr.setString(1, doneId);
                deleteInstCorr.execute();
                
                deleteScopeSnapshot.setString(1, doneId);
                deleteScopeSnapshot.execute();
                
                deleteForEach.setString(1, doneId);
                deleteForEach.execute();
                
                deleteVar.setString(1, doneId);
                deleteVar.execute();
                
                deleteWaitingIMA.setString(1, doneId);
                deleteWaitingIMA.execute();
                
                deleteState.setString(1, doneId);
                deleteState.execute();
                
                conn.commit();
            }
            
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        } finally {
            try {
                
                if (deleteCrmp != null) { deleteCrmp.close(); }
                if (deleteFHD != null) { deleteFHD.close(); }
                if (deleteLCP != null) { deleteLCP.close(); }
                if (deleteEngineCorr != null) { deleteEngineCorr.close(); }
                if (deleteInstCorr != null) { deleteInstCorr.close(); }
                if (deleteScopeSnapshot != null) { deleteScopeSnapshot.close(); }
                if (deleteForEach != null) { deleteForEach.close(); }
                if (deleteVar != null) { deleteVar.close(); }
                if (deleteWaitingIMA != null) { deleteWaitingIMA.close(); }
                if (deleteState != null) { deleteState.close(); }
                
                if (selectStateDoneRS != null) {
                    selectStateDoneRS.close();
                }
                
                if (stmt != null) {
                    stmt.close();
                }
                
                if (conn != null) {
                    conn.close();
                }
                
            } catch (SQLException ex) {
                LOGGER.log(Level.WARNING, "Exception while closing connection, statement or prepared statement", ex);
            }
        }
    }
    
    /**
     * @param message
     */
    public static void print(String message) {
        print(message, null);
    }
    
    /**
     * @param message
     * @param value
     */
    public static void print(String message, String value) {
        if (value == null) {
            LOGGER.log(Level.INFO, message);
            return;
        }
        LOGGER.log(Level.INFO, message + " : " + value);        
    }
    
    public static String readTextFromFile(String fileName, String srcEncoding)
        throws IOException {
        InputStream inputStream = null;
        String ret = null;
        try {
            inputStream = new FileInputStream(fileName);
            InputStreamReader in = new InputStreamReader(inputStream, srcEncoding);
            StringWriter out = new StringWriter();
            
            char[] buf = new char[512];
            int n = 0;
            while ((n = in.read(buf)) != -1) {
                out.write(buf, 0, n);
            }
            out.flush();
            ret = out.toString();
        } catch (IOException e) {
            throw e;
        } finally {
            if (inputStream != null) {
                inputStream.close();
            }
        }
        //mLog.debug("ret: " + ret);
        return ret;
    }    
    
    public static void writeTextToFile(String fileName, String text, String encoding) 
        throws Exception {
        OutputStream outputStream = null;
        try {
            outputStream = new FileOutputStream(fileName);
            OutputStreamWriter out = new OutputStreamWriter(outputStream, encoding);
            StringReader in = new StringReader(text);
            char[] buf = new char[512];
            int n = 0;
            while ((n = in.read(buf)) != -1) {
                out.write(buf, 0, n);
            }
            out.flush();
        }catch (IOException e) {
            throw e;
        } finally {
            if (outputStream != null) {
                outputStream.close();
            }
        }
        
    } 
    
        
    public static String replaceAll(String s, String match, String replacement) {
        StringBuffer sb = new StringBuffer();
        String temp = s;
        while (true) {
            int i = temp.indexOf(match);
            if (i < 0) {
                sb.append(temp);
                return sb.toString();
            }
            sb.append(temp.substring(0, i));
            sb.append(replacement);
            temp = temp.substring(i + match.length());
        }
    }
}
