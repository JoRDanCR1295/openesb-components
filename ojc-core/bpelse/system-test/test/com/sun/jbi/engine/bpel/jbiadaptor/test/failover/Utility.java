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

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

import java.io.InputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;

import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyTxManagerAndDataSource;

public class Utility {
    
    private static final String ENGINETHREADPOOLSIZE = "EngineThreadPoolSize";

    public static ConfigurationInfo getDeploymentConfigurations(String deploymentConfigPropFile, Class classs) throws Exception {
        InputStream is = classs.getResourceAsStream(deploymentConfigPropFile);
        Properties deployedProp = new Properties();
        deployedProp.load(is);
        
        ConfigurationInfo config = new ConfigurationInfo();
        config.setTestFolder(deployedProp.getProperty("testfolder"));
        config.setDeploymentDir(deployedProp.getProperty("deploymentDirectory"));
        config.setOutputFileName(deployedProp.getProperty("outputFileName"));
        config.setEngineThreadPoolSize(Integer.parseInt(deployedProp.getProperty(ENGINETHREADPOOLSIZE, "1")));
        
        Map messageInComingKeyMap = new HashMap();
        Map messageIdToInputMessageMap = new HashMap();
        Map outputKeyToOutputMessageMap = new HashMap();
        Map messageEndPointConnectionMap = new HashMap();
        
        Enumeration enums = deployedProp.keys();
        String key = null;
        String message = null;
        String output = null;
        
        while (enums.hasMoreElements()) {
            key = (String) enums.nextElement();

            if (key.startsWith("Input")) {
                message = (String) deployedProp.get(key);
                Object[] objs = parseMessageProperty(message);
                InComingKey inComingKey = (InComingKey)objs[0];
                messageInComingKeyMap.put(key, inComingKey);
                
                String inputFileName = (String)objs[1];
                String messageQName = (String)objs[2];
                
                if (null != inputFileName && null != messageQName) {
                    MessageInfo messageInfo = new MessageInfo(messageQName, inputFileName);
                    messageIdToInputMessageMap.put(key, messageInfo);
                } else {
                    throw new Exception("Please sepecify the input file");
                }
            } else if (key.startsWith("Output")) {
                output = (String) deployedProp.get(key);
                Object[] objs = parseMessageProperty(output);
                InComingKey outgoingKey = (InComingKey)objs[0];
                String outputFileName = (String)objs[1];
                
                if (null != outputFileName) {
                    outputKeyToOutputMessageMap.put(outgoingKey, outputFileName);
                } else {
                    throw new Exception("Please sepecify the output file");
                }
            } else if (key.startsWith("Connection")) {
                message = (String) deployedProp.get(key);
                if (key.contains("from")) {
                    InComingKey inComingKeyFrom = getInComingKey(message);
                    String incomingModelToKey = key.substring(0, key.indexOf(".")) + ".to";
                    String incomingModelToValue = (String) deployedProp.get(incomingModelToKey);
                    InComingKey inComingKeyTo = getInComingKey(incomingModelToValue);
                    messageEndPointConnectionMap.put(inComingKeyFrom, inComingKeyTo);
                }
            }
        }

        config.setMessageInComingKeyMap(messageInComingKeyMap);
        config.setMessageToInputFileMap(messageIdToInputMessageMap);
        config.setOutputKeyToOutputMessageMap(outputKeyToOutputMessageMap);
        config.setMessageEndPointConnectionMap(messageEndPointConnectionMap);
        return config;
    }
    
    private static InComingKey getInComingKey(String message) {
        if (message == null || message.equals("")) {
            throw new RuntimeException("Invalid Property File");
        }
        DeploymentBindings depBindings = new DeploymentBindings();
        InComingKey inComingKey = null;
        
        StringTokenizer st = new StringTokenizer(message, ",");
        QName service = QName.valueOf(st.nextToken().trim());
        String endpoint = st.nextToken().trim();
        String operation = st.nextToken().trim();
        
        inComingKey = depBindings.createInComingBindingsKey(service, endpoint, operation);
        return inComingKey;
    }
    
    private static Object[] parseMessageProperty(String message) {
        if (message == null || message.equals("")) {
            throw new RuntimeException("Invalid Property File");
        }
        DeploymentBindings depBindings = new DeploymentBindings();
        InComingKey endpointKey = null;
        
        StringTokenizer st = new StringTokenizer(message, ",");
        QName service = QName.valueOf(st.nextToken().trim());
        String endpoint = st.nextToken().trim();
        String operation = st.nextToken().trim();
        
        endpointKey = depBindings.createInComingBindingsKey(service, endpoint, operation);
        
        String fileName = st.nextToken().trim();
        
        String messageQName = null;
        if (st.hasMoreTokens()) {
            messageQName = st.nextToken().trim();
        }
        
        return new Object[] {endpointKey, fileName, messageQName};
    }
    
    public static String getEngineIds(List enginesList) {
        Iterator iter = enginesList != null ? enginesList.iterator() : null;
        StringBuffer engIds = new StringBuffer();
        int i = 1;

        if (iter != null && enginesList.size() > 0) {
            while (iter.hasNext()) {
                if (i > 1)
                    engIds.append(",");
                //engIds.append("'" + ((EngineSimulator) iter.next()).getEngine().getId() + "'");
                engIds.append("'" + (((EngineInfo) iter.next())).getEngine().getEngine().getId() + "'");
                i++;
            }
        } else {
            engIds.append("''");
        }

        return engIds.toString();
    }

    public static int getCount(String query) throws SQLException {
        int count = -1;

        Connection con = getConnection();
        ResultSet rs = null;
        Statement stmt = null;
        
        try {
            stmt = con.createStatement();
            rs = stmt.executeQuery(query);
            while (rs.next()) {
                count = rs.getInt(1);
            }
        } catch (SQLException e) {
            throw e;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
            if (rs != null) {
                rs.close();
            }
            if (con != null) {
                con.close();
            }
        }
        return count;
    }
    
    public static void doCleanUP(String engIds) throws SQLException {
        Connection con = null;
        Statement stmt = null;
        try {
            con = getConnection();
            con.setAutoCommit(false);
            stmt = con.createStatement();
            stmt.execute("update STATE set STATUS = 'DONE' where ENGINEID IN (" + engIds + ")");
            con.commit();
        } finally {
            if (stmt != null) {
                stmt.close();
            }
            if (con != null) {
                con.close();
            }
        }
    }

    public static void doCleanupPersisteneDB() {
        Connection con = null;
        Statement stmt = null;
        try {
            con = getConnection();
            con.setAutoCommit(true);
            stmt = con.createStatement();
            stmt.execute("delete from OUTSTANDINGMSGEX");
            stmt.execute("delete from SCOPE");
            stmt.execute("delete from CRMP");
            stmt.execute("delete from VARIABLE");
            stmt.execute("delete from EVENTHANDLER");
            stmt.execute("delete from LASTCHECKPOINT");
            stmt.execute("delete from INSTANCECORRELATION");
            stmt.execute("delete from ENGINECORRELATION");
            stmt.execute("delete from WAITINGIMA");
            stmt.execute("delete from FOREACH");
            stmt.execute("delete from SIMPLEVARIABLE");
            stmt.execute("delete from STATE");
            stmt.execute("delete from ENGINE");
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }
    
    public static void destroyConnectionPool() {
        DummyTxManagerAndDataSource tm = (DummyTxManagerAndDataSource) BPELSERegistry.getInstance().lookup(
                TransactionManager.class.getName());
        tm.destroyConnectionPool();
    }


    public static Connection getConnection() throws SQLException {
        DummyTxManagerAndDataSource tm = (DummyTxManagerAndDataSource) BPELSERegistry.getInstance().lookup(
                TransactionManager.class.getName());
        return tm.getNonTxConnection();
    }
}
