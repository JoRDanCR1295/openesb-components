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
 * @(#)DBManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.plugins.bpelse.datamodel.*;
import com.sun.jbi.cam.services.configuration.ConfigurationService;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;

import java.util.logging.Logger;
import java.util.List;
import java.util.ArrayList;
import java.sql.Connection;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.Map;
import javax.naming.InitialContext;
import javax.sql.DataSource;

/**
 *
 * @author nnahata
 */
public class DBManager extends BaseBean{
    
    private static final String SELECT_STATE_QUERY = 
            "select * from BPELSE_SCHEMA.STATE";
    private Logger logger = 
            Logger.getLogger(DBManager.class.getName());
    
    private DataSource ds = null;
    private Connection genericConn;


    // BPEL DB connection properties
    private String serverName;
    private int portNumber;
    private String databaseOrSchemaName;
    private String username;
    private String password;
    

    
    
    /** Creates a new instance of DBManager */
    public DBManager() {
        initDataSource();
        
    }
    
     public List<State> getBPInstances(){
        List<State> states = new ArrayList<State>();
        Connection conn = null;
        try {
            conn = ds.getConnection();
            Statement stmt = conn.createStatement();
            ResultSet rs = stmt.executeQuery(SELECT_STATE_QUERY);
            while (rs.next() ){
                String stateid = rs.getString("stateid");
                String engineid = rs.getString("engineid");
                String bpelid = rs.getString("bpelid");
                String status = rs.getString("status");
                State state = new State(stateid, bpelid, engineid, status);
                states.add(state);
            }
        }
        catch(Exception e) {
            e.printStackTrace();
        }finally{
            if(conn != null) {
                try {
                    conn.close();
                } catch (SQLException ex) {
                }
            }
        }
        
        return states;
    }
    
    public List<State> getBPInstances(String suName){
        if(suName == null || suName.length() == 0) {
            return getBPInstances();
        }
        List<State> states = new ArrayList<State>();
        Connection conn = null;
        try {
            conn = ds.getConnection();
            Statement stmt = conn.createStatement();
            ResultSet rs = stmt.executeQuery(SELECT_STATE_QUERY+ " " +
                    "where BPELID like" + "'%" + suName +"%'");
            while (rs.next() ){
                String id = rs.getString("stateid");
                String engineid = rs.getString("engineid");
                String bpelid = rs.getString("bpelid");
                String status = rs.getString("status");
                State state = new State(id, bpelid, engineid, status);
                states.add(state);
            }
        }
        catch(Exception e) {
            e.printStackTrace();
        }finally{
            if(conn != null) {
                try {
                    conn.close();
                } catch (SQLException ex) {
                }
            }
        }
        
        return states;
    }

    public List<BigDecimal> getNumericVarValues(String bpid){
        List<BigDecimal> numericVarValues = new ArrayList<BigDecimal>();
        List<SimpleVariable> listSimpleVars = getAllSimpleVars(bpid);
        for(int i=0; i< listSimpleVars.size(); i++ ){
            SimpleVariable svar = listSimpleVars.get(i);
            if (svar.getNumericValue() != null ){
                numericVarValues.add(svar.getNumericValue());
            }
        }
        return numericVarValues;
    }
    
    public List<SimpleVariable> getNumericVars(String bpid){
        List<SimpleVariable> numericVars = new ArrayList<SimpleVariable>();
        List<SimpleVariable> listSimpleVars = getAllSimpleVars(bpid);
        for(int i=0; i< listSimpleVars.size(); i++ ){
            SimpleVariable svar = listSimpleVars.get(i);
            if (svar.getNumericValue() != null ){
                numericVars.add(svar);
            }
        }
        return numericVars;
    }

    public List<SimpleVariable> getStringVars(String bpid){
        List<SimpleVariable> stringVars = new ArrayList<SimpleVariable>();
        List<SimpleVariable> listSimpleVars = getAllSimpleVars(bpid);
        for(int i=0; i< listSimpleVars.size(); i++ ){
            SimpleVariable svar = listSimpleVars.get(i);
            if (svar.getStringValue() != null ){
                stringVars.add(svar);
            }
        }
        return stringVars;
    }

    public List<SimpleVariable> getBooleanVars(String bpid){
        List<SimpleVariable> booleanVars = new ArrayList<SimpleVariable>();
        List<SimpleVariable> listSimpleVars = getAllSimpleVars(bpid);
        for(int i=0; i< listSimpleVars.size(); i++ ){
            SimpleVariable svar = listSimpleVars.get(i);
            if (svar.getBooleanValue() != null ){
                booleanVars.add(svar);
            }
        }
        return booleanVars;
    }
        
    public List<SimpleVariable> getAllSimpleVars(String bpid){
        List<SimpleVariable> vars = new ArrayList<SimpleVariable>();
        Connection conn = null;
        try {
            conn = ds.getConnection();
            Statement stmt = conn.createStatement();
            String query = "select * from BPELSE_SCHEMA.VARIABLESIMPLE " +
    		"where bpid=" + "'" + bpid + "'";
            System.out.println(">>>>>>>>>>>>> getSimpleVarsByBPID: query" + query);
            ResultSet rs = stmt.executeQuery( query );
            while (rs.next() ){
                long varid = rs.getLong("id");
                
                String stringVal = rs.getString("stringvalue");
                BigDecimal numericVal = rs.getBigDecimal("numericvalue");
                String bolStringVal = rs.getString("booleanvalue");
                Boolean booleanVal = new Boolean(bolStringVal);
                Timestamp timestampVal = rs.getTimestamp("timeValue");




                SimpleVariable var = new SimpleVariable(bpid, varid, stringVal,
                        numericVal, booleanVal,timestampVal);
                
                vars.add(var);
            }
        }
        catch(Exception e) {
            e.printStackTrace();
        }finally{
            if(conn != null) {
                try {
                    conn.close();
                } catch (SQLException ex) {
                }
            }
        }
        
        return vars;
    }
 
    public ResultSet  execGenericQuery(String query){
        try {
            genericConn = ds.getConnection();
            Statement stmt = genericConn.createStatement();
            ResultSet rs = stmt.executeQuery( query );
            return rs;
        }
        catch(Exception e) {
            e.printStackTrace();
        }
        
        return null;
    }
 
    public void closeGenericConnnection() {
        try {
            genericConn.close();
        } catch (SQLException ex) {
            ex.printStackTrace();
        }
        genericConn = null;
    }
    
    public List<String> getInstancesIDDoneList() {
        List<String> idList = new ArrayList<String>();
        String query = "select stateid from BPELSE_SCHEMA.STATE where status='DONE'";
        ResultSet rs = execGenericQuery(query);
        if(rs != null) {
            try {
                while (rs.next() ){
                  String instanceId = rs.getString("stateid");
                  idList.add(instanceId);
                }
            } catch (SQLException ex) {
                ex.printStackTrace();
            }finally {
                closeGenericConnnection();
            }
        }
        return idList;
    }
    
    public void deleteSelectedBpelInstances(List<String> deleteQueryList) {
        if(deleteQueryList == null || deleteQueryList.isEmpty()) {
            return;
        }
        Connection deleteConnection = null;
        try {
            // create the connection to be used in the delete op.
            deleteConnection = ds.getConnection();
            // disable auto commit
            deleteConnection.setAutoCommit(false);
            for (String query : deleteQueryList) {
             logger.finest(query);
             Statement stmt = deleteConnection.createStatement();
             stmt.executeUpdate( query );
             stmt.close();
           }
            deleteConnection.commit();
        } catch (SQLException ex) {
            ex.printStackTrace();
            try {
                deleteConnection.rollback();
            } catch (SQLException rollBackEx) {
                rollBackEx.printStackTrace();
            }
        } finally {
            try {
                if(deleteConnection!= null) {
                    deleteConnection.close();
                }
            } catch (SQLException closeEx) {
                closeEx.printStackTrace();
            }
        }
    }
    
     
    
    
    private void initDataSource(){
        setup();
        ConfigurationService configService = 
                serviceManager.getConfigurationService(tName);

        
        String name = Util.mapComponentValue(cName,componentName);
        String type = Util.mapComponentValue(cType,componentType);

        Map<String,Object> props = null;
        if(type.equals(GenericConstants.SU_TYPE)) {
           String suid = pName+ GenericConstants.HYPHEN_SEPARATOR +cName;
           props = configService.getSUConfigurationProperties(name,type,suid);
        } else {
            props = configService.getConfigurationProperties(name,type);
        }
       
        String dsJndiName = (String)props.get("DB_JNDIName");
        try{
            InitialContext ic = new InitialContext();
            ds = (DataSource) ic.lookup(dsJndiName);      
        } catch(Exception e) {
            e.printStackTrace();
        }           
   }
    
}
