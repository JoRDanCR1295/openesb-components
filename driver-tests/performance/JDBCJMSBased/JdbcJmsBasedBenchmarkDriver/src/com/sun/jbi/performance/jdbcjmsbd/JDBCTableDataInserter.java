/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.performance.jdbcjmsbd;

import java.io.File;
import java.io.FileReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author radval
 */
public class JDBCTableDataInserter extends Thread {

    private static Logger LOGGER = Logger.getLogger(JDBCTableDataInserter.class.getName());
    private Properties properties;
    
    private String mInputSQLFile;
    private String mInputDataFile;
    private String mJDBCDriverFile;
    private String mJDBCDriverClassName;
    private String mDBUrl;
    private String mDBUser;
    private String mDBPassword;
    private String mInputCommitSizeStr;
    private int mInputCommitSize = -1;
    
    private int inputBatchSize;
    
    private Connection mConnection;
    
    
    public JDBCTableDataInserter(Properties properties) {
        this.properties = properties;
        
        this.mInputSQLFile = properties.getProperty("InputSQLFile");
        this.mInputDataFile = properties.getProperty("InputDataFile");
        this.mJDBCDriverFile = this.properties.getProperty("JDBCDriverFile");
        this.mJDBCDriverClassName = this.properties.getProperty("JDBCDriverClassName");
        this.mDBUrl = this.properties.getProperty("DBUrl");
        this.mDBUser = this.properties.getProperty("DBUser");
        this.mDBPassword = this.properties.getProperty("DBPassword");
        this.mInputCommitSizeStr = this.properties.getProperty("InputCommitSize");
        if(mInputCommitSizeStr != null) {
            this.mInputCommitSize = Integer.parseInt(this.mInputCommitSizeStr);
        }        
        
        this.inputBatchSize = Integer.parseInt(properties.getProperty("InputBatchSize"));
        init();
    }

    @Override
    public void run() {
        insertIntoTable();
    }
    
    
    private void insertIntoTable() {
        if(this.mConnection == null){
            return;
        }
        
        int counter = 0;
        int commitCounter =1;
        String val = null;
        boolean isLastCommited = false;    
        try {
            
            String commaSeperatedTableRowData = null;
            if(this.mInputDataFile != null && this.mInputSQLFile != null) {
                this.mConnection.setAutoCommit(false);
                String sql = Utility.readTextFromFile(this.mInputSQLFile, "UTF-8");
                String tableData = Utility.readTextFromFile(this.mInputDataFile, "UTF-8");
                        
                PreparedStatement ps = this.mConnection.prepareStatement(sql);
                commaSeperatedTableRowData = tableData;
                String[] data = parseData(commaSeperatedTableRowData);
                
                    do {
                       for(int i = 0; i < data.length; i++){
                          val = data[i];
                          if(val.equals("$counter")) {
                              val = ""+ (counter +1);
                          }
                          ps.setObject(i+1, val);
                       }
                       
                       //ps.addBatch();
                       
                       if(mInputCommitSize != -1) {
                           ps.addBatch();
                           if(commitCounter == mInputCommitSize) {
                               ps.executeBatch();
                               this.mConnection.commit();
                               commitCounter = 1;
                               isLastCommited = true;
                           } else {
                               isLastCommited = false;
                           }
                           
                       }  else {
                           ps.execute();
                            this.mConnection.commit();
                       }
                       
                       
                       LOGGER.log(Level.FINEST,"Sending counter id  (" + counter + ") table row (" + commaSeperatedTableRowData + ")"); 
                       counter++;
                       commitCounter++;
                    } while (counter < inputBatchSize);
                    
                    if(mInputCommitSize != -1 && !isLastCommited) {
                        ps.executeBatch();
                        this.mConnection.commit();
                        
                    }
                    //ps.executeBatch();
                    
                    
                    LOGGER.log(Level.FINEST,"Done inserting "+ inputBatchSize + " messages."); 
                }
        } catch(Exception ex) {
            LOGGER.log(Level.SEVERE, "Exception occurred while inserting data: " + val, ex);            
            System.exit(0);
        } finally {
            try {
                this.mConnection.close();
            } catch(Exception ex){
                LOGGER.log(Level.SEVERE, "Exception occurred while closing connection. ", ex);            
                System.exit(0);
            }
        }
            
           
    }
    
    
    
    String[] parseData(String commaSeperatedTableRowData) {
        String[] data = null;
        List<String> dataList = new ArrayList<String>();
        
        StringTokenizer st = new StringTokenizer(commaSeperatedTableRowData, ",");
        while(st.hasMoreElements()) {
            String col = (String) st.nextElement();
            dataList.add(col);
        }
        
        data = dataList.toArray(new String[] {});
        
        return data;
    }
    
    private void init() {
        createConnection();
    }
    
    
    private void createConnection() {
        loadJDBCDriver();
        try {
            if(this.mDBUrl != null && this.mDBUser != null && this.mDBPassword != null) {
                mConnection =  DriverManager.getConnection(this.mDBUrl, this.mDBUser, this.mDBPassword);
            }
        } catch(Exception ex){
            LOGGER.log(Level.SEVERE, "Exception occurred while creation a connection. DBURL: "+ this.mDBUrl + " DBUSER: "+ this.mDBUser + " DBPASSWORD: " + this.mDBPassword  , ex);            
            System.exit(0);
        }

        
    }
    private void loadJDBCDriver() {
        try {
            if(this.mJDBCDriverFile != null) {
                StringTokenizer st = new StringTokenizer(this.mJDBCDriverFile, ";");
                List<URL> driverUrls = new ArrayList<URL>();
                while(st.hasMoreElements()) {
                    String driverFile = (String) st.nextElement();
                    File dFile = new File(driverFile);
                    if(dFile.exists()) {
                      driverUrls.add(dFile.toURL());
                    }  else {
                        throw new Exception("Driver File: "+ dFile + " does not exists.");
                    }
                    
                }
                
                
                //load driver
                URLClassLoader loader = new URLClassLoader(driverUrls.toArray(new URL[]{}), getClass().getClassLoader());
                Thread.currentThread().setContextClassLoader(loader);
                
                if(this.mJDBCDriverClassName != null) {
                    Driver d = (Driver) Class.forName(this.mJDBCDriverClassName, true, loader).newInstance();
                    DriverManager.registerDriver(new DriverShim(d));

                }
                
//                Enumeration e = DriverManager.getDrivers();
//                while(e.hasMoreElements()) {
//                    Driver d = (Driver) e.nextElement();
//                     System.out.println("driver "+ d.toString());       
//                }
            }
            
            
        } catch(Exception ex) {
            LOGGER.log(Level.SEVERE, "Exception occurred while find jdbc driver.", ex);            
            System.exit(0);
        }
    }
    
    
    
}
