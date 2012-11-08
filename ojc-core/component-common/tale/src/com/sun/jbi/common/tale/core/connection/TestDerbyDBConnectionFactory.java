package com.sun.jbi.common.tale.core.connection;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Properties;

import com.sun.jbi.common.tale.core.domain.TaleDomain;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.Payload;
import com.sun.jbi.common.tale.core.domain.SourceInfo;
import com.sun.jbi.common.tale.core.domain.Payload.Encode;
import com.sun.jbi.common.tale.core.domain.Payload.EncodeMode;
import com.sun.jbi.common.tale.core.domain.Payload.PayloadType;
import com.sun.jbi.common.tale.core.persist.dao.DAO;
import com.sun.jbi.common.tale.core.persist.dao.DAOException;
import com.sun.jbi.common.tale.core.persist.dao.DAO.Action;

public class TestDerbyDBConnectionFactory extends DBConnectionFactory{
    private Connection conn;
    /**
     * @param args
     */
    public static void main(String[] args) {
        Properties prop = new Properties();
        prop.setProperty(ConnectionProperties.DB_URL, "jdbc:derby://localhost:1527/alesedDb");
        prop.setProperty(ConnectionProperties.DB_USERNAME, "alese_user");
        prop.setProperty(ConnectionProperties.DB_PASSWORD, "alese_user");
        
        TestDerbyDBConnectionFactory testCF = new TestDerbyDBConnectionFactory();
        
        try {
            testCF.initializeDataSource(prop);
            TaleDomain domain = new TestALEDomain(testCF);
            DAO dao = domain.newDAO();
            Integer dbId = 6002;
            test1(dao, dbId.toString());
            dbId ++;
            test2(dao, dbId.toString());
        } catch (SQLException e) {
            e.printStackTrace();
            return;
        } catch (DAOException e) {
            e.printStackTrace();
            return;
        }
    }
    private static void test1(DAO dao, String dbId) throws DAOException {
        
        //create Application DBO object and populate the data
        //required to insert logger record
        SourceInfo dboApp = new SourceInfo();
        //dboApp.setDBMessageID(dbId);
        
        dboApp.setApplicationName("applicationName");
        dboApp.setApplicationType("applicationType");
        dboApp.setDateTimeStamp(new Timestamp(System.currentTimeMillis()));
        dboApp.setAppMessageID("messageID");
        dboApp.setModuleName("moduleName");
        dboApp.setProjectName("projectName");
        dboApp.setServiceName("serviceName");
        dboApp.setUnitName("unitName");
        
        //create Logger DBO object and populate the data
        TaleRequest dboLog = new TaleRequest();
        //dboLog.setDBMessageID(dbId);
        
        dboLog.setCode(100);
        dboLog.setDetails("logDetails");
        dboLog.setDisplayMessage("logDisplayMessage");
        
        //create Payload DBO object and populate the data
        Payload dboPayload = new Payload();
        //dboPayload.setDBMessageID(dbId);
        
        dboPayload.setEncodeFlag(Encode.N);
        dboPayload.setEncodeMode(EncodeMode.ASCII);
        dboPayload.setPayloadType(PayloadType.ORIGINAL_MSG);
        dboPayload.setPayloadMessage("this is test payload");

        //insert the recoed into db
//        dao.insertLoggingDataAndPayLoad(dboApp, dboLog, dboPayload);
        dao.beginTransaction();
        dao.recordLog(dboLog, Action.INSERT);
        dao.recordPayload(dboPayload, Action.INSERT);
        dao.commitTransaction();
    }

    private static void test2(DAO dao, String dbId) throws DAOException {
        //create Application DBO object and populate the data
        //required to insert logger record
        SourceInfo dboApp = new SourceInfo();
        //dboApp.setDBMessageID(dbId);
        
        dboApp.setApplicationName("applicationName");
        dboApp.setApplicationType("applicationType");
        dboApp.setDateTimeStamp(new Timestamp(System.currentTimeMillis()));
        dboApp.setAppMessageID("messageID");
        dboApp.setModuleName("moduleName");
        dboApp.setProjectName("projectName");
        dboApp.setServiceName("serviceName");
        dboApp.setUnitName("unitName");
        
        //create Logger DBO object and populate the data
        TaleRequest dboLog = new TaleRequest();
        //dboLog.setDBMessageID(dbId);
        
        dboLog.setCode(100);
        dboLog.setDetails("logDetails");
        dboLog.setDisplayMessage("logDisplayMessage");
        

        //insert the recoed into db
//        dao.insertLoggingData(dboApp, dboLog);
        dao.beginTransaction();
        dao.recordLog(dboLog, Action.INSERT);
        dao.commitTransaction();
    }
    
    private void initializeDataSource(Properties properties) throws SQLException {
        //DriverManager.registerDriver(new org.apache.derby.jdbc.ClientDriver());

        conn = DriverManager.getConnection(
                properties.getProperty(ConnectionProperties.DB_URL), 
                properties.getProperty(ConnectionProperties.DB_USERNAME), 
                properties.getProperty(ConnectionProperties.DB_PASSWORD));

    }
    
    public DBConnection createDBConnection() throws SQLException {
        DBConnection dbConn = new DBConnection(conn);
        return dbConn;
    }
    
    public DBType getDBType() {
        return DBType.DERBY_DB;
    }

    private static class TestALEDomain extends TaleDomain {
        public TestALEDomain(DBConnectionFactory fac) {
            super(fac);
        }
    }
}
