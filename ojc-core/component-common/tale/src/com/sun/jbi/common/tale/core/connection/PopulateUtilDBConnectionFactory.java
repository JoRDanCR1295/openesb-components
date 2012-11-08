package com.sun.jbi.common.tale.core.connection;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.tale.core.util.I18n;

/**
 * DBConnectionFactory for the populate utility
 * 
 * @author Sun Microsystems
 * 
 */
public class PopulateUtilDBConnectionFactory extends DBConnectionFactory {
    private static String DERBY = "Derby";

    private Connection mConn;
    private String mDbURL = null;
    private String mUser = null;
    private String mPassword = null;
    private String mServer = null;
    private String mPort = null;
    private String mDatabaseOrSID = null;
    private Logger mLogger = Logger.getLogger(PopulateUtilDBConnectionFactory.class.getName());

    public PopulateUtilDBConnectionFactory(Properties prop) {
        super(prop);
    }

    /** @see com.sun.jbi.common.tale.core.connection.DBConnectionFactory#initDataSource(java.util.Properties) */
    protected void initDataSource(Properties properties) {
        String database = properties.getProperty(ConnectionProperties.DB_TYPE);
        if (database.equalsIgnoreCase(DERBY)) {
            setDBType(DBType.DERBY_DB);
        }
        mUser = properties.getProperty(ConnectionProperties.DB_USERNAME);
        mPassword = properties.getProperty(ConnectionProperties.DB_PASSWORD);
        mServer = properties.getProperty(ConnectionProperties.DB_HOST);
        mDatabaseOrSID = properties
                .getProperty(ConnectionProperties.DB_INSTANCE);
        mPort = properties.getProperty(ConnectionProperties.DB_PORT);

        switch (getDBType()) {
            case DERBY_DB:
                mDbURL = "jdbc:derby://" + mServer + ":" + mPort + "/" + mDatabaseOrSID;
                break;
        }
    }

    /** @see com.sun.jbi.common.tale.core.connection.DBConnectionFactory#createDBConnection() */
    public DBConnection createDBConnection() throws SQLException {
        try {
            switch (getDBType()) {
                case DERBY_DB:
                    Class.forName("org.apache.derby.jdbc.ClientDriver");
                    mConn = DriverManager.getConnection(mDbURL, mUser, mPassword);
                    break;
            }
        } catch (ClassNotFoundException ex) {
            String message = I18n.loc(
                    "TALE-7014: DriverManager.getConnection failed.Exception Detals : {0} ",
                    ex.getMessage());
            mLogger.log(Level.SEVERE, message, ex);
        }
        DBConnection dbConn = new DBConnection(mConn);
        return dbConn;
    }
}
