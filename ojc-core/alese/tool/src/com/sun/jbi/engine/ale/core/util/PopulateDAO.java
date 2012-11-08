package com.sun.jbi.engine.ale.core.util;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * PopulateDAO.java
 *
 * @author Edward Chou
 */
public abstract class PopulateDAO {

    private static final Logger logger = Logger.getLogger(PopulateDAO.class.getName());
    
    private static final String INSERT_CSF_REP_USERS = "insert into CSF_REP_USERS " +
    "(USER_LOGICAL_ID, USER_DESCRIPTION, USER_NAME, USER_PASSWORD, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_CSF_JMS_CHANNEL = "insert into CSF_JMS_CHANNEL " +
    "(CHANNEL_CODE, HOST_IP, PORT, NAME, CHANNEL_TYPE, ACTIVE_FLAG, DESCRIPTION, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_CSF_ALERTER_GROUPS = "insert into CSF_ALERTER_GROUPS " +
    "(ALERTER_GROUP, ALERTER_FROM, ALERTER_TO_PRIMARY, ALERTER_TO_SECONDARY, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_CSF_ALERTER_CHANNELS = "insert into CSF_ALERTER_CHANNELS " +
    "(ALERTER_CHANNEL_CODE, CHANNEL_TYPE, HOST_NAME, COMMUNITY, TRAP_PORT, LISTENER_PORT, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_CSF_ALERTER_CODES = "insert into CSF_ALERTER_CODES " +
    "(ALERTER_CODE, ALERTER_LABEL, ALERTER_CATEGORY, ALERTER_LEVEL, ALERTER_DESCRIPTION, ALERTER_GROUP, ALERTER_CHANNEL_CODE, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_CSF_LOGGER_CHANNELS = "insert into CSF_LOGGER_CHANNELS " +
    "(LOGGER_CHANNEL_CODE, CHANNEL_TYPE, FILE_NAME, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_CSF_LOGGER_CODES = "insert into CSF_LOGGER_CODES " +
    "(LOGGER_CODE, LOGGER_LABEL, LOGGER_CATEGORY, LOGGER_LEVEL, LOGGER_DESCRIPTION, LOGGER_CHANNEL_CODE, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_CSF_ERROR_CODES = "insert into CSF_ERROR_CODES " +
    "(ERROR_CODE, ERROR_LABEL, ERROR_CATEGORY, ERROR_LEVEL, ERROR_DESCRIPTION, AUTHORIZE_FLAG, LOGGER_FLAG, ALERTER_FLAG, REPLAY_FLAG, PERSIST_FLAG, ENCODE_FLAG, ALERTER_CODE, LOGGER_CODE, PERSIST_MODE, ENCODE_MODE, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_ALE_CHANNELS = "insert into ALE_CHANNELS " +
    "(ALE_CHANNEL_CODE, CHANNEL_TYPE, FILE_NAME, HOST_NAME, COMMUNITY, TRAP_PORT, LISTENER_PORT, EMAIL_FROM, EMAIL_TO_PRIMARY, " +
    "EMAIL_TO_SECONDARY, JMS_PORT, JMS_TYPE, JMS_NAME, USER_NAME, PASS_WORD, ENDPOINT_NAME, SERVICE_NAME, OPEARTION_NAME, " +
    "FIELD_1, FIELD_2,FIELD_3, FIELD_4, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    private static final String INSERT_LOGGER_CODES_CHANNEL = "insert into LOGGER_CODES_CHANNEL " +
    "(LOGGER_CODE, ALE_CHANNEL_CODE) values " +
    "(?, ?)";
    
    private static final String INSERT_ALERTER_CODES_CHANNEL = "insert into ALERTER_CODES_CHANNEL " +
    "(ALERTER_CODE, ALE_CHANNEL_CODE) values " +
    "(?, ?)";
    
    private static final String INSERT_CSF_RECONCILIATION_APPS = "insert into CSF_RECONCILIATION_APPS " +
    "(APP_NAME, PERSISTENT, INTERVAL1, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
    "(?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    
    private String jdbcDriverClassName;
    private String dbType;
    private String host;
    private String port;
    private String instance;
    private String username;
    private String password;
    
    private Connection connection;
    private PreparedStatement insertCsfRepUsers_PS;
    private PreparedStatement insertCsfJmsChannel_PS;
    private PreparedStatement insertCsfAlerterGroups_PS;
    private PreparedStatement insertCsfAlerterChannels_PS;
    private PreparedStatement insertCsfAlerterCodes_PS;
    private PreparedStatement insertCsfLoggerChannels_PS;
    private PreparedStatement insertCsfLoggerCodes_PS;
    private PreparedStatement insertCsfErrorCodes_PS;
    private PreparedStatement insertAleChannels_PS;
    private PreparedStatement insertLoggerCodesChannel_PS;
    private PreparedStatement insertAlerterCodesChannel_PS;
    private PreparedStatement insertCsfReconciliationApps_PS;
    
    
    protected void init() throws Exception {
        Class.forName(jdbcDriverClassName);
        String url = "jdbc:" + dbType + "://" + host + ":" + port + "/" + instance;
        connection = DriverManager.getConnection(url, username, password);
        insertCsfRepUsers_PS = connection.prepareStatement(INSERT_CSF_REP_USERS);
        insertCsfJmsChannel_PS = connection.prepareStatement(INSERT_CSF_JMS_CHANNEL);
        insertCsfAlerterGroups_PS = connection.prepareStatement(INSERT_CSF_ALERTER_GROUPS);
        insertCsfAlerterChannels_PS = connection.prepareStatement(INSERT_CSF_ALERTER_CHANNELS);
        insertCsfAlerterCodes_PS = connection.prepareStatement(INSERT_CSF_ALERTER_CODES);
        insertCsfLoggerChannels_PS = connection.prepareStatement(INSERT_CSF_LOGGER_CHANNELS);
        insertCsfLoggerCodes_PS = connection.prepareStatement(INSERT_CSF_LOGGER_CODES);
        insertCsfErrorCodes_PS = connection.prepareStatement(INSERT_CSF_ERROR_CODES);
        insertAleChannels_PS = connection.prepareStatement(INSERT_ALE_CHANNELS);
        insertLoggerCodesChannel_PS = connection.prepareStatement(INSERT_LOGGER_CODES_CHANNEL);
        insertAlerterCodesChannel_PS = connection.prepareStatement(INSERT_ALERTER_CODES_CHANNEL);
        insertCsfReconciliationApps_PS = connection.prepareStatement(INSERT_CSF_RECONCILIATION_APPS);
    }
    
    public void close() throws Exception {
        insertCsfRepUsers_PS.close();
        insertCsfJmsChannel_PS.close();
        insertCsfAlerterGroups_PS.close();
        insertCsfAlerterChannels_PS.close();
        insertCsfAlerterCodes_PS.close();
        insertCsfLoggerChannels_PS.close();
        insertCsfLoggerCodes_PS.close();
        insertCsfErrorCodes_PS.close();
        insertAleChannels_PS.close();
        insertLoggerCodesChannel_PS.close();
        insertAlerterCodesChannel_PS.close();
        insertCsfReconciliationApps_PS.close();
        connection.close();
    }
    
    public boolean insertCsfRepUsers(String s) {
        String[] values = s.split("\\s*,\\s*", 7);
        if (values.length != 7) {
            return false;
        }
        
        try {
            insertCsfRepUsers_PS.clearParameters();
            insertCsfRepUsers_PS.setString(1, values[0]);
            insertCsfRepUsers_PS.setString(2, values[1]);
            insertCsfRepUsers_PS.setString(3, values[2]);
            insertCsfRepUsers_PS.setString(4, values[3]);
            insertCsfRepUsers_PS.setString(5, values[4]);
            insertCsfRepUsers_PS.setString(6, values[5]);
            insertCsfRepUsers_PS.setString(7, values[6]);
            insertCsfRepUsers_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }
    
    public boolean insertCsfJmsChannel(String s) {
        String[] values = s.split("\\s*,\\s*", 9);
        if (values.length != 9) {
            return false;
        }
        
        try {
            insertCsfJmsChannel_PS.clearParameters();
            insertCsfJmsChannel_PS.setInt(1, Integer.parseInt(values[0]));
            insertCsfJmsChannel_PS.setString(2, values[1]);
            insertCsfJmsChannel_PS.setShort(3, Short.parseShort(values[2]));
            insertCsfJmsChannel_PS.setString(4, values[3]);
            insertCsfJmsChannel_PS.setString(5, values[4]);
            insertCsfJmsChannel_PS.setString(6, values[5]);
            insertCsfJmsChannel_PS.setString(7, values[6]);
            insertCsfJmsChannel_PS.setString(8, values[7]);
            insertCsfJmsChannel_PS.setString(9, values[8]);
            insertCsfJmsChannel_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertCsfAlerterGroups(String s) {
        String[] values = s.split("\\s*,\\s*", 7);
        if (values.length != 7) {
            return false;
        }
        
        try {
            insertCsfAlerterGroups_PS.clearParameters();
            insertCsfAlerterGroups_PS.setString(1, values[0]);
            insertCsfAlerterGroups_PS.setString(2, values[1]);
            insertCsfAlerterGroups_PS.setString(3, values[2]);
            insertCsfAlerterGroups_PS.setString(4, values[3]);
            insertCsfAlerterGroups_PS.setString(5, values[4]);
            insertCsfAlerterGroups_PS.setString(6, values[5]);
            insertCsfAlerterGroups_PS.setString(7, values[6]);
            insertCsfAlerterGroups_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertCsfAlerterChannels(String s) {
        String[] values = s.split("\\s*,\\s*", 9);
        if (values.length != 9) {
            return false;
        }
        
        try {
            insertCsfAlerterChannels_PS.clearParameters();
            insertCsfAlerterChannels_PS.setInt(1, Integer.parseInt(values[0]));
            insertCsfAlerterChannels_PS.setString(2, values[1]);
            insertCsfAlerterChannels_PS.setString(3, values[2]);
            insertCsfAlerterChannels_PS.setString(4, values[3]);
            insertCsfAlerterChannels_PS.setString(5, values[4]);
            insertCsfAlerterChannels_PS.setString(6, values[5]);
            insertCsfAlerterChannels_PS.setString(7, values[6]);
            insertCsfAlerterChannels_PS.setString(8, values[7]);
            insertCsfAlerterChannels_PS.setString(9, values[8]);
            insertCsfAlerterChannels_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertCsfAlerterCodes(String s) {
        String[] values = s.split("\\s*,\\s*", 10);
        if (values.length != 10) {
            return false;
        }
        
        try {
            insertCsfAlerterCodes_PS.clearParameters();
            insertCsfAlerterCodes_PS.setInt(1, Integer.parseInt(values[0]));
            insertCsfAlerterCodes_PS.setString(2, values[1]);
            insertCsfAlerterCodes_PS.setString(3, values[2]);
            insertCsfAlerterCodes_PS.setString(4, values[3]);
            insertCsfAlerterCodes_PS.setString(5, values[4]);
            insertCsfAlerterCodes_PS.setString(6, values[5]);
            insertCsfAlerterCodes_PS.setInt(7, Integer.parseInt(values[6]));
            insertCsfAlerterCodes_PS.setString(8, values[7]);
            insertCsfAlerterCodes_PS.setString(9, values[8]);
            insertCsfAlerterCodes_PS.setString(10, values[9]);
            insertCsfAlerterCodes_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertCsfLoggerChannels(String s) {
        String[] values = s.split("\\s*,\\s*", 6);
        if (values.length != 6) {
            return false;
        }
        
        try {
            insertCsfLoggerChannels_PS.clearParameters();
            insertCsfLoggerChannels_PS.setInt(1, Integer.parseInt(values[0]));
            insertCsfLoggerChannels_PS.setString(2, values[1]);
            insertCsfLoggerChannels_PS.setString(3, values[2]);
            insertCsfLoggerChannels_PS.setString(4, values[3]);
            insertCsfLoggerChannels_PS.setString(5, values[4]);
            insertCsfLoggerChannels_PS.setString(6, values[5]);
            insertCsfLoggerChannels_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertCsfLoggerCodes(String s) {
        String[] values = s.split("\\s*,\\s*", 9);
        if (values.length != 9) {
            return false;
        }
        
        try {
            insertCsfLoggerCodes_PS.clearParameters();
            insertCsfLoggerCodes_PS.setInt(1, Integer.parseInt(values[0]));
            insertCsfLoggerCodes_PS.setString(2, values[1]);
            insertCsfLoggerCodes_PS.setString(3, values[2]);
            insertCsfLoggerCodes_PS.setString(4, values[3]);
            insertCsfLoggerCodes_PS.setString(5, values[4]);
            insertCsfLoggerCodes_PS.setInt(6, Integer.parseInt(values[5]));
            insertCsfLoggerCodes_PS.setString(7, values[6]);
            insertCsfLoggerCodes_PS.setString(8, values[7]);
            insertCsfLoggerCodes_PS.setString(9, values[8]);
            insertCsfLoggerCodes_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertCsfErrorCodes(String s) {
        String[] values = s.split("\\s*,\\s*", 18);
        if (values.length != 18) {
            return false;
        }
        
        try {
            insertCsfErrorCodes_PS.clearParameters();
            insertCsfErrorCodes_PS.setInt(1, Integer.parseInt(values[0]));
            insertCsfErrorCodes_PS.setString(2, values[1]);
            insertCsfErrorCodes_PS.setString(3, values[2]);
            insertCsfErrorCodes_PS.setString(4, values[3]);
            insertCsfErrorCodes_PS.setString(5, values[4]);
            insertCsfErrorCodes_PS.setString(6, values[5]);
            insertCsfErrorCodes_PS.setString(7, values[6]);
            insertCsfErrorCodes_PS.setString(8, values[7]);
            insertCsfErrorCodes_PS.setString(9, values[8]);
            insertCsfErrorCodes_PS.setString(10, values[9]);
            insertCsfErrorCodes_PS.setString(11, values[10]);
            insertCsfErrorCodes_PS.setInt(12, Integer.parseInt(values[11]));
            insertCsfErrorCodes_PS.setInt(13, Integer.parseInt(values[12]));
            insertCsfErrorCodes_PS.setString(14, values[13]);
            insertCsfErrorCodes_PS.setString(15, values[14]);
            insertCsfErrorCodes_PS.setString(16, values[15]);
            insertCsfErrorCodes_PS.setString(17, values[16]);
            insertCsfErrorCodes_PS.setString(18, values[17]);
            insertCsfErrorCodes_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertAleChannels(String s) {
        String[] values = s.split("\\s*,\\s*", 25);
        if (values.length != 25) {
            return false;
        }
        
        try {
            insertAleChannels_PS.clearParameters();
            insertAleChannels_PS.setInt(1, Integer.parseInt(values[0]));
            insertAleChannels_PS.setString(2, values[1]);
            insertAleChannels_PS.setString(3, values[2]);
            insertAleChannels_PS.setString(4, values[3]);
            insertAleChannels_PS.setString(5, values[4]);
            insertAleChannels_PS.setString(6, values[5]);
            insertAleChannels_PS.setString(7, values[6]);
            insertAleChannels_PS.setString(8, values[7]);
            insertAleChannels_PS.setString(9, values[8]);
            insertAleChannels_PS.setString(10, values[9]);
            insertAleChannels_PS.setString(11, values[10]);
            insertAleChannels_PS.setString(12, values[11]);
            insertAleChannels_PS.setString(13, values[12]);
            insertAleChannels_PS.setString(14, values[13]);
            insertAleChannels_PS.setString(15, values[14]);
            insertAleChannels_PS.setString(16, values[15]);
            insertAleChannels_PS.setString(17, values[16]);
            insertAleChannels_PS.setString(18, values[17]);
            insertAleChannels_PS.setString(19, values[18]);
            insertAleChannels_PS.setString(20, values[19]);
            insertAleChannels_PS.setString(21, values[20]);
            insertAleChannels_PS.setString(22, values[21]);
            insertAleChannels_PS.setString(23, values[22]);
            insertAleChannels_PS.setString(24, values[23]);
            insertAleChannels_PS.setString(25, values[24]);
            insertAleChannels_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertLoggerCodesChannel(String s) {
        String[] values = s.split("\\s*,\\s*", 2);
        if (values.length != 2) {
            return false;
        }
        
        try {
            insertLoggerCodesChannel_PS.clearParameters();
            insertLoggerCodesChannel_PS.setInt(1, Integer.parseInt(values[0]));
            insertLoggerCodesChannel_PS.setInt(2, Integer.parseInt(values[1]));
            insertLoggerCodesChannel_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertAlerterCodesChannel(String s) {
        String[] values = s.split("\\s*,\\s*", 2);
        if (values.length != 2) {
            return false;
        }
        
        try {
            insertAlerterCodesChannel_PS.clearParameters();
            insertAlerterCodesChannel_PS.setInt(1, Integer.parseInt(values[0]));
            insertAlerterCodesChannel_PS.setInt(2, Integer.parseInt(values[1]));
            insertAlerterCodesChannel_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    public boolean insertCsfReconciliationApps(String s) {
        String[] values = s.split("\\s*,\\s*", 5);
        if (values.length != 5) {
            return false;
        }
        
        try {
            insertCsfReconciliationApps_PS.clearParameters();
            insertCsfReconciliationApps_PS.setString(1, values[0]);
            insertCsfReconciliationApps_PS.setString(2, values[1]);
            insertCsfReconciliationApps_PS.setInt(3, Integer.parseInt(values[2]));
            insertCsfReconciliationApps_PS.setString(4, values[3]);
            insertCsfReconciliationApps_PS.setString(5, values[4]);
            insertCsfReconciliationApps_PS.execute();
        } catch (SQLException sqle) {
            logger.log(Level.WARNING, "error inserting record: " + sqle.getMessage());
            return false;
        }
        
        return true;
    }

    /**
     * @return the jdbcDriverClassName
     */
    public String getJdbcDriverClassName() {
        return jdbcDriverClassName;
    }
    /**
     * @param jdbcDriverClassName the jdbcDriverClassName to set
     */
    public void setJdbcDriverClassName(String jdbcDriverClassName) {
        this.jdbcDriverClassName = jdbcDriverClassName;
    }
    /**
     * @return the dbType
     */
    public String getDbType() {
        return dbType;
    }
    /**
     * @param dbType the dbType to set
     */
    public void setDbType(String dbType) {
        this.dbType = dbType;
    }
    /**
     * @return the host
     */
    public String getHost() {
        return host;
    }
    /**
     * @param host the host to set
     */
    public void setHost(String host) {
        this.host = host;
    }
    /**
     * @return the instance
     */
    public String getInstance() {
        return instance;
    }
    /**
     * @param instance the instance to set
     */
    public void setInstance(String instance) {
        this.instance = instance;
    }
    /**
     * @return the password
     */
    public String getPassword() {
        return password;
    }
    /**
     * @param password the password to set
     */
    public void setPassword(String password) {
        this.password = password;
    }
    /**
     * @return the port
     */
    public String getPort() {
        return port;
    }
    /**
     * @param port the port to set
     */
    public void setPort(String port) {
        this.port = port;
    }
    /**
     * @return the username
     */
    public String getUsername() {
        return username;
    }
    /**
     * @param username the username to set
     */
    public void setUsername(String username) {
        this.username = username;
    }
    
    
}
