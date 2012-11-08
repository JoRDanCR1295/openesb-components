package com.sun.jbi.engine.ale.core.util;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * PopulateALEDBUtil.java
 *
 * @author Edward Chou
 */
public class PopulateALEDBUtil {

    private static final Logger logger = Logger.getLogger(PopulateALEDBUtil.class.getName());
    
    // CSV filename props
    private static final String CSF_REP_USERS_PROP = "CSF_REP_USERS";
    private static final String CSF_JMS_CHANNEL_PROP = "CSF_JMS_CHANNEL";
    private static final String CSF_ALERTER_GROUPS_PROP = "CSF_ALERTER_GROUPS";
    private static final String CSF_ALERTER_CHANNELS_PROP = "CSF_ALERTER_CHANNELS";
    private static final String CSF_ALERTER_CODES_PROP = "CSF_ALERTER_CODES";
    private static final String CSF_LOGGER_CHANNELS_PROP = "CSF_LOGGER_CHANNELS";
    private static final String CSF_LOGGER_CODES_PROP = "CSF_LOGGER_CODES";
    private static final String CSF_ERROR_CODES_PROP = "CSF_ERROR_CODES";
    private static final String ALE_CHANNELS_PROP = "ALE_CHANNELS";
    private static final String LOGGER_CODES_CHANNEL_PROP = "LOGGER_CODES_CHANNEL";
    private static final String ALERTER_CODES_CHANNEL_PROP = "ALERTER_CODES_CHANNEL";
    private static final String CSF_RECONCILIATION_APPS_PROP = "CSF_RECONCILIATION_APPS";
    
    public enum CONFIG_TABLE {
        CSF_REP_USERS(CSF_REP_USERS_PROP),
        CSF_JMS_CHANNEL(CSF_JMS_CHANNEL_PROP),
        CSF_ALERTER_GROUPS(CSF_ALERTER_GROUPS_PROP),
        CSF_ALERTER_CHANNELS(CSF_ALERTER_CHANNELS_PROP),
        CSF_ALERTER_CODES(CSF_ALERTER_CODES_PROP),
        CSF_LOGGER_CHANNELS(CSF_LOGGER_CHANNELS_PROP),
        CSF_LOGGER_CODES(CSF_LOGGER_CODES_PROP),
        CSF_ERROR_CODES(CSF_ERROR_CODES_PROP),
        ALE_CHANNELS(ALE_CHANNELS_PROP),
        LOGGER_CODES_CHANNEL(LOGGER_CODES_CHANNEL_PROP),
        ALERTER_CODES_CHANNEL(ALERTER_CODES_CHANNEL_PROP),
        CSF_RECONCILIATION_APPS(CSF_RECONCILIATION_APPS_PROP);

        private String propName = "";

        CONFIG_TABLE(String propName) {
            this.propName = propName;
        }
        public String getPropName() {
            return propName;
        }
    }
    
    public static void main(String[] args) {
        String configPath = args[0];
        if (configPath == null || configPath.length() == 0) {
            logger.log(Level.SEVERE, "missing command argument for Config File location");
            return;
        }
        
        File f = new File(configPath);
        if (!f.isFile()) {
            logger.log(Level.SEVERE, "invalide location for Config File location: " + configPath);
            return;
        }
        
        // read config props
        Properties configProps = new Properties();
        InputStream is = null;
        try {
            is = new BufferedInputStream(new FileInputStream(f));
            configProps.load(is);
        } catch (IOException ioe) {
            logger.log(Level.SEVERE, "error reading config property file", ioe);
            return;
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ioe) {
                    // ignore
                }
            }
        }
        
        // instantiate dao object
        PopulateDAO dao = null;
        try {
            dao = PopulateDAOFactory.createDAO(configProps);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "error instantiating DAO from property file", e);
            return;
        }
        
        
        for (CONFIG_TABLE table :  CONFIG_TABLE.values()) {
            try {
                parseCSVFiles(table, dao, configProps);
            } catch (Exception e) {
                logger.log(Level.SEVERE, "error populating configuration data for table: " + table, e);
                continue;
            }
        }
        
        try {
            dao.close();
        } catch (Exception e) {
            logger.log(Level.WARNING, "error closing DAO", e);
            return;
        }
    }
    
    private static void parseCSVFiles(CONFIG_TABLE table, PopulateDAO dao, Properties configProps) throws Exception {
        String filePath = configProps.getProperty(table.getPropName());
        if (filePath == null || filePath.length() == 0) {
            throw new Exception ("empty filePath for table: " + table);
        }
        File f = new File(filePath);
        if (!f.isFile()) {
            throw new Exception("filePath is not a file: " + filePath);
        }
        
        int numInserted = 0;
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(f));
            String curLine = reader.readLine();
            while (curLine != null) {
                switch (table) {
                case CSF_REP_USERS:
                    if (dao.insertCsfRepUsers(curLine)) {
                        numInserted++;
                    }
                    break;
                case CSF_JMS_CHANNEL:
                    if (dao.insertCsfJmsChannel(curLine)) {
                        numInserted++;
                    }
                    break;
                case CSF_ALERTER_GROUPS:
                    if (dao.insertCsfAlerterGroups(curLine)) {
                        numInserted++;
                    }
                    break;
                case CSF_ALERTER_CHANNELS:
                    if (dao.insertCsfAlerterChannels(curLine)) {
                        numInserted++;
                    }
                    break;
                case CSF_ALERTER_CODES:
                    if (dao.insertCsfAlerterCodes(curLine)) {
                        numInserted++;
                    }
                    break;
                case CSF_LOGGER_CHANNELS:
                    if (dao.insertCsfLoggerChannels(curLine)) {
                        numInserted++;
                    }
                    break;
                case CSF_LOGGER_CODES:
                    if (dao.insertCsfLoggerCodes(curLine)) {
                        numInserted++;
                    }
                    break;
                case CSF_ERROR_CODES:
                    if (dao.insertCsfErrorCodes(curLine)) {
                        numInserted++;
                    }
                    break;
                case ALE_CHANNELS:
                    if (dao.insertAleChannels(curLine)) {
                        numInserted++;
                    }
                    break;
                case LOGGER_CODES_CHANNEL:
                    if (dao.insertLoggerCodesChannel(curLine)) {
                        numInserted++;
                    }
                    break;
                case ALERTER_CODES_CHANNEL:
                    if (dao.insertAlerterCodesChannel(curLine)) {
                        numInserted++;
                    }
                    break;
                case CSF_RECONCILIATION_APPS:
                    if (dao.insertCsfReconciliationApps(curLine)) {
                        numInserted++;
                    }
                    break;
                    
                }
                
                curLine = reader.readLine();
            }
            
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException ioe) {
                    // ignore
                }
            }
        }
        logger.log(Level.INFO, numInserted + " records inserted into table: " + table);
    }
}
