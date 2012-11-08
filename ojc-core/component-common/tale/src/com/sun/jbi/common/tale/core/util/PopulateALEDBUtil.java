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
 * @(#)PopulateALEDBUtil.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.tale.core.connection.DBConnectionFactory;
import com.sun.jbi.common.tale.core.connection.PopulateUtilDBConnectionFactory;
import com.sun.jbi.common.tale.core.domain.TaleDomain;
import com.sun.jbi.common.tale.core.domain.LoggerChannel;
import com.sun.jbi.common.tale.core.domain.LoggerCode;
import com.sun.jbi.common.tale.core.domain.RepUser;
import com.sun.jbi.common.tale.core.persist.dao.DAO;
import com.sun.jbi.common.tale.core.persist.dao.DAOException;

/**
 * The utlity for populating different ALE configuration tables like CSF_LOGGER_CHANNELS,
 * CSF_LOGGER_CODES, etc. from .CSV files.
 * @author Sun Microsystems
 */
public class PopulateALEDBUtil {

    static PopulateUtilDBConnectionFactory connFactory = null;
    
    static PopulateUtilALEDomain domain = null;

    static final String REPUSERS_CSVFILE = "RepUsersCSVFile";

    static final String LOGGERCHANNELS_CSVFILE = "LoggerChannelsCSVFile";

    static final String LOGGERCODES_CSVFILE = "LoggerCodesCSVFile";

    static Logger mLogger = Logger.getLogger(PopulateALEDBUtil.class.getName());

    public enum CONFIG_TABLE {
        /** CSF_REP_USERS */
        CSF_REP_USERS(REPUSERS_CSVFILE),
        /** CSF_LOGGER_CHANNELS */
        CSF_LOGGER_CHANNELS(LOGGERCHANNELS_CSVFILE),
        /** CSF_LOGGER_CODES */
        CSF_LOGGER_CODES(LOGGERCODES_CSVFILE);

        private String mCSVFileName = "";

        CONFIG_TABLE(String fileName) {
            mCSVFileName = fileName;
        }

        public String getCSVFileName() {
            return mCSVFileName;
        }

    }

    static Properties configProp = new Properties();

    public static void main(String[] args) {
        init(args[0]);
        populateConfigTables();
    }

    public static void init(String configFile) {
        try {
            configProp.load(new FileInputStream(new File(configFile)));
            connFactory = new PopulateUtilDBConnectionFactory(configProp);
            domain = new  PopulateUtilALEDomain(connFactory);
        } catch (Exception ie) {
            String message = I18n.loc(
                            "TALE-7011: Error reading the config file. \nException Detals : {0} ",
                            ie.getMessage());
            mLogger.log(Level.SEVERE, message);
        }
    }

    /**
     * For each table defined in the enumeration CONFIG_TABLE, 
     * call the populate table method
     *
     */
    public static void populateConfigTables() {
        for (CONFIG_TABLE tb : CONFIG_TABLE.values()) {
            String csvFileName = configProp.getProperty(tb.getCSVFileName());
            if (csvFileName != null && csvFileName.trim().length() != 0) {
                populateTable(tb, csvFileName);
            } else {
                mLogger.log(Level.INFO,I18n.loc(
                        "TALE-5001: CSV File Name for Table {0} not provided. Ignored.", tb));
            }
        }
    }

    /**
     * Populates the data for the table from the corresponding .CSV File
     * @param tableName Configuration Table Name
     * @param fileName CSV File location which contains the data
     */
    private static void populateTable(CONFIG_TABLE tableName, String fileName) {
        File configFile = new File(fileName);
        try {
            FileReader fr = new FileReader(configFile);
            BufferedReader InputFile = new BufferedReader(fr);
            String currentRecord = InputFile.readLine();
            // Assuming the first line will be the column names, go to the next line
            currentRecord = InputFile.readLine();
            
            while (currentRecord != null) {
                if (currentRecord.trim().length() != 0) {
                    String[] stArray = currentRecord.split(",");
                    try {
                        switch (tableName) {
                        case CSF_REP_USERS:
                            insertRepUserRecord(stArray);
                            break;
                        case CSF_LOGGER_CHANNELS:
                            insertLoggerChannelRecord(stArray);
                            break;
                        case CSF_LOGGER_CODES:
                            insertLoggerCodeRecord(stArray);
                            break;
                        }
                    } catch (DAOException ex) {
                        String message = I18n.loc(
                                        "TALE-7012: Error while inserting following record into {0}. " +
                                        "\n{1} \nThe exception is: {2} \nThe Cause is: {3}",
                                        tableName, currentRecord, ex, ex.getCause());
                        message = "\n\n" + message;
                        mLogger.log(Level.SEVERE, message);
                    }
                }
                currentRecord = InputFile.readLine();
            }
        } catch (Exception ex) {
            String message = I18n.loc(
                    "TALE-7013: Error reading the config file {0}. \nException Detals : {1} ",
                    fileName, ex.getMessage());
            mLogger.log(Level.SEVERE, message);
        }
    }

    /**
     * Inserts a record into table CSF_REP_USERS
     * @param stArray Array of String tokens
     * @throws DAOException
     */
    private static void insertRepUserRecord(String[] stArray)
            throws DAOException {
        RepUser repUserRecord = new RepUser();
        repUserRecord.setUserId(stArray[0]);
        repUserRecord.setUserDesc(stArray[1]);
        repUserRecord.setUserName(stArray[2]);
        repUserRecord.setUserPassword(stArray[3]);
        if ("Y".equalsIgnoreCase(stArray[4])) {
            repUserRecord.setActiveFlag(true);
        } else {
            repUserRecord.setActiveFlag(false);
        }
        repUserRecord.setCreateID(stArray[5]);
        repUserRecord.setModifiedID(stArray[7]);

        DAO dao = domain.newDAO();
        try {
            dao.beginTransaction();
            dao.insertRepUserRecord(repUserRecord);
            dao.commitTransaction();
        } catch (DAOException e) {
            throw e;
        } finally {
            dao.release();
        }
    }

    /**
     * Inserts a record into table CSF_LOGGER_CHANNELS
     * @param stArray Array of String tokens
     * @throws DAOException
     */
    private static void insertLoggerChannelRecord(String[] stArray)
            throws DAOException {
        LoggerChannel loggerChannelRecord = new LoggerChannel();
        loggerChannelRecord.setLoggerChannelCode(Integer.parseInt(stArray[0]));
        loggerChannelRecord.setChannelType(stArray[1]);
        loggerChannelRecord.setFileName(stArray[2]);
        if ("Y".equalsIgnoreCase(stArray[3])) {
            loggerChannelRecord.setActiveFlag(true);
        } else {
            loggerChannelRecord.setActiveFlag(false);
        }
        loggerChannelRecord.setCreateID(stArray[4]);
        loggerChannelRecord.setModifiedID(stArray[6]);

        DAO dao = domain.newDAO();
        try {
            dao.beginTransaction();
            dao.insertLoggerChannelRecord(loggerChannelRecord);
            dao.commitTransaction();
        } catch (DAOException e) {
            throw e;
        } finally {
            dao.release();
        }
    }

    /**
     * Inserts a reord into table CSF_LOGGER_CODES
     * @param stArray
     * @throws DAOException
     */
    private static void insertLoggerCodeRecord(String[] stArray)
            throws DAOException {
        LoggerCode loggerCodeRecord = new LoggerCode();
        loggerCodeRecord.setLoggerCode(Integer.parseInt(stArray[0]));
        loggerCodeRecord.setLoggerLabel(stArray[1]);
        loggerCodeRecord.setLoggerCategory(stArray[2]);
        loggerCodeRecord.setLoggerLevel(stArray[3]);
        loggerCodeRecord.setLoggerDescription(stArray[4]);
        loggerCodeRecord.setLoggerChannelCode(Integer.parseInt(stArray[5]));
        if ("Y".equalsIgnoreCase(stArray[6])) {
            loggerCodeRecord.setActiveFlag(true);
        } else {
            loggerCodeRecord.setActiveFlag(false);
        }
        loggerCodeRecord.setCreateID(stArray[7]);
        loggerCodeRecord.setModifiedID(stArray[9]);

        DAO dao = domain.newDAO();
        try {
            dao.beginTransaction();
            dao.insertLoggerCodeRecord(loggerCodeRecord);
            dao.commitTransaction();
        } catch (DAOException e) {
            throw e;
        } finally {
            dao.release();
        }
    }
    
    private static class PopulateUtilALEDomain extends TaleDomain {
        public PopulateUtilALEDomain(DBConnectionFactory fac) {
            super(fac);
        }
    }
}
