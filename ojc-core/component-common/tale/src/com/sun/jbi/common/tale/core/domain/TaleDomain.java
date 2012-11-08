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
 * @(#)TaleDomain.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

import java.util.Properties;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.tale.client.TaleClient;
import com.sun.jbi.common.tale.core.connection.ConnectionConfiguration;
import com.sun.jbi.common.tale.core.connection.ConnectionProperties;
import com.sun.jbi.common.tale.core.connection.DBConnectionFactory;
import com.sun.jbi.common.tale.core.connection.DBConnectionFactory.DBType;
import com.sun.jbi.common.tale.core.connection.impl.ConnectionConfigurationImpl;
import com.sun.jbi.common.tale.core.persist.ALEDBSchemaCreation;
import com.sun.jbi.common.tale.core.persist.dao.DAO;
import com.sun.jbi.common.tale.core.persist.dao.DAOException;
import com.sun.jbi.common.tale.core.persist.dao.DerbyDAO;
import com.sun.jbi.common.tale.core.util.TaleConfigurationException;
import com.sun.jbi.common.tale.core.util.I18n;
import com.sun.jbi.common.util.Util;

/**
 * Represents the ALE domain model.
 * @author Kevan Simpson
 */
public class TaleDomain {
    private DBConnectionFactory mDBConnectionFactory;
    private ComponentContext mContext;
    
    /**
     * Factory method to create an instance of <code>TaleDomain</code>.
     * <p>
     * <b>NOTE:</b> This method is intended for use by the ALE-SE, not the {@link TaleClient}.
     * 
     * @param ctx The ALE-SE component context.
     * @param config The configuration of ALE-SE.
     * @return a domain instance.
     */
    public static TaleDomain newInstance(ComponentContext ctx, ComponentConfig config) {
        TaleDomain domain = new TaleDomain();
        domain.initialize(ctx, config);
        return domain;
    }
    
    /**
     * Factory method to create an instance of <code>TaleDomain</code>.
     * <p>
     * The following properties are expected to be defined in the specified configuration:
     * <ul>
     *      <li>{@link ConnectionProperties#DB_TYPE}</li>
     *      <li>{@link ConnectionProperties#DatabaseJNDIName}</li>
     *      <li>{@link ConnectionProperties#DB_INSTANCE}</li>
     *      <li>{@link ConnectionProperties#DB_HOST}</li>
     *      <li>{@link ConnectionProperties#DB_PORT}</li>
     *      <li>{@link ConnectionProperties#DB_USERNAME}</li>
     *      <li>{@link ConnectionProperties#DB_PASSWORD}</li>
     * </ul></p>
     * 
     * <p>
     * <b>NOTE:</b> This method is intended for use by the {@link TaleClient}.
     * 
     * @param prop Configuration properties of ALE database.
     * @return a domain instance.
     */
    public static TaleDomain newInstance(Properties prop) {
        return new TaleDomain(new DBConnectionFactory(prop));
    }
    
    /** Default no-arg constructor. */
    protected TaleDomain() {
    }
    
    /* Used for testing only. */
    protected TaleDomain(DBConnectionFactory fac) {
//        mConnection = DriverManager.getConnection(
//                "jdbc:derby://localhost:1527/aleseDS", "alese_user", "alese_user");
        mDBConnectionFactory = fac;
    }
    
    public Logger getDomainLogger(String logger) {
        // TODO is this appropriate? it's to avoid exposing ComponentContext to TaleService
        return Util.getLogger(mContext, logger);
    }
    
    /**
     * Instantiates and returns a new {@link DAO}.
     * 
     * @return a <code>DAO</code> instance.
     * @throws DAOException if an error occurs creating DAO.
     */
    public DAO newDAO() throws DAOException {
        DAO dao = null;
        DBType dbType = mDBConnectionFactory.getDBType();
        switch (dbType) {
            case DERBY_DB:
            default:
                dao = new DerbyDAO(mDBConnectionFactory);
                break;
        }

        return dao;
    }

    protected void initialize(ComponentContext ctx, ComponentConfig config) {
        // DB Initialization
        Properties dbProps = new Properties();
        String datasourceJndiName = config.getProperty(
                ConnectionProperties.DatabaseJNDIName).getValue();
        dbProps.put(ConnectionProperties.DatabaseJNDIName, datasourceJndiName);//"jdbc/aleseDS");
        // Create the DB Connection Pool and the JNDI JDBC resource if not already created
        ConnectionConfiguration connConfig = new ConnectionConfigurationImpl(ctx, dbProps);
        if (!connConfig.checkJDBCResource()) {
            connConfig.createConnPoolAndResource();     
        }
        if (mDBConnectionFactory == null) {
            mDBConnectionFactory = new DBConnectionFactory(dbProps, ctx.getNamingContext());
        }
        
        boolean isSchemaVerified = ALEDBSchemaCreation.getInstance()
                .checkTablesIntegrity(dbProps, mDBConnectionFactory);
        if (!isSchemaVerified) {
            ALEDBSchemaCreation.getInstance().createTables(dbProps, mDBConnectionFactory);
            isSchemaVerified = ALEDBSchemaCreation.getInstance()
                    .checkTablesIntegrity(dbProps, mDBConnectionFactory);
            if (!isSchemaVerified) {
                throw new TaleConfigurationException(I18n.loc(
                        "TALE-7015: ALELSE Persistence Schema for JDBC Resource {0} not Valid. " +
                        " Please verify that persistence schema exists and contains all the required tables.", 
                        datasourceJndiName));
            }
            // Populate the newly created alese tables with the sample data 
            ALEDBSchemaCreation.getInstance().populateTables(dbProps, mDBConnectionFactory);
        }
        
        //initialize identity column for db operations
        mDBConnectionFactory.readIdentity();
    }
}
