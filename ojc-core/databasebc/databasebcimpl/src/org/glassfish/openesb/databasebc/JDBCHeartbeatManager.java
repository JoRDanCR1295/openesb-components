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
 * @(#)JDBCHeartbeatManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

import javax.jbi.component.ComponentContext;
import javax.naming.Context;
import javax.naming.NamingException;
import javax.sql.DataSource;
import javax.xml.namespace.QName;

/**
 * author : Venkat P Process requests received from the External Database
 */
public class JDBCHeartbeatManager implements Runnable {

    EndpointBean epb;

    private ComponentContext mContext;

    private RuntimeConfiguration mRuntimeConfig;

    private AtomicBoolean mMonitor;

    PreparedStatement ps = null;

    Connection connection = null;

    private String mTableName = null;

    private int mPollMilliSeconds = 10000;

    private QName mOperation;

    private static final Messages mMessages = Messages.getMessages(JDBCHeartbeatManager.class);

    private static final Logger mLogger = Messages.getLogger(JDBCHeartbeatManager.class);

    /*
     * Constructor
     */
    public JDBCHeartbeatManager(final EndpointBean endpoint, final ComponentContext context, final QName opname)
            throws Exception {
        epb = endpoint;
        mContext = context;
        mMonitor = new AtomicBoolean(false);
        mOperation = opname;
    }

    // @Override
    public void run() {
        do {
            try {
                execute();
            } catch (final Exception ex) {
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11102.JHM_ERROR_WHILE_EXECUTING_SQL"), ex);
            }
            try {
                Thread.sleep(mPollMilliSeconds);
            } catch (final Exception e) {
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11103.JHM_THREAD_SLEEP_ABRUPTED"), e);
            }
        } while (mMonitor.get() != Boolean.TRUE);
    }

    /**
     * @throws Exception
     */
    public void execute() throws Exception {
        try {
            Map operationNameToMetaData = (Map) epb.getValueObj(EndpointBean.OPERATION_NAME_TO_META_DATA);
            OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(mOperation.getLocalPart());
            mPollMilliSeconds = meta.getJDBCSql().getPollMilliSeconds();
            mTableName = meta.getJDBCSql().getTableName();
            if (epb.isClustered()) {
                try {
                    if (this.connection == null) {
                        connection = getDatabaseConnection(mRuntimeConfig.getProperties().getProperty(
                                RuntimeConfiguration.CONFIG_CLUSTER_DATABASE_JNDINAME));
                        connection.setAutoCommit(true);
                    }
                } catch (NamingException ne) {
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11104.JHM_ERROR_IN_LOOKUP",
                            new Object[] { new Object[] { mRuntimeConfig.getProperties().getProperty(
                                    RuntimeConfiguration.CONFIG_CLUSTER_DATABASE_JNDINAME) } }), ne);
                    throw ne;
                } catch (SQLException se) {
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11105.JHM_ERROR_WHILE_GETTING_CONNECTION"), se);
                    throw se;
                }
                upDateHeartBeat();
            }
        } catch (final Exception e) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11102.JHM_ERROR_WHILE_EXECUTING_SQL"), e);
            throw e;
        }
    }

    /**
     * @param jndiName
     * @return
     * @throws javax.naming.NamingException
     */
    private Object getDataSourceFromContext(final String jndiName) throws javax.naming.NamingException {
        final Context c = mContext.getNamingContext();
        return c.lookup(jndiName);
    }

    /**
     * @param jndiName
     * @return
     * @throws Exception
     */
    private Connection getDatabaseConnection(final String jndiName) throws SQLException, NamingException {
        return ((DataSource) getDataSourceFromContext(jndiName)).getConnection();
    }

    /*
     * stop the thread to update the Hearbeat.
     */

    protected void stopReceiving() {
        mMonitor.set(Boolean.TRUE);
        try {
            if (connection != null) {
                connection.close();
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11106.JHM_ERROR_WHILE_CLOSING_CONNECTION"), e);
        }
    }

    /*
     * Runtime Config object to cluster JNDI name
     */
    public void setRuntimeConfig(RuntimeConfiguration runtimeConfg) {
        mRuntimeConfig = runtimeConfg;
    }

    /*
     * used to update the instance heartbeat per each poll to know instance is alive or not
     */

    private boolean upDateHeartBeat() {
        boolean retry;
        boolean updated;
        do {
            retry = false;
            updated = false;
            PreparedStatement ps = null;
            Connection con = null;
            String BASE_INSTANCESTATE_UPDATE_STMT_STR = "UPDATE INSTANCESTATE" + //$NON-NLS-1$
                    " SET lastupdatetime = CURRENT_TIMESTAMP " + "WHERE INSTANCEID = ? and TABLENAME = ?"; //$NON-NLS-1$ 

            String BASE_INSTANCESTATE_INSERT_STMT_STR = "INSERT INTO INSTANCESTATE" + //$NON-NLS-1$
                    " VALUES(?, CURRENT_TIMESTAMP, ?)"; //$NON-NLS-1$

            try {
                con = connection;
                ps = con.prepareStatement(BASE_INSTANCESTATE_UPDATE_STMT_STR);
                ps.setString(1, epb.getInstanceName());
                ps.setString(2, mTableName);
                int updateCount = ps.executeUpdate();
                if (updateCount == 0) {
                    // this indicates that no entry exists for this instance id,
                    // insert new one.
                    ps = con.prepareStatement(BASE_INSTANCESTATE_INSERT_STMT_STR);
                    ps.setString(1, epb.getInstanceName());
                    ps.setString(2, mTableName);
                    int inserted = ps.executeUpdate();
                }
                mLogger.log(Level.INFO, mMessages.getString("DBBC_R10902.JHM_UPDATED_TIME_STAMP",
                        new Object[] { new Object[] { epb.getInstanceName() } }));
                updated = true;
            } catch (SQLException e) {
                if (con != null) {
                    try {
                        con.rollback();
                    } catch (SQLException ex) {
                        mLogger.log(Level.WARNING, mMessages.getString("DBBC_W11001.JCM_EXCEPTION_WHILE_ROLLBACK"), ex);
                    }
                } else {
                    // TODO retry for the connection
                }
            } finally {
                if (ps != null) {
                    try {
                        ps.close();
                    } catch (SQLException e) {
                        mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"), e);
                    }
                }
            }
        } while (retry);
        return updated;
    }

}
