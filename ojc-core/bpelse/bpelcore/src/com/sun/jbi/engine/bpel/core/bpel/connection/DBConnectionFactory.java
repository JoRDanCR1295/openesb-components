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
 * @(#)DBConnectionFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.connection;

import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELConfigurationException;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * @author Sun Microsystems
 */
public class DBConnectionFactory {
	
	/* Log handle */
	private static final Logger LOGGER = Logger.getLogger(DBConnectionFactory.class.getName());
	
	/* The system property name to get the retry interval from */
//	private static final String DB_RETRY_INT_PROPERTY_IN_SEC = "com.sun.jbi.bpelse.dbConnRetryIntInSec";
	
	/* */
	private static final String DERBY_DATABSE = "Apache Derby";

	/* */
    private static final String ORACLE_DATABSE = "Oracle";
    
    /* */
    private static final String MYSQL_DATABSE = "MySQL";
    
    /* */
    private static final String POSTGRES_DATABSE = "PostgreSQL";

    /* */
    private static final String SUN_DATASOURCE_IMPL_CLASSNAME = "com.sun.appserv.jdbc.DataSource";
    
//    /* */
//    private static final String SQL_SELECT_COUNT_FROM_ENGINE = 
//    	"select COUNT(*) from " + PersistenceDBSchemaCreation.ENGINE;
    
    /* */
    private static final Object mRetrySyncObj = new Object();
    
    /* */
    private static final Object mXARetrySyncObj = new Object();
    
    /* */
    private static boolean mRetryInProgress = false;
    
    /* */
    private static boolean mXARetryInProgress = false;
    
	
	/* Flag to indicate the nonXADatasource is sun's implementation. To be more specific, if it is an instance of
	 * the 'com.sun.appserv.jdbc.DataSource' class.
	 */
	private boolean mNonXADSIsSunImpl = false;
	
	/* Flag to indicate the XADatasource is sun's implementation. To be more specific, if it is an instance of
	 * the 'com.sun.appserv.jdbc.DataSource' class.
	 */
	private boolean mXADSIsSunImpl = false;
	
	/* The method 'markConnectionAsBad(Connection conn) of the class 'com.sun.appserv.jdbc.DataSource' */
	private Method mMarkConnectionAsBad;
	
	/* Reference to the BPEL SE engine. Required for DB retry logic. 
	 * If engine is null, the retry logic will not go on indefinitely. 
	 */
	private Engine mEngine = null;
	
	/* */
    private int mDbType;

    /* Indicates if the current usage is from a test environment */
    private boolean mIsTestEnv;
    
    /* The datasource for XA DB connections */
    private DataSource xaDataSourceInstance;
    
    /* The datasource for non XA DB connections */
    private DataSource nonXaDataSourceInstance;
    
    /**
     * @param prop
     * @param initialContext
     * @param engine
     */
    public DBConnectionFactory(Properties prop, InitialContext initialContext, Engine engine) {
    	
    	mEngine = engine;

    	String datasourceNonXAJndiName = prop.getProperty(ConnectionProperties.DatabaseNonXAJNDIName);
    	String datasourceXAJndiName = prop.getProperty(ConnectionProperties.DatabaseXAJNDIName);
        try {
            nonXaDataSourceInstance = (DataSource) initialContext.lookup(datasourceNonXAJndiName);
            
        } catch (NamingException nonXaExep) {
        	String msg = I18n.loc("BPCOR-7001: Exception while looking up BPELSE Persistence Schema JDBC Non-XA Resource with the JNDI name {0}. Exception Detals : {1} ", datasourceNonXAJndiName, nonXaExep.getMessage());
			LOGGER.log(Level.SEVERE, msg);  
        	throw new BPELConfigurationException(msg);
        }
        
        try {
        	xaDataSourceInstance = (DataSource) initialContext.lookup(datasourceXAJndiName);
        } catch (NamingException xaExep) {
        	String msg = I18n.loc("BPCOR-7032: Exception while looking up BPELSE Persistence Schema JDBC XA Resource with the JNDI name {0}. Exception details :  {1} ", datasourceXAJndiName, xaExep.getMessage());
        	throw new BPELConfigurationException(msg);
        }
        

        try {
        	this.mDbType = getDBType(prop);
			verifyXAandNonMetaInfo(prop);
		} catch (SQLException e) {
        	String msg = I18n.loc("BPCOR-7036: Could not get Database Connection Meta Data from JDBC resource with the JNDI name {0}. " +
        			"\nException Details : {1} ", datasourceNonXAJndiName, e.getMessage());
            BPELTraceManager.getInstance().alertUnableToConnectToDB(mEngine.getId(), e.getMessage());
			LOGGER.log(Level.SEVERE, msg);  
        	throw new BPELConfigurationException(msg);
		}
		
		doSunDatasourceSpecficTasks();
        doJUnitSpecificTasks(prop);
    }

    /**
     * Verify that Non-XA and XA Connections are configured to point to the same underlying database/user/schema.
     * @throws SQLException 
     */
    private void verifyXAandNonMetaInfo(Properties prop) throws SQLException {
        Connection nonxaConn = null;
        Connection xaConn = null;
        
		try {
			nonxaConn = nonXaDataSourceInstance.getConnection();
			String urlNonXAConn = nonxaConn.getMetaData().getURL();
			String userNonXAConn = nonxaConn.getMetaData().getUserName();
			
			xaConn = xaDataSourceInstance.getConnection();
			String urlXAConn = xaConn.getMetaData().getURL();
			String userXAConn = xaConn.getMetaData().getUserName();

			if (!userNonXAConn.equals(userXAConn)) {
				String msg = I18n.loc("BPCOR-7003: The configured user for Non-XA and XA Connection for BPEL SE does not match. " +
						"These need to match. \nFound : Non-XA Connection User - {0}\nXA Connection User - {1}", userNonXAConn, userXAConn);
				throw new BPELConfigurationException(msg);
			}
				
			if (!urlNonXAConn.equals(urlXAConn)) {
				String msg = I18n.loc("BPCOR-7039: The configured Non-XA and XA Connection Meta Data (URL) for BPEL SE does not match. " +
						"These need to match. \nFound : Non-XA Connection URL - {0}\nXA Connection URL - {1}", urlNonXAConn, urlXAConn);
				throw new BPELConfigurationException(msg);
			}
			
		} finally {
			if (nonxaConn != null) {
				try {
					nonxaConn.close();
				} catch (SQLException ex) {
					LOGGER.log(Level.WARNING, I18n .loc("BPCOR-6000: Error when trying to close DB cursors"), ex);
				}
			}
			if (xaConn != null) {
				try {
					xaConn.close();
				} catch (SQLException ex) {
					LOGGER.log(Level.WARNING, I18n .loc("BPCOR-6000: Error when trying to close DB cursors"), ex);
				}
			}
		}
	}

	/**
     * The following will also determine if the configured database is one of the supported types.
     * 
     * @param prop
     * @return
	 * @throws SQLException 
     */
    private int getDBType(Properties prop) throws SQLException {
        Connection conn = null;
        int dbType = -1;
        
		try {
			conn = nonXaDataSourceInstance.getConnection();
			DatabaseMetaData meta = conn.getMetaData();
			String databaseProductName = meta.getDatabaseProductName();

			// DO we need to check for the supported version also ?
			if (databaseProductName.equalsIgnoreCase(DERBY_DATABSE)) {
				dbType = ConnectionProperties.DERBY_DB.intValue();
			} else if (databaseProductName.equalsIgnoreCase(ORACLE_DATABSE)) {
				dbType = ConnectionProperties.ORCL_DB.intValue();
			} else if (databaseProductName.equalsIgnoreCase(MYSQL_DATABSE)) {
				dbType = ConnectionProperties.MYSQL_DB.intValue();
			} else if (databaseProductName.equalsIgnoreCase(POSTGRES_DATABSE)) {
				dbType = ConnectionProperties.POSTGRES_DB.intValue();
			} else {
				String msg = I18n.loc("BPCOR-7002: Database Not Supported: {0} Supported Databases : {1} , {2}, {3} and {4}",
								databaseProductName, DERBY_DATABSE, ORACLE_DATABSE, MYSQL_DATABSE, POSTGRES_DATABSE);
				throw new BPELConfigurationException(msg);
			}
			
        } finally {
			if (conn != null) {
				try {
					conn.close();
				} catch (SQLException ex) {
					LOGGER.log(Level.WARNING, I18n .loc("BPCOR-6000: Error when trying to close DB cursors"), ex);
				}
			}
		}
		
		return dbType;
    }
    

    /**
	 * Check to see if the Datasource (XA and nonXA) is sun's implementation.
	 * if yes we will take advantage of the method 'markConnectionAsBad'
	 * which sun's implementation provides. Using reflection to avoid compile
	 * time as well as runtime dependencies on proprietary Sun class.
	 */
    private void doSunDatasourceSpecficTasks() {
        try {
        	Class sunDSClass = Class.forName(SUN_DATASOURCE_IMPL_CLASSNAME);
        	// The DataSource is an instance of Sun's implementation. So we can go ahead and take advantage of the
    		// proprietary method markConnectionAsBad
    		mMarkConnectionAsBad = sunDSClass.getMethod("markConnectionAsBad", new Class[]{Connection.class});
    		
    		if (sunDSClass.isInstance(nonXaDataSourceInstance)) {
        		mNonXADSIsSunImpl = true;
        	}
        	
    		if (sunDSClass.isInstance(xaDataSourceInstance)) {
        		mXADSIsSunImpl = true;
        	}		
        } catch (Exception e) {
            /* Exception. Do nothing. mNonXADSIsSunImpl is already set to false.*/
        } 
    }
    
    /**
     *  check if this is JUnit test. Otherwise, get the connection meta and get the DB Type from it.
     *  TODO this JUnit specifc code be removed and moved outside of main engine execution ?
     */
    private void doJUnitSpecificTasks(Properties prop) {
        String strIsTestEnv = prop.getProperty("IsTestEnvironment"); 
        if (!Utility.isEmpty(strIsTestEnv)) {
            mIsTestEnv = Boolean.valueOf(strIsTestEnv);
        }
    }
    
    /**
	 * return database type
	 * 
	 * @return int database type
	 */
    public int getType() {
        return mDbType;
    }

    /**
     * Creates and returns a DBNonXAConnection that has a non transactional Connection object as its underlying 
     * connection.
     * Created using the resource type javax.sql.DataSource.
     * @return com.sun.jbi.engine.bpel.connection.DBNonXAConnection
     * @throws Exception throws Exception if a connection could not be obtained.
     */
    public AbstractDBConnection createNonXAConnection() throws SQLException {
    	// DEVNOTE: Keep in mind that the getConnection method might take some time to return a connection 
    	// when it is out of good connections. Also the getConnection call behaviour seems to be 
    	// synchronized. Hence when more then one of the bpel se threads try for a connection at the 
    	// the same time, it will be serial, ie one after the other.
    	Connection conn = nonXaDataSourceInstance.getConnection();
        if (mIsTestEnv) {
            return (AbstractDBConnection) conn;
        }
        AbstractDBConnection dbConn = new DBNonXAConnection(conn);
    	return dbConn;
    }
    
    /**
     * Creates and returns a connection that is enlisted with the TM and should be used within XA transactions only.
     * Created using the resource type javax.sql.XADataSource.
     * @return DBConnection
     * @throws Exception Throws exception if a connection could not be obtained
     */
    public AbstractDBConnection createXAConnection() throws Exception {
        Connection conn = xaDataSourceInstance.getConnection();
        if (mIsTestEnv) {
            return (AbstractDBConnection) conn;
        }
        AbstractDBConnection dbConn = new DBConnection(conn);
        return dbConn;
    }
    
    /**
     * 
     * @param conn
     * @return
     */
    public boolean validateNonXAConnection(AbstractDBConnection conn) {
    	if (conn != null) {
    		Connection sqlConn = conn.getUnderlyingConnection();
    		// Try to validate the connection and if it is bad invalidate the connection.
    		if (!isConnectionValid(sqlConn)) {
    			markNonXAConnectionAsBad(sqlConn);
    			return false;
    		}
    		return true;
    	} else {
    		return false;
    	}
    }
    
    /**
     * 
     * @param conn
     * @return
     */
    public boolean validateXAConnection(AbstractDBConnection conn) {
    	if (conn != null) {
    		Connection sqlConn = conn.getUnderlyingConnection();
    		// Try to validate the connection and if it is bad invalidate the connection.
    		if (!isConnectionValid(sqlConn)) {
    			markXAConnectionAsBad(sqlConn);
    			return false;
    		}
    		return true;
    	} else {
    		return false;
    	}
    }
    
    /**
     * 
     * @param conn
     * @param exc
     * @return
     */
    public boolean doConnectionRetry(AbstractDBConnection conn, Exception exc) {
    	// Validate the connection only if the engine reference is not null, the exception is not null and is 
    	// an instance of the SQLException.
    	if ((mEngine != null) && (exc != null) && (exc instanceof SQLException)) {
    		// This block determines if a connection retry should be done. This will be true if the DB was indeed
    		// unavailable.
    		boolean retry = true;
    		if (conn != null) {
    			retry = (!validateNonXAConnection(conn));
    		}

    		if (retry) {
    			if (LOGGER.isLoggable(Level.FINE)) {
    				LOGGER.log(Level.FINE, 
    						I18n.loc("BPCOR-3002: Connection unavailable. Retrying for database connection every " + 
    								"{0} seconds.", mEngine.getRetryInterval()));
    			}
                try {
                	if (conn != null) {
                		conn.close();
                	}
                } catch (SQLException e) {
                    e.printStackTrace();
                }
    			BPELTraceManager.getInstance().alertUnableToConnectToDB(mEngine.getId(), exc.getMessage());
    			if (doNonXAConnectionRetry()) {
    				return true;
    			} else {
    				LOGGER.log(Level.SEVERE, 
    						I18n.loc("BPCOR-7004: Aborting database connection retry. BPELSE is stopped"));
    			}
    		} 
    	} 
    	
    	return false;
    }
    
    public boolean doConnectionRetryForXA() {
    	BPELTraceManager.getInstance().alertUnableToConnectToDB(mEngine.getId(), "XADBConnection bad");
    	return doXAConnectionRetry();
    }
    
    /*
     * 
     */
    private boolean doNonXAConnectionRetry() {
    	
    	if (mEngine != null) {
    		synchronized (mRetrySyncObj) {
    			if (mEngine.getState() == Engine.STOPPED) {
    				return false;
    			}

    			if (!mRetryInProgress) {
    				mRetryInProgress = true;
    				boolean dbConnRestored = isConnectionRestored();
    				while (!dbConnRestored) {
    					try {
    						mRetrySyncObj.wait(mEngine.getRetryInterval() * 1000);
    					} catch (InterruptedException ie) {/* Do nothing. */}

    					if (mEngine.getState() == Engine.STOPPED) {
    						break;
    					}
    					dbConnRestored = isConnectionRestored();
    				}
    				mRetryInProgress = false;
    				mRetrySyncObj.notifyAll();
    				if (dbConnRestored) {
                        BPELTraceManager.getInstance().alertDBConnectionRestored(mEngine.getId());
                    }
    				return dbConnRestored;
    			} else {
    				try {
    					mRetrySyncObj.wait();
    				} catch (InterruptedException ie) {/* do nothing */}

    				if (mEngine.getState() == Engine.STOPPED) {
    					return false;
    				}
    				return true;
    			}
    		}
    	} else {
    		return false;
    	}
    }
    
    /*
     * 
     */
    private boolean doXAConnectionRetry() {
    	
    	if (mEngine != null) {
    		synchronized (mXARetrySyncObj) {
    			if (mEngine.getState() == Engine.STOPPED) {
    				return false;
    			}

    			if (!mXARetryInProgress) {
    				mXARetryInProgress = true;
    				boolean dbConnRestored = isXAConnectionRestored();
    				while (!dbConnRestored) {
    					try {
    						mXARetrySyncObj.wait(mEngine.getRetryInterval() * 1000);
    					} catch (InterruptedException ie) {/* Do nothing. */}

    					if (mEngine.getState() == Engine.STOPPED) {
    						break;
    					}
    					dbConnRestored = isXAConnectionRestored();
    				}
    				mXARetryInProgress = false;
    				mXARetrySyncObj.notifyAll();
    				if (dbConnRestored) {
                        BPELTraceManager.getInstance().alertDBConnectionRestored(mEngine.getId());
                    }
    				return dbConnRestored;
    			} else {
    				try {
    					mXARetrySyncObj.wait();
    				} catch (InterruptedException ie) {/* do nothing */}

    				if (mEngine.getState() == Engine.STOPPED) {
    					return false;
    				}
    				return true;
    			}
    		}
    	} else {
    		return false;
    	}
    }
    
    /**
     * Returns the XA DataSource
     * @return
     */
    public DataSource getXADataSource () {
    	return xaDataSourceInstance;
    }
    
    /**
     * Returns None XA DataSource
     * @return
     */
    public DataSource getNonXADataSource () {
    	return nonXaDataSourceInstance;
    }
    
    /*
     * 
     */
    private void markNonXAConnectionAsBad(Connection sqlConn) {
    	// DEVNOTE vm: Glassfish specific code
    	// This actual code if we had imported the glassfish specific class 'com.sun.appserv.jdbc.DataSource' would
    	// look like this.
    	
    	//    	if (nonXaDataSourceInstance instanceof com.sun.appserv.jdbc.DataSource) {
    	//    		com.sun.appserv.jdbc.DataSource sunDataSourceInstance = 
    	//        		(com.sun.appserv.jdbc.DataSource) nonXaDataSourceInstance;
    	//        	sunDataSourceInstance.markConnectionAsBad(sqlConn);
    	//    	}
    	
    	// However since we don't want to have a dependency on that class during compile time or during runtime
    	// (we could be in an AppServer which uses a different implementation of the DataSource) we use reflection
    	// to invoke the markConnectionAsBad method of com.sun.appserv.jdbc.DataSource. The determination of the
    	// DataSource implementation is done in the constructor of this class.
    	
    	if (mNonXADSIsSunImpl) {
    		try {
    			mMarkConnectionAsBad.invoke(nonXaDataSourceInstance, new Object[] {sqlConn});
    		} catch (Exception e) {
    			LOGGER.log(Level.WARNING, 
    					I18n.loc("BPCOR-6001: Datasource is sun implementation. However the connection could not be marked as bad."), 
    					e);
    			/*on any exception do nothing and let processing continue.*/
    		}
    	}
    }
    
    /*
     * 
     */
    private void markXAConnectionAsBad(Connection sqlConn) {
    	// see markNonXAConnectionAsBad for DEVNOTES.
    	if (mXADSIsSunImpl) {
    		try {
    			mMarkConnectionAsBad.invoke(xaDataSourceInstance, new Object[] {sqlConn});
    		} catch (Exception e) {
    			LOGGER.log(Level.WARNING, 
    					I18n.loc("BPCOR-6001: Datasource is sun implementation. However the connection could not be marked as bad."), 
    					e);
    			/*on any exception do nothing and let processing continue.*/
    		}
    	}
    }
    
    /*
     * 
     */
    private boolean isConnectionValid(Connection sqlConn) {
    	
    	// if a SQLException occurs while trying to execute the trivial query assume that the connection is bad,
    	// i.e. most probably the Database is unavailable. Emphasis on 'most probably' since we can
    	// not be 100% sure that this is a connection problem (Not without looking at the errorcodes, which are
    	// vendor specific). However since the sql is trivial it should have gone through unless perhaps the  
    	// Engine table does not exist. This would still be reason enough to shutdown the bpelse.
    	
    	boolean connValid = true;
    	PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            //Use metaData to ensure working for both persistence and monitoring
//            stmt = sqlConn.prepareStatement(SQL_SELECT_COUNT_FROM_ENGINE);
            rs = sqlConn.getMetaData().getSchemas();
        } catch (SQLException sqlExc) {
        	// Set the return value.
        	connValid = false;
        } finally {
        	try {
        		if (rs != null) {
        			rs.close();
        		}
            } catch (SQLException se) {/* Do Nothing */};

            try {
            	if (stmt != null) {
            		stmt.close();
            	}
            } catch (SQLException se) {/* Do Nothing */};
        }
    	return connValid;
    }
    
    /*
     * 
     */
    private boolean isConnectionRestored() {
    	// Check to see if it is possible to get a good DBConnection.
		AbstractDBConnection dbConn = null;
		boolean dbConnRestored = false;
		try {
			dbConn = createNonXAConnection();
			dbConnRestored = validateNonXAConnection(dbConn);
		} catch (Exception e) {
			// do nothing. The database connection is still unavailable.
		} finally {
			try {
				if (dbConn != null) {
					dbConn.close();
				}
			} catch (Exception e) {/* do nothing */}
		}
		
		return dbConnRestored;
    }
    
    /*
     * 
     */
    private boolean isXAConnectionRestored() {
    	// Check to see if it is possible to get a good DBConnection.
		AbstractDBConnection dbConn = null;
		boolean dbConnRestored = false;
		try {
			dbConn = createXAConnection();
			dbConnRestored = validateXAConnection(dbConn);
		} catch (Exception e) {
			// do nothing. The database connection is still unavailable.
		} finally {
			try {
				if (dbConn != null) {
					dbConn.close();
				}
			} catch (Exception e) {/* do nothing */}
		}
		
		return dbConnRestored;
    }
    
    /**
     * Notifies any objects that are waiting on the DBConnection retry lock.
     */
    public static void notifyBlockedThreads() {
    	synchronized (mRetrySyncObj) {
        	mRetrySyncObj.notifyAll();
        }
    	synchronized (mXARetrySyncObj) {
    		mXARetrySyncObj.notifyAll();
    	}
    }
    
    public Engine getEngine () {
        return mEngine;
    }
    
}
