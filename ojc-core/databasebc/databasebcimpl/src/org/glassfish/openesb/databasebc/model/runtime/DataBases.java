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
 * @(#)DataBases.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.model.runtime;

import com.sun.jbi.internationalization.Messages;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import javax.sql.*;
import javax.sql.XADataSource;
public enum DataBases {
	DERBY,ORACLE,DB2,SQLSERVER,VSAM,MYSQL;
    DBConnectionInfo mdbConnectionInfo = null;

    private static final Logger mLogger = Logger.getLogger(DataBases.class.getName());
    private static final Messages mMessages = Messages.getMessages(DataBases.class);
    
    public int dbType() {
        return ordinal();
    }

    @Override
    public String toString() {
        switch (this) {
        case DERBY:
            return "DERBY";
        case ORACLE:
            return "ORACLE";
        case DB2:
            return "DB2";
        case SQLSERVER:
        	return "SQLSERVER";
        case VSAM:
            return "VSAM";
        case MYSQL:
            return "MYSQL";
        }
        return null;
    }

    public Class getXADataSourceClass() throws ClassNotFoundException {
        switch (this) {
        case DERBY:
            return Class.forName("org.apache.derby.jdbc.ClientXADataSource");
        case ORACLE:
            return Class.forName("oracle.jdbc.xa.client.OracleXADataSource");
        case DB2:
        	return Class.forName("com.ibm.db2.jdbc.DB2XADataSource");
        case SQLSERVER:
        	return Class.forName("com.ddtek.jdbcx.sqlserver.SQLServerDataSource");
        }
        return null;
    }	
    
    public Class getDataSourceClass() throws ClassNotFoundException {
        switch (this) {
        case DERBY:
            return Class.forName("org.apache.derby.jdbc.ClientDataSource");
        case ORACLE:
            return Class.forName("oracle.jdbc.pool.OracleDataSource");		
        case DB2:
        	return Class.forName("com.SeeBeyond.db2.jdbcx.db2.DB2DataSource");
        case SQLSERVER:
        	return Class.forName("com.SeeBeyond.sqlserver.jdbcx.sqlserver.SQLServerDataSource");
        }
        
        return null;
    }	

	public Object buildDataSource() throws ClassNotFoundException , Exception{
		Class dsClass = null;
	
		if (mdbConnectionInfo.getXAEnabled().equalsIgnoreCase("XATransaction"))
				dsClass = getXADataSourceClass();
		else
				dsClass = getDataSourceClass();		
		return buildDataSource(dsClass);		
	}

	public Object buildDataSource(final Class dsclass) throws ClassNotFoundException, Exception {
		try {
			final Object dataSourceInstance = dsclass.newInstance();
            final Class[] stringParam = new Class[] {String.class};
            final Class[] intParam = new Class[]{int.class};
            
            if (this == DERBY) {
            	dsclass.getMethod("setServerName", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { mdbConnectionInfo.getServerName() });
            	dsclass
                    .getMethod("setPortNumber", new Class[] { int.class })
                    .invoke(dataSourceInstance,
                    new Object[] { mdbConnectionInfo.getPortNumber() });
            	dsclass.getMethod("setDatabaseName", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { mdbConnectionInfo.getDatabaseOrSchemaName() });
            	dsclass.getMethod("setUser", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { mdbConnectionInfo.getUsername() });
            	dsclass.getMethod("setPassword", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { mdbConnectionInfo.getPassword() });
            } else if (this == ORACLE) {            	
            	dsclass.getMethod("setURL", stringParam)
					.invoke(dataSourceInstance,
                        new Object[] { mdbConnectionInfo.getDbUrl()});
            	dsclass.getMethod("setUser", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { mdbConnectionInfo.getUsername() });
            	dsclass.getMethod("setPassword", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { mdbConnectionInfo.getPassword() });
            } else if (this == DB2) {
            	dsclass.getMethod("setServerName", stringParam)
					.invoke(dataSourceInstance,
                   new Object[] { mdbConnectionInfo.getServerName()});
            	dsclass.getMethod("setPortNumber", intParam)
					.invoke(dataSourceInstance,
					new Object[] { mdbConnectionInfo.getPortNumber()});
            	dsclass.getMethod("setDatabaseName", stringParam)
            		.invoke(dataSourceInstance,
            		new Object[] { mdbConnectionInfo.getDatabaseOrSchemaName()});        	
            	dsclass.getMethod("setUser", stringParam)
            		.invoke(dataSourceInstance,
                new Object[] { mdbConnectionInfo.getUsername() });
            	dsclass.getMethod("setPassword", stringParam)
            		.invoke(dataSourceInstance,
                new Object[] { mdbConnectionInfo.getPassword() });
            } else if (this == SQLSERVER) {
            	dsclass.getMethod("setServerName", stringParam)
				.invoke(dataSourceInstance,
               new Object[] { mdbConnectionInfo.getServerName()});
            	dsclass.getMethod("setPortNumber", intParam)
					.invoke(dataSourceInstance,
					new Object[] { mdbConnectionInfo.getPortNumber()});
            	dsclass.getMethod("setDatabaseName", stringParam)
        			.invoke(dataSourceInstance,
        			new Object[] { mdbConnectionInfo.getDatabaseOrSchemaName()});
            	dsclass.getMethod("setUser", stringParam)
            		.invoke(dataSourceInstance,
            	new Object[] { mdbConnectionInfo.getUsername() });
            	dsclass.getMethod("setPassword", stringParam)
            		.invoke(dataSourceInstance,
            	new Object[] { mdbConnectionInfo.getPassword() });
            }            
            return dataSourceInstance;
        } catch (final IllegalAccessException iae) {
            mLogger.log(Level.SEVERE,"Illegal Access Exception While Dealing with Driver class",iae);
        } catch (final NoSuchMethodException nsme) {
            mLogger.log(Level.SEVERE,"No Such Method Exception While Dealing with Driver class",nsme);
        } catch (final InvocationTargetException ite) {
            mLogger.log(Level.SEVERE,"Invocation Target Exception While Dealing with Driver class",ite);
        } catch (final InstantiationException ie) {
            mLogger.log(Level.SEVERE," Exception While Instantiating the Driver class",ie);
        }
        return null;
}


    /*public Object buildXADataSource() {
        try {
        	final Class dsclass = getXADataSourceClass();
            final Object dataSourceInstance = dsclass.newInstance();
            final Class[] stringParam = new Class[] { String.class };
            final Class[] stringParams = new Class[] {String.class,String.class};
            
            if (this == DERBY) {
            	dsclass.getMethod("setServerName", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { dbConnectionInfo.getServerName() });
            	dsclass.getMethod("setPortNumber", new Class[] { int.class })
                    .invoke(dataSourceInstance,
                    new Object[] { dbConnectionInfo.getPortNumber() });
            	dsclass.getMethod("setDatabaseName", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { dbConnectionInfo.getDatabaseOrSchemaName() });
            	dsclass.getMethod("setUser", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { dbConnectionInfo.getUsername() });
            	dsclass.getMethod("setPassword", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { dbConnectionInfo.getPassword() });
            } else if (this == ORACLE) {
            	dsclass.getMethod("setURL", stringParam).invoke(dataSourceInstance,
                        new Object[] { dbConnectionInfo.getDbUrl()});
            	dsclass.getMethod("setUser", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { dbConnectionInfo.getUsername() });
            	dsclass.getMethod("setPassword", stringParam)
                    .invoke(dataSourceInstance,
                    new Object[] { dbConnectionInfo.getPassword() });
            }
            return dataSourceInstance;
        } catch (final ClassNotFoundException cnfe) {
            cnfe.getMessage();
        } catch (final IllegalAccessException iae) {
        } catch (final NoSuchMethodException nsme) {
        } catch (final InvocationTargetException ite) {
        } catch (final InstantiationException ie) {
        }

        return null;
    }
*/
	
    public void setDBConnectionInfo(final DBConnectionInfo dbConnectionInfo) {
        this.mdbConnectionInfo = dbConnectionInfo;
    }

    public DataBases getValue(final String value) {
        if (value.equalsIgnoreCase("DERBY")) {
            return DERBY;
        } else if (value.equalsIgnoreCase("ORACLE")) {
            return ORACLE;
        } else if (value.equalsIgnoreCase("VSAM")) {
            return VSAM;
        } else if (value.equalsIgnoreCase("DB2")) {
            return DB2;
        } else if (value.equalsIgnoreCase("SQLSERVER")) {
            return SQLSERVER;
        } else if (value.equalsIgnoreCase("MYSQL")) {
            return MYSQL;
        } else {
            return null;
        }
    }

    public DatabaseModel getDatabaseModel() {
        switch (this) {
        case DERBY:
            return DerbyDataAccess.getInstance();
        case ORACLE:
            return OracleDataAccess.getInstance();
        case DB2:
            return Db2DataAccess.getInstance();
        case SQLSERVER:
            return SqlServerDataAccess.getInstance();
        }
        return DatabaseModelImpl.getInstance();
    }

   public String createAndBindJNDIDS(final ComponentContext componentContext,
							Object datasource, final String jndiname, final String xaEnabled)
	   throws NamingException {

		final InitialContext namingContext = componentContext.getNamingContext();
		
	    if(mdbConnectionInfo.getXAEnabled().equalsIgnoreCase("XATransaction")){
			   datasource =(XADataSource)datasource;			
		}else{
			   datasource =(DataSource)datasource;		
		}
		if(lookup(jndiname,namingContext)){
			namingContext.bind(jndiname, datasource);
		}
		return jndiname;
	 }

	public boolean lookup(final String jndiname,final InitialContext namingContext) throws NamingException{
		try{			
			namingContext.lookup(jndiname);	
		}catch(javax.naming.NameNotFoundException e){			
			return true;
		}	
		return false;
	}   
}
