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
 * @(#)ChangeDBPropertyValues.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Connection;
import java.sql.Statement;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;


public class ChangeDBPropertyValues {

    private static String mDBType;
    private static String mDBURL;
    private static String mDBUser;
    private static String mDBPassWd;
    public static void main(String[] args) throws Exception {
        if (args.length < 2) {
            System.out.println("expects an argument to the file location of the DB properties");
            System.out.println("expects an argument pointing to the directory from where the replacement needs to start");
            System.out.println(" Usage Pattern: ChangeDBPropertyValues <DBPropsFile> <rootPathToStartReplacement> ");
            System.exit(0);
        }

        System.out.println(args[0]);
        try {
                File propsFile = new File(args[0]);
                Properties props = new Properties();
                props.load(propsFile.toURL().openStream());
                mDBType = props.getProperty(ConnectionProperties.DB_TYPE);
                mDBURL = props.getProperty(ConnectionProperties.DB_URL);
                mDBUser = props.getProperty(ConnectionProperties.DB_USERNAME);
                mDBPassWd = props.getProperty(ConnectionProperties.DB_PASSWORD);
                String rootLocation = args[1];
                URL url = null;
                if (Utility.isEmpty(rootLocation)) {
                    url = ChangeDBPropertyValues.class.getResource("/");
                    File root = new File(url.toURI());
                    System.out.println(root.isDirectory());
                    getPropertyFiles(root);
                } else {
                    String bpelseCoreRootPath = rootLocation + "\\bpelcore\\test";
                    File bpelseCoreRoot = new File(bpelseCoreRootPath);
                    getPropertyFiles(bpelseCoreRoot);
//                    String bpelseJBIRootPath = rootLocation + "\\bpeljbiadapter\\test";
//                    File bpelseJBIRoot = new File(bpelseJBIRootPath);
//                    getPropertyFiles(bpelseJBIRoot);
                    String bpelseSystemRootPath = rootLocation + "\\system-test\\test";
                    File bpelseSystemRoot = new File(bpelseSystemRootPath);
                    getPropertyFiles(bpelseSystemRoot);
                }
                DBSchemaCreation tablesCreator = PersistenceDBSchemaCreation.getInstance();
                DBSchemaCreation monitorTableCreator = MonitorDBSchemaCreation.getInstance();

                DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(props);
                DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
                    new DummyNonXATxManagerAndDataSource(props);
                BPELSERegistry registry = BPELSERegistry.getInstance();
                registry.register(TransactionManager.class.getName(), dummyTMAndDS);
                props.setProperty((ConnectionProperties.DatabaseNonXAJNDIName), "dummyJNDI");
                props.setProperty((ConnectionProperties.DatabaseXAJNDIName), "dummyJNDI");
                InitialContext ctx = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, props);

                DBConnectionFactory dbConnFac = new DBConnectionFactory(props, ctx, null);

                tablesCreator.dropTables(props, dbConnFac);
                monitorTableCreator.dropTables(props, dbConnFac);
                Connection conn = null;
                Statement stmt = null;
                if (mDBType.equals("Oracle")) {
                	try {
                		conn = dbConnFac.createNonXAConnection().getUnderlyingConnection();
                		stmt = conn.createStatement();
                		// The following is only for 10G.
                		stmt.execute("PURGE recyclebin");
                		conn.commit();
                	} finally {
                		if (conn != null) {
                			conn.close();
                		} 
                		if (stmt != null) {
                			stmt.close();
                		}
                	}
                }
        } catch (Exception ex) {
            ex.printStackTrace();
            throw ex;
        }
    }

    static FilenameFilter fileFilter = new MyFileNameFilter();
    private static void getPropertyFiles(File dir) {
        File[] files = dir.listFiles(fileFilter);
        for(File file:files) {
            if (file.isDirectory()) {
                getPropertyFiles(file);
            } else if (file.getName().endsWith(".properties")) {
                changePropFile(file);
            }
        }
    }

    private static void changePropFile(File propFile) {
        try {
            Properties props = new Properties();
            props.load(propFile.toURL().openStream());
            String perEnabled = props.getProperty(ConnectionProperties.PERSISTENCEENABLED);
            String monitorEnabled = props.getProperty(Engine.MONITOR_ENABLED);
            if (Utility.isEmpty(perEnabled) && Utility.isEmpty(monitorEnabled)) {
                return;
            }
            if (!Boolean.valueOf(perEnabled) && !Boolean.valueOf(monitorEnabled) ) {
                return;
            }
            if (mDBType.equals("Oracle")) {
                props.setProperty(ConnectionProperties.DB_TYPE, ConnectionProperties.ORCL_DB.toString());
                props.setProperty(ConnectionProperties.DB_URL, mDBURL);
                props.setProperty(ConnectionProperties.DB_USERNAME, mDBUser);
                props.setProperty(ConnectionProperties.DB_PASSWORD, mDBPassWd);
                props.store(new FileOutputStream(propFile), null);
            } else if (mDBType.equals("MySQL")) {
                props.setProperty(ConnectionProperties.DB_TYPE, ConnectionProperties.MYSQL_DB.toString());
                props.setProperty(ConnectionProperties.DB_URL, mDBURL);
                props.setProperty(ConnectionProperties.DB_USERNAME, mDBUser);
                props.setProperty(ConnectionProperties.DB_PASSWORD, mDBPassWd);
                props.store(new FileOutputStream(propFile), null);
            }
        } catch (MalformedURLException ex) {
            // TODO Auto-generated catch block
            ex.printStackTrace();
        } catch (IOException ex) {
            // TODO Auto-generated catch block
            ex.printStackTrace();
        }
    }

    static class MyFileNameFilter implements FilenameFilter {

        /** @see java.io.FilenameFilter#accept(java.io.File, java.lang.String)
         */
        public boolean accept(File file, String fileName) {
            File childFile = new File(file, fileName);
            if ((childFile.isDirectory() && file.list().length > 0) || fileName.endsWith(".properties")) {
                return true;
            }
            return false;
        }
    }
}
