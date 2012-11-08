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
 * @(#)BlobTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;

import java.io.*;
import java.net.URL;
import java.rmi.server.UID;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.impl.RVariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
import com.sun.wsdl4j.ext.WSDL4JExt;

import javax.wsdl.Definition;
import javax.wsdl.Part;
import javax.wsdl.Message;

/**
 * @author Sun Microsystems
 *
 */
public class BlobTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(
            "com.sun.jbi.engine.bpel.core.test.bpelpersist.TestBlob");
    static String ORCL_VAR_INSERT_STMT_STR = "insert into test_variable values(?, ?)";
    static String ORCL_STATE_INSERT_STMT_STR = "insert into test_state values(?, ?, ?, ?)";
    static int NO_OF_VARS = 4;
    static int NO_OF_STATES = 4;
    Integer mVariableId = 0;
    public BlobTest(String testName) {
        super(testName);
    }
    
    public static Test suite() throws Exception {
        TestSuite suite = new TestSuite(BlobTest.class);

        //        TestSuite suite = new TestSuite();
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
        Connection con = getConnection();
        con.setAutoCommit(false);
        Statement create = con.createStatement();
        int dbType = -1;
        if (con.getClass().getPackage().toString().toLowerCase().contains("oracle")) {
            dbType = ConnectionProperties.ORCL_DB;
        } else if (con.getClass().getPackage().toString().toLowerCase().contains("derby")) {
            dbType = ConnectionProperties.DERBY_DB;
        } else if (con.getClass().getPackage().toString().toLowerCase().contains("mysql")) {
            dbType = ConnectionProperties.MYSQL_DB;
        } else if (con.getClass().getPackage().toString().toLowerCase().contains("postgresql")) {
            dbType = ConnectionProperties.POSTGRES_DB;
        }
        //create.execute("Create Schema BLOB_TEST");
        try {
            create.execute(
                "create table test_state (id varchar(128), bpelid varchar(128), " +
                "engineid varchar(128), status varchar(32) DEFAULT 'RUNNING')");
            if (dbType == ConnectionProperties.DERBY_DB) {
                create.execute("create table test_variable (id varchar(128), value blob (10 M))");
            } else if (dbType == ConnectionProperties.ORCL_DB) {
                create.execute("create table test_variable (id varchar(128), value blob)");
            } else if (dbType == ConnectionProperties.MYSQL_DB) {
                create.execute("create table test_variable (id varchar(128), value longblob)");
            } else if (dbType == ConnectionProperties.POSTGRES_DB) {
                create.execute("create table test_variable (id varchar(128), value bytea)");
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            tearDown();
        }
        con.commit();
        con.close();
    }

    protected void tearDown() throws Exception {
        super.tearDown();

        Connection con = getConnection();
        con.setAutoCommit(false);
        Statement drop = con.createStatement();
        try {
            drop.execute("DROP TABLE test_state");
            drop.execute("DROP TABLE test_variable");
        } catch (SQLException ex) {
            ex.printStackTrace();
        }
//        drop.execute("Drop Schema BLOB_TEST" + " RESTRICT"); // restrict for only Derby DB
        con.commit();
        con.close();
    }

    public void testBatchBlob() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testBatchBlob");
        Connection con = getConnection();
        con.setAutoCommit(false);
        
        PreparedStatement stmt1 = con.prepareStatement(ORCL_STATE_INSERT_STMT_STR);
        
        for (int i = 0; i < NO_OF_STATES; i++) {
            String str = new Integer(i).toString();
            stmt1.setString(1, new UID().toString());
            stmt1.setString(2, str);
            stmt1.setString(3, str);
            stmt1.setString(4, "active");
            stmt1.addBatch();
        }
        
        stmt1.executeBatch();
        con.commit();
        
        // look at the commented code method "commentedCode()", for the other ways to persist blob.
        PreparedStatement stmt = con.prepareStatement(ORCL_VAR_INSERT_STMT_STR);        
        for (int i = 0; i < NO_OF_VARS; i++) {
            RVariable var = constructVariable(i);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(bos);
            out.writeObject(var.getWSDLMessageType());
            out.flush();
            
            stmt.setString(1, mVariableId.toString());
            
            byte[] bytes = bos.toByteArray();
            stmt.setBytes(2, bytes);
            mVariableId++;
            stmt.addBatch();
        }
        
        stmt.executeBatch();
        
        con.commit();
        con.close();
        Utility.logExit(getClass().getSimpleName(), "testBatchBlob");
    }

    public void testHugeBlob() throws Exception {
        
        Utility.logEnter(getClass().getSimpleName(), "testHugeBlob");
        
        Connection con = getConnection();
        con.setAutoCommit(false);
        
        PreparedStatement stmt = con.prepareStatement(ORCL_VAR_INSERT_STMT_STR);

        ByteArrayOutputStream[] osArray = loadFiles();
        ByteArrayOutputStream bos = null;
        
        for (int i = 0; i < osArray.length; i++) {
            bos = osArray[i];
            stmt.setString(1, mVariableId.toString());
            stmt.setBytes(2, bos.toByteArray());
            mVariableId++;
            stmt.addBatch();
        }
        
        stmt.executeBatch();
        
        con.commit();
        con.close();
        Utility.logExit(getClass().getSimpleName(), "testHugeBlob");
    }

    private static RVariable constructVariable(int i) {
        RVariable var = new RVariableImpl(null, 1);
        var.setName(new Integer(i).toString());

        Message msg = constructMsg(i);

        //var.setWSMessage(msg);
        return var;
    }

    private static Message constructMsg(int i) {
        Definition wsdlDef = WSDL4JExt.newDefinition();
        Message msg = wsdlDef.createMessage();

        for (i = i * 10; i >= 0; i--) {
            Part part1 = wsdlDef.createPart();

            part1.setName("part1");
            part1.setTypeName(new QName("http://www.w3.org/2001/XMLSchema", "string"));

            Part part2 = wsdlDef.createPart();
            part2.setName("part2");
            part2.setTypeName(new QName("http://www.w3.org/2001/XMLSchema", "string"));
            msg.addPart(part1);
            msg.addPart(part2);
        }

        return msg;
    }

    private ByteArrayOutputStream[] loadFiles() throws Exception {
        String filePath = "data";
        URL url = getClass().getResource(filePath);

        File dir = new File(url.toURI());
        File[] listOfFiles = dir.listFiles();
        File ipFile = null;

        //List<ByteArrayOutputStream>
        List retVals = new ArrayList();

        for (int i = 0, j = 0; (i < listOfFiles.length) && (j < NO_OF_VARS);
                i++) {
            ipFile = listOfFiles[i];

            if (ipFile.length() < 10000) {
                continue;
            }

            FileInputStream is = new FileInputStream(ipFile);
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            int c;

            while ((c = is.read()) != -1) {
                os.write(c);
            }

            is.close();
            os.close();
            retVals.add(os);
            j++;
        }

        return (ByteArrayOutputStream[]) retVals.toArray(new ByteArrayOutputStream[] {
                
            });
    }

    private void commentedCode() {
        // using Oracle specific blob to insert the blob.

        /*
        oracle.sql.BLOB blob = null;
        PreparedStatement stmt = con.prepareStatement(ORCL_VAR_INSERT_STMT_STR);;
        for (int i = 0; i < NO_OF_VARS; i++) {
            blob = oracle.sql.BLOB.createTemporary(con, true, oracle.sql.BLOB.DURATION_SESSION);
            RVariable var = constructVariable(i);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(bos);
            DBObject varDBO1 = null;
            out.writeObject(var.getMessage());
            out.flush();

            stmt.setString(1, new Integer(i).toString());
            stmt.setLong(2, new Long(var.getName()).longValue());
            blob.putBytes(1, bos.toByteArray());
            //stmt.setBytes(3, bos.toByteArray());
            //stmt.setObject(3, bos.toByteArray());
            stmt.setBlob(3, blob);
            stmt.addBatch();
            //stmt.executeBatch();
        }
        oracle.sql.BLOB blob = oracle.sql.BLOB.createTemporary(con, true, oracle.sql.BLOB.DURATION_SESSION);
        PreparedStatement stmt = con.prepareStatement(ORCL_VAR_INSERT_STMT_STR);;
        for (int i = 0; i < NO_OF_VARS; i++) {
            RVariable var = constructVariable(i);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(bos);
            DBObject varDBO1 = null;
            out.writeObject(var.getMessage());
            out.flush();

            stmt.setString(1, new Integer(i).toString());
            stmt.setLong(2, new Long(var.getName()).longValue());
            byte[] bytes = bos.toByteArray();
            blob.trim(0);
            blob.putBytes(1, bytes);
            //stmt.setBytes(3, bos.toByteArray());
            //stmt.setObject(3, bos.toByteArray());
            stmt.setBlob(3, blob);
            stmt.addBatch();
            //stmt.executeBatch();
        }
        stmt.executeBatch();
         */
    }
}
