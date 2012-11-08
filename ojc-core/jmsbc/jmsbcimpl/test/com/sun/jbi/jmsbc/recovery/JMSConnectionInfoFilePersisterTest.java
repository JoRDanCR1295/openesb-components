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
 * @(#)JMSConnectionInfoFilePersisterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.recovery;

import junit.framework.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Properties;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collections;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.jmsbc.LogSupport;
import com.sun.jbi.internationalization.Messages;

/**
 *
 * Unit test for JMSConnectionInfoFilePersister
 */
public class JMSConnectionInfoFilePersisterTest extends TestCase {
    private String testDir = null;
    private String testFile = null;
    private Properties persisterProps = null;
    
    public JMSConnectionInfoFilePersisterTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        testFile = "connections";
        persisterProps = new Properties();
        String dir = getClass().getResource("JMSConnectionInfoFilePersisterTest.class").getPath();
        testDir = dir.substring(0, dir.length() - "JMSConnectionInfoFilePersisterTest.class".length() - 1);
        persisterProps.setProperty(JMSConnectionInfoFilePersister.FILE_PERSISTER_PROPS_DIR,
        testDir);
        persisterProps.setProperty(JMSConnectionInfoFilePersister.FILE_PERSISTER_PROPS_FILENAME,
                                   testFile);
        
        new File(testDir,testFile).delete();
        
        new File(testDir,JMSConnectionInfoFilePersister.FILE_PERSISTER_DEFAULT_FILENAME).delete();
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of initialize method, of class com.sun.jbi.jmsbc.recovery.JMSConnectionInfoFilePersister.
     */
    public void testInitializeNew() throws Exception {
        System.out.println("initialize with new file");
        
        JMSConnectionInfoFilePersister instance = new JMSConnectionInfoFilePersister();
        
        instance.initialize(persisterProps);
        
        // TODO review the generated test code and remove the default call to fail.
        assertEquals(0, instance.retrieve().length);
    }

    /**
     * Test of initialize, persiste, and retrieve methods, of class com.sun.jbi.jmsbc.recovery.JMSConnectionInfoFilePersister.
     */
    public void testInitializePersistRetrieve() throws Exception {
        System.out.println("initialize, persist, and retrieve connection info records with different info");
        
        JMSConnectionInfoFilePersister instance = new JMSConnectionInfoFilePersister();
        
        // initialize with some records
        instance.initialize(persisterProps);        
        JMSConnectionInfoRecord recs [] = new JMSConnectionInfoRecord [6];
        recs[0] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", null);
        recs[1] = new JMSConnectionInfoRecord("mq://foohost:7878", "foo", "fighter", "");
        recs[2] = new JMSConnectionInfoRecord("mq://mqhost:7878", "admin", "admin", "");
        recs[3] = new JMSConnectionInfoRecord("mq://localhost:7676", "guest", "guest", "");
        Properties jndiEnvProps1 = new Properties();
        jndiEnvProps1.setProperty("jndiSys1-Ent1", "jndiSys1-Val1");
        jndiEnvProps1.setProperty("jndiSys1-Ent2", "jndiSys1-Val2");
        Properties jndiEnvProps2 = new Properties();
        jndiEnvProps2.setProperty("jndiSys2-Ent1", "jndiSys2-Val1");
        jndiEnvProps2.setProperty("jndiSys2-Ent2", "jndiSys2-Val2");
        recs[4] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyTCF",
                                                  "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                  "mySecCredentials", jndiEnvProps1);
        recs[5] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyQCF",
                                                  "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                  "mySecCredentials", jndiEnvProps2);
        instance.persist(recs);
        
        ConnectionInfoRecord [] recsRestored = instance.retrieve();
        ConnectionInfoRecord fooFighterRec = null;
        ConnectionInfoRecord jndiRecMyTCF = null;
        for (int i=0; i < recsRestored.length; i++) {
            if (((JMSConnectionInfoRecord)recsRestored[i]).getPassword().equals("fighter")) {
                fooFighterRec = recsRestored[i];
            }
            
            if (((JMSConnectionInfoRecord)recsRestored[i]).getConnectionURL().equals("jndi://") && 
                ((JMSJndiConnectionInfoRecord)recsRestored[i]).getConnectionFactoryName().equals("MyTCF")) {
                jndiRecMyTCF = recsRestored[i];
            }
        }
        
        assertTrue (recs[1].getUsername().equals(((JMSConnectionInfoRecord)fooFighterRec).getUsername()));
        assertTrue (((JMSJndiConnectionInfoRecord)recs[4]).getJndiEnv().getProperty("jndiSys1-Ent2").equals(((JMSJndiConnectionInfoRecord)jndiRecMyTCF).getJndiEnv().getProperty("jndiSys1-Ent2")));
    }    
    
    /**
     * Test of initialize, persiste, and retrieve methods, of class com.sun.jbi.jmsbc.recovery.JMSConnectionInfoFilePersister.
     */
    public void testInitializePersistRetrieveUniqueRecords() throws Exception {
        System.out.println("initialize, persist, and retrieve connection info records with same info");
        
        JMSConnectionInfoFilePersister instance = new JMSConnectionInfoFilePersister();
        
        // initialize with some records that have the same information
        instance.initialize(persisterProps);        
        JMSConnectionInfoRecord recs [] = new JMSConnectionInfoRecord [6];
        recs[0] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", "");
        recs[1] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", "");
        recs[2] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", "");
        recs[3] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", "");
        Properties jndiEnvProps1 = new Properties();
        jndiEnvProps1.setProperty("jndiSys1-Ent1", "jndiSys1-Val1");
        jndiEnvProps1.setProperty("jndiSys1-Ent2", "jndiSys1-Val2");
        Properties jndiEnvProps2 = new Properties();
        jndiEnvProps2.setProperty("jndiSys1-Ent1", "jndiSys1-Val1");
        jndiEnvProps2.setProperty("jndiSys1-Ent2", "jndiSys1-Val2");
        recs[4] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyQCF",
                                                  "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                  "mySecCredentials", jndiEnvProps1);
        recs[5] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyQCF",
                                                  "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                  "mySecCredentials", jndiEnvProps2);

        instance.persist(recs);
                
        ConnectionInfoRecord [] recsRestored = instance.retrieve();
        
        assertTrue (recsRestored.length==2);        
    }    

    /**
     * Test of initialize, persiste, and retrieve methods, of class com.sun.jbi.jmsbc.recovery.JMSConnectionInfoFilePersister.
     */
    public void testInitializePersistRetrieveMergeRecords() throws Exception {
        System.out.println("initialize, persist, and retrieve connection info records; multiple persists calls which merge records");
        
        JMSConnectionInfoFilePersister instance = new JMSConnectionInfoFilePersister();
        
        // initialize with some records
        instance.initialize(persisterProps);        
        JMSConnectionInfoRecord recs [] = new JMSConnectionInfoRecord [4];
        recs[0] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", "");
        recs[1] = new JMSConnectionInfoRecord("mq://foohost:7878", "foo", "fighter", "");
        recs[2] = new JMSConnectionInfoRecord("mq://mqhost:7878", "admin", "admin", "");
        recs[3] = new JMSConnectionInfoRecord("mq://localhost:7676", "guest", "guest", "");
        instance.persist(recs);
        
        JMSConnectionInfoRecord recs2 [] = new JMSConnectionInfoRecord [2];
        recs2[0] = new JMSConnectionInfoRecord("mq://localhost:8888", "8888", "8888", "");
        recs2[1] = new JMSConnectionInfoRecord("mq://localhost:9999", "9999", "9999", "");
        instance.persist(recs2);
        
        JMSJndiConnectionInfoRecord recs3 [] = new JMSJndiConnectionInfoRecord [2];
        Properties jndiEnvProps1 = new Properties();
        jndiEnvProps1.setProperty("jndiSys1-Ent1", "jndiSys1-Val1");
        jndiEnvProps1.setProperty("jndiSys1-Ent2", "jndiSys1-Val2");
        Properties jndiEnvProps2 = new Properties();
        jndiEnvProps2.setProperty("jndiSys2-Ent1", "jndiSys2-Val1");
        jndiEnvProps2.setProperty("jndiSys2-Ent2", "jndiSys2-Val2");
        recs3[0] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyTCF",
                                                   "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                   "mySecCredentials", jndiEnvProps1);
        recs3[1] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyQCF",
                                                   "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                   "mySecCredentials", jndiEnvProps2);
        instance.persist(recs3);
        
        ConnectionInfoRecord [] recsRestored = instance.retrieve();
        ConnectionInfoRecord aRec8888 = null;
        ConnectionInfoRecord jndiRecMyTCF = null;
        for (int i=0; i < recsRestored.length; i++) {
            if (((JMSConnectionInfoRecord)recsRestored[i]).getPassword().equals("8888")) {
                aRec8888 = recsRestored[i];
            }
            if (((JMSConnectionInfoRecord)recsRestored[i]).getConnectionURL().equals("jndi://") && 
                ((JMSJndiConnectionInfoRecord)recsRestored[i]).getConnectionFactoryName().equals("MyTCF")) {
                jndiRecMyTCF = recsRestored[i];
            }
        }

        assertTrue (recsRestored.length == 8);
        assertTrue (((JMSConnectionInfoRecord)aRec8888).getPassword().equals("8888"));        
        assertTrue (((JMSJndiConnectionInfoRecord)jndiRecMyTCF).getJndiEnv().getProperty("jndiSys1-Ent2").equals("jndiSys1-Val2"));
    }
    
    /**
     * Test of initialize, persist, and remove methods, of class com.sun.jbi.jmsbc.recovery.JMSConnectionInfoFilePersister.
     */
    public void testInitializePersistRemoveRecords() throws Exception {
        System.out.println("initialize, persist, and remove connection info records");
        
        JMSConnectionInfoFilePersister instance = new JMSConnectionInfoFilePersister();
        
        // initialize with some records
        instance.initialize(persisterProps);        
        JMSConnectionInfoRecord recs [] = new JMSConnectionInfoRecord [6];
        recs[0] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", "");
        recs[1] = new JMSConnectionInfoRecord("mq://foohost:7878", "foo", "fighter", "");
        recs[2] = new JMSConnectionInfoRecord("mq://mqhost:7878", "admin", "admin", "");
        recs[3] = new JMSConnectionInfoRecord("mq://localhost:7676", "guest", "guest", "");
        Properties jndiEnvProps1 = new Properties();
        jndiEnvProps1.setProperty("jndiSys1-Ent1", "jndiSys1-Val1");
        jndiEnvProps1.setProperty("jndiSys1-Ent2", "jndiSys1-Val2");
        Properties jndiEnvProps2 = new Properties();
        jndiEnvProps2.setProperty("jndiSys2-Ent1", "jndiSys2-Val1");
        jndiEnvProps2.setProperty("jndiSys2-Ent2", "jndiSys2-Val2");
        recs[4] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyTCF",
                                                  "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                  "mySecCredentials", jndiEnvProps1);
        recs[5] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyQCF",
                                                  "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                  "mySecCredentials", jndiEnvProps2);
        instance.persist(recs);
        
        JMSConnectionInfoRecord recToRemove1 [] = new JMSConnectionInfoRecord [1];
        recToRemove1[0] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", "");
        instance.remove(recToRemove1);  // remove one        
        JMSConnectionInfoRecord recToRemove3 [] = new JMSConnectionInfoRecord [3];
        recToRemove3[0] = new JMSConnectionInfoRecord("mq://foohost:7878", "foo", "fighter", "");
        recToRemove3[1] = new JMSConnectionInfoRecord("mq://mqhost:7878", "admin", "admin", "");
        recToRemove3[2] = new JMSConnectionInfoRecord("mq://localhost:7676", "guest", "guest", "");
        instance.remove(recs);  // remove the rest
        JMSConnectionInfoRecord recToRemove2 [] = new JMSConnectionInfoRecord [2];
        recToRemove2[0] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyTCF",
                                                  "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                  "mySecCredentials", jndiEnvProps1);
        recToRemove2[1] = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyQCF",
                                                  "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                  "mySecCredentials", jndiEnvProps2);
        instance.remove(recToRemove2);
        assertTrue (instance.retrieve().length == 0);
    }
    
    /**
     * Test of initialize, persiste, and retrieve methods, of class com.sun.jbi.jmsbc.recovery.JMSConnectionInfoFilePersister.
     */
    public void testInitializePersistRetrieveDefaultFile() throws Exception {
        System.out.println("initialize, persist, and retrieve connection info records using default file name in persister");
        
        JMSConnectionInfoFilePersister instance = new JMSConnectionInfoFilePersister();

        // Only set the directory
        persisterProps = new Properties();
        persisterProps.setProperty(JMSConnectionInfoFilePersister.FILE_PERSISTER_PROPS_DIR,
                                   testDir);
        
        // initialize with some records
        instance.initialize(persisterProps);        
        JMSConnectionInfoRecord recs [] = new JMSConnectionInfoRecord [1];
        recs[0] = new JMSConnectionInfoRecord("mq://localhost:7676", "admin", "admin", null);
        instance.persist(recs);
        
        ConnectionInfoRecord [] recsRestored = instance.retrieve();
        
        assertTrue (recsRestored.length==1 && 
                    ((JMSConnectionInfoRecord)recsRestored[0]).getUsername().equals("admin"));        
    }    
    
    /**
     * Test of initialize, persiste, and retrieve methods, of class com.sun.jbi.jmsbc.recovery.JMSConnectionInfoFilePersister.
     */
    public void testClose() throws Exception {
        try {
            System.out.println("other methods should fail if initialize is not called");

            JMSConnectionInfoFilePersister instance = new JMSConnectionInfoFilePersister();
            // initialize with some records
            instance.initialize(persisterProps);        
            
            // force close on instance
            instance.close();
            
            instance.retrieve();
        } catch (ConnectionInfoPersistException ex) {
            assertTrue(true);
        }
    }
}
