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
 */

package com.sun.jbi.mqbc.mbeans;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.InvalidKeyException;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularType;

import junit.framework.TestCase;

public class RuntimeConfigurationTest extends TestCase {
    private RuntimeConfiguration subject;
    private File testDir;
    private File persistenceConfigFile;

    public RuntimeConfigurationTest() {
        super();
    }
    
    public void testSetOutboundThreadsPersistence() throws Exception {
        // Persistence integrity
        subject.setOutboundThreads(new Integer(10000));
        Properties props = new Properties();
        FileInputStream fis = new FileInputStream(persistenceConfigFile);
        props.load(fis);
        fis.close();
        assertTrue(props.getProperty("OutboundThreads") != null);
        assertTrue(props.getProperty("OutboundThreads").equals("10000"));
    }
    
    public void testSetOutboundThreads() throws Exception {
        // Null test
        try {
            subject.setOutboundThreads(null);
        } catch (InvalidAttributeValueException e) {
            // expected for null argument
            assert(true);
        }
        
        // Constraints
        try {
            subject.setOutboundThreads(new Integer(0));
            fail("Setting outbound threads to 0 allowed");
        } catch (InvalidAttributeValueException e) {
        }
        try {
            subject.setOutboundThreads(new Integer(65536));
            fail("Maximum constraint (" + 65535 + ") not enforced");
        } catch (InvalidAttributeValueException e) {
        }
    }
    
    public void testGetOutboundThreads() throws Exception {
        // Defaults
        int defaultThreadCount = subject.getOutboundThreads().intValue();
        assertEquals("Expected default thread count of 10, got "
                + defaultThreadCount, 10, defaultThreadCount);
        
        // Read/Write
        int write = 65534;
        int read;
        subject.setOutboundThreads(write);
        read = subject.getOutboundThreads();
        assertEquals("Wrote " + write + ", read: " + read, read, write);
    }
    
    public void testCountVariables() throws Exception {
        // Defaults
        assertEquals("Initial mbean should have no variables!", 0, subject.countVariables());
    }
    
    public void testAddApplicationVariablePersistence() throws Exception {
        CompositeData data = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier","value","STRING"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        subject.addApplicationVariable("identifier", data);
        File file = new File(testDir, RuntimeConfiguration.PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        Properties props = new Properties();
        FileInputStream fis = new FileInputStream(file);
        props.load(fis);
        fis.close();
        assertTrue(props.getProperty("identifier") != null);
        assertTrue(props.getProperty("identifier").equals("value{STRING}"));
    }
    
    public void testAddApplicationVariable() throws Exception {
        // Acceptable data
        CompositeData data = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier","value","STRING"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite width too wide
        CompositeData data4 = new CompositeDataImpl(
                new String[] {"name","value","type","foo"},
                new String[] {"identifier","value","STRING","bar"},
                "MyCompositeData4",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite has unsupported keys -- missing "name" key
        CompositeData dataWTF_name = new CompositeDataImpl(
                new String[] {"nickname","value","type"},
                new String[] {"identifier","value","STRING"},
                "MyCompositeDataWTF_name",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite has missing value
        CompositeData dataWTF_value = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier",null,"STRING"},
                "MyCompositeDataWTF_Value",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite has missing type
        CompositeData dataWTF_type = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier","value",null},
                "MyCompositeDataWTF_Type",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite type size/format
        try {
            subject.addApplicationVariable("identifier", data4);
            fail("Incompatible Composite (width 4) accepted");
        } catch (MBeanException e) {
        }
        
        // Missing name key
        try {
            subject.addApplicationVariable("identifier", dataWTF_name);
            fail("Incompatible Composite (name key missing) accepted");
        } catch (MBeanException e) {
        }

        // Null value
        try {
            subject.addApplicationVariable("identifier", dataWTF_value);
            fail("Incompatible Composite (value key missing) accepted");
        } catch (MBeanException e) {
        }

        // Null type
        try {
            subject.addApplicationVariable("identifier", dataWTF_type);
            fail("Incompatible Composite (type key missing) accepted");
        } catch (MBeanException e) {
        }
        
        // Read/Write
        subject.addApplicationVariable("identifier", data);
        Map<String, String[]> vars = subject.retrieveApplicationVariablesMap();
        String[] values = vars.get("identifier");
        int size = values.length;
        String val1 = values[0];
        String val2 = values[1];
        assertTrue("Expected size 2, got " + size, size == 2);
        assertTrue("Value{0} expected to be 'value', got '" + val1 + "'", "value".equals(values[0]));
        assertTrue("Value{1} expected to be 'STRING', got '" + val2 + "'", "STRING".equals(values[1]));
        
        // Uniqueness
        try {
            subject.addApplicationVariable("identifier", data);
            fail("Duplicate variable accepted");
        } catch (MBeanException e) {
            ;
        }
        
        // Injecting a variable whose call argument name
        // does not match its "name" field value
        try {
            subject.addApplicationVariable("foo", data);
            fail("Accepted variable 'foo' whose internal name is '" + data.get("name") + "'");
        } catch (MBeanException e) {
        }
    }
    
    public void testSetApplicationVariable() throws Exception {
        // Acceptable data
        CompositeData data = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier","value","STRING"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Acceptable data
        CompositeData data1 = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier","value1","STRING1"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Acceptable data
        CompositeData data2 = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier2","value","STRING"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite width too wide
        CompositeData data4 = new CompositeDataImpl(
                new String[] {"name","value","type","foo"},
                new String[] {"identifier","value","STRING","bar"},
                "MyCompositeData4",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite has unsupported keys -- missing "name" key
        CompositeData dataWTF_name = new CompositeDataImpl(
                new String[] {"nickname","value","type"},
                new String[] {"identifier","value","STRING"},
                "MyCompositeDataWTF_name",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite has missing value
        CompositeData dataWTF_value = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier",null,"STRING"},
                "MyCompositeDataWTF_Value",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Composite has missing type
        CompositeData dataWTF_type = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier","value",null},
                "MyCompositeDataWTF_Type",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Non-existent variable
        try {
            subject.setApplicationVariable("identifier", data);
            fail("Accepted a variable that is not yet defined");
        } catch (MBeanException e) {
        }
        
        // Existing variable - should work
        subject.addApplicationVariable("identifier", data);
        subject.setApplicationVariable("identifier", data);        

        // Composite type size/format
        try {
            subject.setApplicationVariable("identifier", data4);
            fail("Incompatible Composite (width 4) accepted");
        } catch (MBeanException e) {
        }
        
        // Missing name key
        try {
            subject.setApplicationVariable("identifier", dataWTF_name);
            fail("Incompatible Composite (name key missing) accepted");
        } catch (MBeanException e) {
        }
        
        // Null value
        try {
            subject.setApplicationVariable("identifier", dataWTF_value);
            fail("Incompatible Composite (value key missing) accepted");
        } catch (MBeanException e) {
        }

        // Null type
        try {
            subject.setApplicationVariable("identifier", dataWTF_type);
            fail("Incompatible Composite (type key missing) accepted");
        } catch (MBeanException e) {
        }
        
        // Injecting a variable whose call argument name
        // does not match its "name" field value
        try {
            subject.setApplicationVariable("identifier", data2);
            fail("Accepted updating existing variable 'identifier' whose internal name is '" + data.get("name") + "'");
        } catch (MBeanException e) {
        }
        
        // Read/Write
        subject.setApplicationVariable("identifier", data1);
        Map<String, String[]> vars = subject.retrieveApplicationVariablesMap();
        String[] values = vars.get("identifier");
        int size = values.length;
        String val1 = values[0];
        String val2 = values[1];
        assertTrue("Expected size 2, got " + size, size == 2);
        assertTrue("Value{0} expected to be 'value1', got '" + val1 + "'", "value1".equals(values[0]));
        assertTrue("Value{1} expected to be 'STRING1', got '" + val2 + "'", "STRING1".equals(values[1]));
    }
    
    public void testSetApplicationVariablePersistence() throws Exception {
        CompositeData data1 = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier1","value1","STRING"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        CompositeData data2 = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier1","value2","INTEGER"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        subject.addApplicationVariable("identifier1", data1);
        subject.setApplicationVariable("identifier1", data2);
        File file = new File(testDir, RuntimeConfiguration.PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        Properties props = new Properties();
        FileInputStream fis = new FileInputStream(file);
        props.load(fis);
        fis.close();
        String value = props.getProperty("identifier1");
        assertTrue("value is null", value != null);
        assertTrue("unexpected value: " + value, value.equals("value2{INTEGER}"));
    }
    
    public void testDeleteApplicationVariable() throws Exception {
        CompositeData data = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier","value","STRING"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        
        // Delete non-existent variable
        try {
            subject.deleteApplicationVariable("foo");
            fail("Non-existent deletion allowed");
        } catch (MBeanException e) {
        }
        
        // Write/read
        subject.addApplicationVariable("identifier", data);
        Map<String, String[]> map = subject.retrieveApplicationVariablesMap();
        assertTrue(map.containsKey("identifier"));
        subject.deleteApplicationVariable("identifier");
        assertTrue(!map.containsKey("identifier"));
    }
    
    public void testDeleteApplicationVariablePersistence() throws Exception {
        CompositeData data = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {"identifier","value","STRING"},
                "MyCompositeData",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});

        subject.addApplicationVariable("identifier", data);
        Map<String, String[]> map = subject.retrieveApplicationVariablesMap();
        assertTrue(map.containsKey("identifier"));
        File file = new File(testDir, RuntimeConfiguration.PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        Properties props = new Properties();
        FileInputStream fis = new FileInputStream(file);
        props.load(fis);
        fis.close();
        String value = props.getProperty("identifier");
        assertTrue("Variable 'identifier' not persisted", value != null);
        assertTrue("Variable 'identifier' unexpected value: " + value
                + "; expecting 'value{STRING}'", value.equals("value{STRING}"));

        subject.deleteApplicationVariable("identifier");
        assertTrue(!map.containsKey("identifier"));
        file = new File(testDir, RuntimeConfiguration.PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        props = new Properties();
        fis = new FileInputStream(file);
        props.load(fis);
        fis.close();
        value = props.getProperty("identifier");
        assertTrue("Persistence does not reflect deleted variable 'identifier'", value == null);
    }
    
    public void testGetApplicationVariables() throws Exception {
        // Populate some data first
        String[] keys = new String[] {"name1", "name2", "name3",};
        CompositeData data1 = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {keys[0],"value1","STRING"},
                "MyCompositeData1",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        CompositeData data2 = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {keys[1],"value1","STRING"},
                "MyCompositeData2",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        CompositeData data3 = new CompositeDataImpl(
                new String[] {"name","value","type"},
                new String[] {keys[2],"value1","STRING"},
                "MyCompositeData3",
                new SimpleType[] {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING});
        subject.addApplicationVariable(keys[0], data1);
        subject.addApplicationVariable(keys[1], data2);
        subject.addApplicationVariable(keys[2], data3);
        
        // Retrieve tabular data and verify consistency
        TabularData tabularData = subject.getApplicationVariables();
        TabularType tabularType = tabularData.getTabularType();
        CompositeType tabularRowType = tabularType.getRowType();
        int size = tabularData.size();
        assertEquals("Expected row count " + keys.length + ", got " + size, size, keys.length);
        assertTrue("Foreign TabularType", tabularType.equals(RuntimeConfiguration.APPVAR_TABULAR_TYPE));
        assertTrue("Foreign CompositeType", tabularRowType.equals(RuntimeConfiguration.APPVAR_ROW_TYPE));
        for (Object keyset: tabularData.keySet()) {
            List<String> keylist = (List<String>) keyset;
            for (Object key: keylist) {
                assertTrue("Non-string key found: " + key.toString(), key instanceof String);
            }
        }
        for (String key: keys) {
            assertTrue("Tabular data missing key " + key, tabularData.containsKey(new Object[] {key}));
        }
    }
    
    public void testQueryApplicationConfigurationType() throws Exception {
        assertEquals("Foreign Application Configuration CompositeType",
                subject.queryApplicationConfigurationType(),
                RuntimeConfiguration.APPCONFIG_ROW_TYPE);
    }
    
    public void testAddApplicationConfiguration() throws Exception {
        String name;
        String correctName;
        CompositeData data1;
        CompositeData data2;
        
        // Bad name (should be OK; validation will auto-fix)
        name = " Co#nf][ig@urat`ion%~%%)(+_0   $$$$<>.,!@#$%^&*()_+$$1'  |}{][   ";
        correctName = "Configuration01";
        data1 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                name,
                "localhost",
                9009,
                "foo1",
                "jax1",
                "starie1",
                "cipher1",
                "peer1"
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING
        });
        subject.addApplicationConfiguration(name, data1);
        TabularData tabularData = subject.getApplicationConfigurations();
        CompositeData rowData = tabularData.get(new Object[] {correctName});
        Object queueManagerHostValue = rowData.get("queuemanagerHost");
        Object queueManagerPortValue = rowData.get("queuemanagerPort");
        Object username = rowData.get("username");
        Object password = rowData.get("password");
        Object cipherSuite = rowData.get("cipherSuite");
        Object sslPeerName = rowData.get("sslPeerName");
        Object queueManagerNameValue = rowData.get("queuemanagerName");
        assertTrue(queueManagerHostValue + " not 'localhost'", queueManagerHostValue.equals("localhost"));
        assertTrue(queueManagerPortValue + " not 9009", queueManagerPortValue.equals(9009));
        assertTrue(username + " not jax1", username.equals("jax1"));
        assertTrue(password + " not starie1", password.equals("starie1"));
        assertTrue(cipherSuite + " not cipher", cipherSuite.equals("cipher1"));
        assertTrue(sslPeerName + " not peer1", sslPeerName.equals("peer1"));
        assertTrue(queueManagerNameValue + " not foo1", queueManagerNameValue.equals("foo1"));
        
        // Configuration name collision
        try {
            subject.addApplicationConfiguration("Configuration01", data1);
            fail("Accepted application configuration with existing name");
        } catch (MBeanException e) {
        }
        
        // App Config object with a foreign field
        name = "Configuration02";
        data1 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "firstName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                name,
                "localhost9009",
                9009,
                "foo1",
                "jax1",
                "starie1",
                "bend",
                "arch",
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        try {
            subject.addApplicationConfiguration(name, data1);
            fail("Accepted application configuration with foreign field 'firstName'");
        } catch (MBeanException e) {
        }

        // Port field with out-of-range values
        // -1 and 0 are acceptable, and mean the same things
        name = "Configuration02";
        data1 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                name,
                "localhost",
                -3485,
                "foo1",
                "jax",
                "starie",
                "mangos",
                "clams",
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        data2 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                name,
                "localhost",
                65536,
                "foo1",
                "jax",
                "starie",
                "panther",
                "rope"
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        try {
            subject.addApplicationConfiguration(name, data1);
            fail("Accepted application configuration with an invalid port value (-3485)");
        } catch (MBeanException e) {
        }
        try {
            subject.addApplicationConfiguration(name, data2);
            fail("Accepted application configuration with an invalid port value (65536)");
        } catch (MBeanException e) {
        }

        // Port field unsupported value type
        // bad port value *type*
        data1 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                "Configuration02",
                "localhost",
                "foo",
                "foo1",
                "jax1",
                "starie1",
                "hellfire",
                "abyss",
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        try {
            subject.addApplicationConfiguration("Configuration02", data1);
            fail("Accepted application configuration with invalid value type 'foo' for queueManagerPort");
        } catch (MBeanException e) {
            assertTrue(e.getCause().getLocalizedMessage(), e.getCause().getLocalizedMessage().contains("MQBC-0819"));
        }

        // Queue manager null name (should be OK)
        data1 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                "Configuration02",
                null,
                555,
                "foo2",
                "jax2",
                "starie2",
                "teldrassil",
                "sea",
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        subject.addApplicationConfiguration("Configuration02", data1);
        
        // Queue manager bad name but fixable
        data1 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                "Configuration03",
                "localhostess",
                5555,
                "foo3",
                "jax3",
                "starie3",
                "tanaris",
                "hammock",
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        subject.addApplicationConfiguration("Configuration03", data1);
        
        // Write/read
        tabularData = subject.getApplicationConfigurations();
        CompositeData[] rowDataArr = new CompositeData[] {
                tabularData.get(new Object[] {"Configuration01"}),            
                tabularData.get(new Object[] {"Configuration02"}),            
                tabularData.get(new Object[] {"Configuration03"}),            
        };
        String[] qMgrHostValues = new String[] {
                "localhost",
                "",
                "localhostess"
        };
        Integer[] qMgrPortValues = new Integer[] {
                9009,
                555,
                5555,
        };
        String[] qMgrHostNames = new String[] {
                "foo1",
                "foo2",
                "foo3",
        };
        String[] usernames = new String[] {
                "jax1",
                "jax2",
                "jax3",
        };
        String[] passwords = new String[] {
                "starie1",
                "starie2",
                "starie3",
        };
        String[] cipherSuites = new String[] {
                "cipher1",
                "teldrassil",
                "tanaris",
        };
        String[] sslPeerNames = new String[] {
                "peer1",
                "sea",
                "hammock",
        };
        for (int i = 0; i < rowDataArr.length; ++i) {
            Object qmgrHost = rowDataArr[i].get("queuemanagerHost");
            Object qmgrPort = rowDataArr[i].get("queuemanagerPort");
            Object qmgrName = rowDataArr[i].get("queuemanagerName");
            Object user = rowDataArr[i].get("username");
            Object pass = rowDataArr[i].get("password");
            Object cipher = rowDataArr[i].get("cipherSuite");
            Object peer = rowDataArr[i].get("sslPeerName");
            assertTrue("queuemanagerHost mismatch, expected "
                    + qMgrHostValues[i] + " got " + qmgrHost,
                    qMgrHostValues[i].equals(qmgrHost));
            assertTrue("queuemanagerPort mismatch, expected "
                    + qMgrPortValues[i].toString() + " got " + qmgrPort.toString(),
                    qMgrPortValues[i].equals(qmgrPort));
            assertTrue("queuemanagerName mismatch, expected "
                    + qMgrHostNames[i] + " got " + qmgrName,
                    qMgrHostNames[i].equals(qmgrName));
            assertTrue("username mismatch, expected "
                    + usernames[i] + " got " + user,
                    usernames[i].equals(user));
            assertTrue("password mismatch, expected "
                    + passwords[i] + " got " + pass,
                    passwords[i].equals(pass));
            assertTrue("cipherSuite mismatch, expected "
                    + cipherSuites[i] + " got " + cipher,
                    cipherSuites[i].equals(cipher));
            assertTrue("sslPeerName mismatch, expected "
                    + sslPeerNames[i] + " got " + peer,
                    sslPeerNames[i].equals(peer));
        }
    }
    
    public void testAddApplicationConfigurationPersistence() throws Exception {
        CompositeData data1 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                "Configuration01",
                "localhost",
                -1,
                "foo",
                "jax",
                "starie",
                "cipher1",
                "peer1"
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        CompositeData data2 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                "Configuration02",
                null,
                0,
                "bar",
                "seafood",
                "clams",
                "cipher2",
                "peer2",
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        CompositeData data3 = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                "Configuration03",
                "localhostess",
                65535,
                "baz",
                "rocket",
                "pocket",
                "cipher3",
                "peer3",
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        subject.addApplicationConfiguration("Configuration01", data1);
        subject.addApplicationConfiguration("Configuration02", data2);
        subject.addApplicationConfiguration("Configuration03", data3);
        
        
        // pattern to match '[' id ']'
        Pattern stanzaStartPattern = Pattern.compile("^\\s*\\[\\s*(\\w+)\\s*\\]\\s*$");
        // pattern to match id '=' value
        Pattern stanzaTailPattern = Pattern.compile("^\\s*(\\w+)\\s*=\\s*(-?\\w*)\\s*$");
        // Read persistence file and verify formatting
        File file = new File(testDir, RuntimeConfiguration.PERSIST_APPLICATION_CONFIG_FILE_NAME);
        assertTrue("Application configuration persistence file " + file.getAbsolutePath() + " does not exist",
                file.exists());
        FileReader feader = new FileReader(file);
        BufferedReader reader = new BufferedReader(feader);
        List<String> configurationNames = new LinkedList<String>();
        List<String> queueManagerHosts = new LinkedList<String>();
        List<String> queueManagerNames = new LinkedList<String>();
        List<String> usernames = new LinkedList<String>();
        List<String> passwords = new LinkedList<String>();
        List<String> cipherSuites = new LinkedList<String>();
        List<String> sslPeerNames = new LinkedList<String>();
        List<Integer> queuePorts = new LinkedList<Integer>();
        String line;
        String lastName = null;
        boolean gotQueueManager = false;
        boolean gotQueuePort = false;
        boolean gotQueueManagerName = false;
        boolean gotUsername = false;
        boolean gotPassword = false;
        boolean gotCipherSuite = false;
        boolean gotSslPeerName = false;
        while ((line = reader.readLine()) != null) {
            Matcher headMatcher = stanzaStartPattern.matcher(line);
            Matcher tailMatcher = stanzaTailPattern.matcher(line);

            if (headMatcher.matches()) {
                lastName = headMatcher.group(1);
                configurationNames.add(lastName);
                gotQueueManager = false;
                gotQueuePort = false;
                gotPassword = false;
                gotUsername = false;
                gotQueueManagerName = false;
                gotCipherSuite = false;
                gotSslPeerName = false;
            } else if (tailMatcher.matches()) {
                String name = tailMatcher.group(1);
                String value = tailMatcher.group(2);
                if ("configurationName".equals(name)) {
                    assertTrue("configurationName value '" + value
                            + "' encountered under wrong block, '" + lastName + "'",
                            value.equals(lastName));
                } else if ("queuemanagerHost".equals(name)) {
                    assertTrue("Found more than 1 queuemanagerHost param for application config object "
                            + lastName + "; value: " + value,
                            !gotQueueManager);
                    queueManagerHosts.add(value);
                    gotQueueManager = true;
                } else if ("queuemanagerName".equals(name)) {
                    assertTrue("Found more than 1 queuemanagerName param for application config object "
                            + lastName + "; value: " + value,
                            !gotQueueManagerName);
                    queueManagerNames.add(value);
                    gotQueueManagerName = true;
                } else if ("password".equals(name)) {
                    assertTrue("Found more than 1 password param for application config object "
                            + lastName + "; value: " + value,
                            !gotPassword);
                    passwords.add(value);
                    gotPassword = true;
                } else if ("username".equals(name)) {
                    assertTrue("Found more than 1 username param for application config object "
                            + lastName + "; value: " + value,
                            !gotUsername);
                    usernames.add(value);
                    gotUsername = true;
                } else if ("cipherSuite".equals(name)) {
                    assertTrue("Found more than 1 cipherSuite param for application config object "
                            + lastName + "; value: " + value,
                            !gotCipherSuite);
                    cipherSuites.add(value);
                    gotCipherSuite = true;
                } else if ("sslPeerName".equals(name)) {
                    assertTrue("Found more than 1 sslPeerName param for application config object "
                            + lastName + "; value: " + value,
                            !gotSslPeerName);
                    sslPeerNames.add(value);
                    gotSslPeerName = true;
                } else if ("queuemanagerPort".equals(name)) {
                    assertTrue("Found more than 1 queueManagerPort param for application config object "
                            + lastName + "; value: " + value,
                            !gotQueuePort);
                    queuePorts.add(Integer.valueOf(value));
                    gotQueuePort = true;
                } else {
                    fail("Unrecognized application configuration parameter: " + name + "=" + value);
                }
            } else {
                // Unrecognized line, ignore it.
                // Not a failure because RuntimeConfiguration class designed to
                // ignore unrecognized objects.
                ;
            }
        }
        reader.close();
        for (int configNameIdx = 0; configNameIdx < configurationNames.size(); ++configNameIdx) {
            String name = configurationNames.get(configNameIdx);
            String queueManagerHost = queueManagerHosts.get(configNameIdx);
            String queueManagerName = queueManagerNames.get(configNameIdx);
            String username = usernames.get(configNameIdx);
            String password = passwords.get(configNameIdx);
            String cipherSuite = cipherSuites.get(configNameIdx);
            String sslPeerName = sslPeerNames.get(configNameIdx);
            Integer queuePort = queuePorts.get(configNameIdx);
            if ("Configuration01".equals(name)) {
                assertTrue("Expected localhost, got " + queueManagerHost, "localhost".equals(queueManagerHost));
                assertTrue("Expected -1, got " + queuePort.toString(), queuePort == -1);
                assertTrue("Expected foo, got " + queueManagerName, "foo".equals(queueManagerName));
                assertTrue("Expected jax, got " + username, "jax".equals(username));
                assertTrue("Expected starie, got " + password, "starie".equals(password));
                assertTrue("Expected cipher1, got " + cipherSuite, "cipher1".equals(cipherSuite));
                assertTrue("Expected peer1, got " + sslPeerName, "peer1".equals(sslPeerName));
            } else if ("Configuration02".equals(name)) {
                assertTrue("Expected '', got " + queueManagerHost, "".equals(queueManagerHost));
                assertTrue("Expected 0, got " + queuePort.toString(), queuePort == 0);
                assertTrue("Expected bar, got " + queueManagerName, "bar".equals(queueManagerName));
                assertTrue("Expected seafood, got " + username, "seafood".equals(username));
                assertTrue("Expected clams, got " + password, "clams".equals(password));
                assertTrue("Expected cipher2, got " + cipherSuite, "cipher2".equals(cipherSuite));
                assertTrue("Expected peer2, got " + sslPeerName, "peer2".equals(sslPeerName));
            } else if ("Configuration03".equals(name)) {
                assertTrue("Expected localhostess, got " + queueManagerHost, "localhostess".equals(queueManagerHost));
                assertTrue("Expected 65535, got " + queuePort.toString(), queuePort == 65535);
                assertTrue("Expected baz, got " + queueManagerName, "baz".equals(queueManagerName));
                assertTrue("Expected rocket, got " + username, "rocket".equals(username));
                assertTrue("Expected pocket, got " + password, "pocket".equals(password));
                assertTrue("Expected cipher3, got " + cipherSuite, "cipher3".equals(cipherSuite));
                assertTrue("Expected peer3, got " + sslPeerName, "peer3".equals(sslPeerName));
            } else {
                fail("Foreign application config object read from file: " + name);
            }
        }
    }
    
    public void testDeleteApplicationConfiguration() throws Exception {
        String name;
        String correctName;
        CompositeData data;
        TabularData tabularData;
        CompositeData rowData;
        
        // Bad name (should be OK; validation will auto-fix)
        name = " Co#nf][ig@urat`ion%~%%)(+_0   $$$$<>.,!@#$%^&*()_+$$1'  |}{][   ";
        correctName = "Configuration01";
        data = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                name,
                "localhost",
                9009,
                "foo",
                "starie",
                "jax",
                "cipher0",
                "peer0"
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        subject.addApplicationConfiguration(name, data);        
        tabularData = subject.getApplicationConfigurations();
        rowData = tabularData.get(new Object[] {correctName});
        Object queueManagerHostValue = rowData.get("queuemanagerHost");
        Object queueManagerPortValue = rowData.get("queuemanagerPort");
        Object queueManagerName = rowData.get("queuemanagerName");
        Object username = rowData.get("username");
        Object password = rowData.get("password");
        Object cipherSuite = rowData.get("cipherSuite");
        Object sslPeerName = rowData.get("sslPeerName");
        assertTrue(queueManagerHostValue + " not 'localhost'", queueManagerHostValue.equals("localhost"));
        assertTrue(queueManagerPortValue + " not 9009", queueManagerPortValue.equals(9009));
        assertTrue(queueManagerName + " not 'foo'", queueManagerName.equals("foo"));
        assertTrue(username + " not 'starie'", username.equals("starie"));
        assertTrue(password + " not 'jax'", password.equals("jax"));
        assertTrue(cipherSuite + " not 'cipher0'", cipherSuite.equals("cipher0"));
        assertTrue(sslPeerName + " not 'peer0'", sslPeerName.equals("peer0"));
        subject.deleteApplicationConfiguration(name);
        tabularData = subject.getApplicationConfigurations();
        assertTrue("Failed deletion of application configuration " + correctName,
                tabularData.isEmpty());
        
        // Do it again, add as a bad name, attempt to delete with the good name.
        subject.addApplicationConfiguration(name, data);        
        tabularData = subject.getApplicationConfigurations();
        assertTrue("Failed addition of application configuration " + name,
                !tabularData.isEmpty());
        subject.deleteApplicationConfiguration(correctName);
        tabularData = subject.getApplicationConfigurations();
        assertTrue("Failed deletion of application configuration " + correctName,
                tabularData.isEmpty());

        // Delete non-existent configuration object
        try {
            subject.deleteApplicationConfiguration(null);
            fail("Deletion of nonexistent application configuration accepted");
        } catch (MBeanException e) {
            assertTrue("Expected MQBC-0820, instead got: " + e.getCause()
                    .getLocalizedMessage(),
                    e.getCause().getLocalizedMessage().contains("MQBC-0820"));
        }
    }
    
    public void testDeleteApplicationConfigurationPersistence() throws Exception {
        String name;
        String correctName;
        CompositeData data;
        TabularData tabularData;
        CompositeData rowData;
        
        // Add with a bad name (should be OK; validation will auto-fix)
        name = " Co#nf][ig@urat`ion%~%%)(+_0   $$$$<>.,!@#$%^&*()_+$$1'  |}{][   ";
        correctName = "Configuration01";
        data = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                name,
                "localhost",
                9009,
                "bar",
                "jen",
                "muffin",
                "eats",
                "drinks"
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        subject.addApplicationConfiguration(name, data);
        
        // Add another one
        name = "Configuration02";
        data = new CompositeDataImpl(new String[]{
                "configurationName",
                "queuemanagerHost",
                "queuemanagerPort",
                "queuemanagerName",
                "username",
                "password",
                "cipherSuite",
                "sslPeerName",
        }, new Object[]{
                name,
                "localhostess",
                9109,
                "baz",
                "muffin",
                "jen",
                "eats1",
                "drinks1"
        }, "AppConfigObjectCompositeData", new SimpleType[]{
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.INTEGER,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
                SimpleType.STRING,
        });
        subject.addApplicationConfiguration(name, data);
        
        // Delete the first configuration
        name = " Co#nf][ig@urat`ion%~%%)(+_0   $$$$<>.,!@#$%^&*()_+$$1'  |}{][   ";
        subject.deleteApplicationConfiguration(name);
        
        // Read file, verify deletion
        // pattern to match '[' id ']'
        Pattern stanzaStartPattern = Pattern.compile("^\\s*\\[\\s*(\\w+)\\s*\\]\\s*$");
        // pattern to match id '=' value
        Pattern stanzaTailPattern = Pattern.compile("^\\s*(\\w+)\\s*=\\s*(-?\\w*)\\s*$");
        // Read persistence file and verify formatting
        File file = new File(testDir, RuntimeConfiguration.PERSIST_APPLICATION_CONFIG_FILE_NAME);
        assertTrue("Application configuration persistence file " + file.getAbsolutePath() + " does not exist",
                file.exists());
        FileReader feader = new FileReader(file);
        BufferedReader reader = new BufferedReader(feader);
        List<String> configurationNames = new LinkedList<String>();
        List<String> queueManagerHosts = new LinkedList<String>();
        List<String> queueManagerNames = new LinkedList<String>();
        List<String> usernames = new LinkedList<String>();
        List<String> passwords = new LinkedList<String>();
        List<String> cipherSuites = new LinkedList<String>();
        List<String> sslPeerNames = new LinkedList<String>();
        List<Integer> queuePorts = new LinkedList<Integer>();
        String line;
        String lastName = null;
        boolean gotQueueManager = false;
        boolean gotQueuePort = false;
        boolean gotQueueManagerName = false;
        boolean gotUsername = false;
        boolean gotPassword = false;
        boolean gotCipherSuite = false;
        boolean gotSslPeerName = false;
        while ((line = reader.readLine()) != null) {
            Matcher headMatcher = stanzaStartPattern.matcher(line);
            Matcher tailMatcher = stanzaTailPattern.matcher(line);

            if (headMatcher.matches()) {
                lastName = headMatcher.group(1);
                configurationNames.add(lastName);
                gotQueueManager = false;
                gotQueuePort = false;
                gotQueueManagerName = false;
                gotUsername = false;
                gotPassword = false;
                gotCipherSuite = false;
                gotSslPeerName = false;
            } else if (tailMatcher.matches()) {
                String param = tailMatcher.group(1);
                String value = tailMatcher.group(2);
                if ("configurationName".equals(param)) {
                    assertTrue("configurationName value '" + value
                            + "' encountered under wrong block, '" + lastName + "'",
                            value.equals(lastName));
                } else if ("queuemanagerHost".equals(param)) {
                    assertTrue("Found more than 1 queuemanagerHost param for application config object "
                            + lastName + "; value: " + value,
                            !gotQueueManager);
                    queueManagerHosts.add(value);
                    gotQueueManager = true;
                } else if ("queuemanagerPort".equals(param)) {
                    assertTrue("Found more than 1 queuemanagerPort param for application config object "
                            + lastName + "; value: " + value,
                            !gotQueuePort);
                    queuePorts.add(Integer.valueOf(value));
                    gotQueuePort = true;
                } else if ("queuemanagerName".equals(param)) {
                    assertTrue("Found more than 1 queuemanagerName param for application config object "
                            + lastName + "; value: " + value,
                            !gotQueueManagerName);
                    queueManagerNames.add(value);
                    gotQueueManagerName = true;
                } else if ("password".equals(param)) {
                    assertTrue("Found more than 1 password param for application config object "
                            + lastName + "; value: " + value,
                            !gotPassword);
                    passwords.add(value);
                    gotPassword = true;
                } else if ("username".equals(param)) {
                    assertTrue("Found more than 1 username param for application config object "
                            + lastName + "; value: " + value,
                            !gotUsername);
                    usernames.add(value);
                    gotUsername = true;
                } else if ("cipherSuite".equals(param)) {
                    assertTrue("Found more than 1 cipherSuite param for application config object "
                            + lastName + "; value: " + value,
                            !gotCipherSuite);
                    cipherSuites.add(value);
                    gotCipherSuite = true;
                } else if ("sslPeerName".equals(param)) {
                    assertTrue("Found more than 1 sslPeerName param for application config object "
                            + lastName + "; value: " + value,
                            !gotSslPeerName);
                    sslPeerNames.add(value);
                    gotSslPeerName = true;
                } else {
                    fail("Unrecognized application configuration parameter: " + param + "=" + value);
                }
            } else {
                // Unrecognized line, ignore it.
                // Not a failure because RuntimeConfiguration class designed to
                // ignore unrecognized objects.
                ;
            }
        }
        reader.close();
        for (int configNameIdx = 0; configNameIdx < configurationNames.size(); ++configNameIdx) {
            String configName = configurationNames.get(configNameIdx);
            String queueManagerHost = queueManagerHosts.get(configNameIdx);
            String queueManagerName = queueManagerNames.get(configNameIdx);
            String username = usernames.get(configNameIdx);
            String password = passwords.get(configNameIdx);
            Integer queuePort = queuePorts.get(configNameIdx);
            if ("Configuration01".equals(configName)) {
                fail("Delete application configuration Configuration01 is still persisted!");
            } else if ("Configuration02".equals(configName)) {
                assertTrue("Expected 'localhostess', got " + queueManagerHost, "localhostess".equals(queueManagerHost));
                assertTrue("Expected 'baz', got " + queueManagerName, "baz".equals(queueManagerName));
                assertTrue("Expected 'muffin', got " + username, "muffin".equals(username));
                assertTrue("Expected 'jen', got " + password, "jen".equals(password));
                assertTrue("Expected 9109, got " + queuePort.toString(), queuePort == 9109);
            } else {
                fail("Foreign application config object read from file: " + configName);
            }
        }
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        
        // Create temp directory
        // Create temp file first and reuse that name for the directory.
        testDir = File.createTempFile(RuntimeConfigurationTest.class.getName(), null);
        testDir.delete();
        testDir.mkdir();
        assert(testDir.isDirectory());
        
        // Create a Config.properties file that mimics the one created by
        // the ConfigPersistence API for each BC, during their install time.
        // This file is required to instantiate a RuntimeConfiguration object using
        // this location as the workspace root, otherwise ConfigPersistence
        // will raise an exception.
        persistenceConfigFile = new File(testDir, "config.properties");
        assertTrue(persistenceConfigFile.createNewFile());
        populateConfigPropertiesFile(persistenceConfigFile);
        
        subject = new RuntimeConfiguration(testDir.getAbsolutePath(), null);
    }

    protected void tearDown() throws Exception {
        persistenceConfigFile.delete();
        testDir.delete();
    }
    
    private void populateConfigPropertiesFile(File file)
            throws IOException {
        FileOutputStream fos = new FileOutputStream(file);
        Properties properties = new Properties();
        properties.setProperty("OutboundThreads", "10");
        try {
            properties.store(fos, null);
        } finally {
            try {
                fos.close();
            } catch (IOException e) {
                ;
            }
        }
    }
    
    static class CompositeDataImpl implements CompositeData {
        Map<Object, Object> keysToValues;
        Map<Object, SimpleType> keysToTypes;
        private CompositeType compositeType;

        CompositeDataImpl(Object[] keys,
                          Object[] values,
                          String name,
                          SimpleType[] types) {
            keysToValues = new TreeMap<Object, Object>();
            keysToTypes = new HashMap<Object, SimpleType>();
            
            if (keys.length != values.length) {
                throw new IllegalArgumentException("Different number of keys and values");
            }
            if (keys.length != types.length) {
                throw new IllegalArgumentException("Different number of keys and types");
            }
            
            String[] keysAsStrings = new String[keys.length];
            for (int i = 0; i < keys.length; ++i) {
                keysToValues.put(keys[i], values[i]);
                keysToTypes.put(keys[i], types[i]);
                keysAsStrings[i] = keys[i].toString();
            }

            try {
                compositeType = new CompositeType(name, name, keysAsStrings, keysAsStrings, types);
            } catch (OpenDataException e) {
                throw new RuntimeException(e);
            }
        }
        
        public CompositeType getCompositeType() {
            return compositeType;
        }

        public Object get(String key) {
            if (key == null) {
                throw new IllegalArgumentException("null");
            }
            if ("".equals(key)) {
                throw new IllegalArgumentException("blank");
            }
            if (!keysToValues.containsKey(key)) {
                throw new InvalidKeyException(key);
            }
            return keysToValues.get(key);
        }

        public Object[] getAll(String[] keys) {
            List<Object> keyList = new LinkedList<Object>();
            for (String key: keys) {
                keyList.add(get(key));
            }
            return keyList.toArray();
        }

        public boolean containsKey(String key) {
            return keysToValues.containsKey(key);
        }

        public boolean containsValue(Object val) {
            return keysToValues.containsValue(val.toString());
        }

        /**
         * Returns an unmodifiable Collection view of the item values contained in this <tt>CompositeData</tt> instance.
         * The returned collection's iterator will return the values in the ascending lexicographic order of the corresponding
         * item names.
         *
         * @return the values.
         */
        public Collection values() {
            // Use list so iterator follows order in which elements are added.
            // Lexicographic ordering is guaranteed because we use a SortedMap
            // with String keys.
            return Collections.unmodifiableCollection(
                    new LinkedList<Object>(keysToValues.values()));
        }
    }
}
