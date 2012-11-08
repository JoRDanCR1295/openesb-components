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
 * @(#)MOF.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.snmpbc.mbeans.TestMBeanImpl;
import com.sun.jbi.snmpbc.mbeans.TestMBeanIntf;
import com.sun.jbi.snmpengine.SNMPEngineFactory;
import com.sun.jbi.snmpengine.SNMPRA;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import javax.xml.namespace.QName;

/**
 * represents metadata store used by SNMP BC.  starts an instance of SNMP Engine
 * for each MOF.
 *
 * @author echou
 */
public class MOF extends GenericOperation {

    private static final Logger logger = Logger.getLogger(MOF.class.getName());
    
    private String id;
    private SNMPServiceUnitManager suManager;
   
    // SNMP Engine instance
    private SNMPRA snmpRA;
    private int snmpPort;
    private InboundMessageProcessorListenerEndpoint callback;
    
    // test mbean
    TestMBeanImpl testMBean;
    private ObjectName testMBName;
    
    
    public MOF(SNMPServiceUnitManager suManager, 
            String id,
            QName operationName, 
            Endpoint endpoint) {
        super(operationName, endpoint);
        
        this.id = id;
        this.suManager = suManager;
        this.snmpPort = endpoint.getSNMPAddress().getPort().intValue();
    }
    
    public void start() throws Exception {
        callback = new InboundMessageProcessorListenerEndpoint(this, suManager);
        
        // start SNMP Engine
        SNMPEngineFactory snmpFactory = new SNMPEngineFactory();
        snmpRA = snmpFactory.create();
        snmpRA.setPort(snmpPort);
        
        try {
            snmpRA.start(callback);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error during starting SNMP Engine", e);
            throw e;
        }
        
        try {
            initTestMBean();
        } catch (Exception e) {
            logger.log(Level.WARNING, "Error when initializing TestMBean", e);
        }
    }
    
    private void initTestMBean() throws Exception {
        // register testing mbean
        MBeanServer mbServer = suManager.getComponentContext().getMBeanServer();
        MBeanNames mbNames = suManager.getComponentContext().getMBeanNames();
        testMBName = mbNames.createCustomComponentMBeanName("snmpbc_tester_" + snmpPort);
        testMBean = new TestMBeanImpl(callback);
        StandardMBean mbean = 
                new StandardMBean(testMBean, TestMBeanIntf.class);
        mbServer.registerMBean(mbean, testMBName);
        logger.log(Level.FINE, "registered testmbean: " + testMBName);
        
    }
    
    private void cleanupTestMBean() throws Exception {
        // unregister testing mbean
        MBeanServer mbServer = suManager.getComponentContext().getMBeanServer();
        mbServer.unregisterMBean(testMBName);
        testMBean = null;
        logger.log(Level.FINE, "unregistered testmbean: " + testMBName);
    }
    
    public void stop() throws Exception {
        // stop SNMP Engine
        snmpRA.stop();
        callback = null;
        
        try {
            cleanupTestMBean();
        } catch (Exception e) {
            logger.log(Level.WARNING, "Error when cleanup TestMBean", e);
        }
    }
    
    public String getId() {
        return id;
    }
    
    public SNMPRA getRA() {
        return snmpRA;
    }
    
    public InboundMessageProcessorListenerEndpoint getInboundMessageProcessorListenerEndpoint() {
        return callback;
    }
    
    public TestMBeanImpl getTestMBean() {
        return testMBean;
    }
}
