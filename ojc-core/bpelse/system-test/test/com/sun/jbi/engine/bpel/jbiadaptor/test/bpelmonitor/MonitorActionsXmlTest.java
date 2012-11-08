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
 * @(#)Assign1Test.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpelmonitor;

import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.stream.StreamSource;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.jbi.engine.bpel.XmlResourceProviderPoolImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.EngineImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.impl.ActionableMBeanImpl;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestContext;

public class MonitorActionsXmlTest extends TestCase {
    Engine mEng;
    EngineChannelSimulatorAdaptor mChannel;

    public MonitorActionsXmlTest(String arg0) {
        super(arg0);
    }

    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {

    }

    private void createEngine(String propertiesFileLoc) throws Exception {
        Properties connProp = new Properties();
        InputStream is = getClass().getResourceAsStream(propertiesFileLoc);
        connProp.load(is);

        DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(connProp);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(connProp);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ic = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, connProp);
        mEng = new EngineImpl(connProp, ic);
        mEng.preStart();
        registerXmlResourceProviderPool();
    }
    
    private void registerXmlResourceProviderPool(){
        //Register a XmlResourceProviderPool with the Registry. We use the name of
        //the interface XmlResourceProviderPool to register the pool. We create only
        //one object in the pool, since this is a single threaded environment.
        BPELSERegistry registry = BPELSERegistry.getInstance();

        XmlResourceProviderPoolImpl xmlResProviderPool;
        try {
            xmlResProviderPool = new XmlResourceProviderPoolImpl(1);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        registry.register(XmlResourceProviderPool.class.getName(), xmlResProviderPool);
    }

    /** @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
    	mEng = null;
        super.tearDown();
    }

    public static TestSuite suite() {
        return new TestSuite(MonitorActionsXmlTest.class);
    }
    
    public void test_withPersistence() throws Exception {
        String persistenceProps = "/com/sun/jbi/engine/bpel/jbiadaptor/test/common/persistenceBPELse.properties";
        createEngine(persistenceProps);
        
        ActionableMBeanImpl obj = new ActionableMBeanImpl(mEng);
        String actionsXML = obj.getActions();
        System.out.println("actionsXML file is - \n" + actionsXML);

        compareFiles("actions_with_persistence_en_US.xml", actionsXML);
    }
    
    
    public void test_withoutPersistence() throws Exception {
        String persistenceProps = "/com/sun/jbi/engine/bpel/jbiadaptor/test/common/bpelse.properties";
        createEngine(persistenceProps);
        
        ActionableMBeanImpl obj = new ActionableMBeanImpl(mEng);
        String actionsXML = obj.getActions();
        System.out.println("actionsXML file is - \n" + actionsXML);
        
        compareFiles("actions_without_persist_en_US.xml", actionsXML);
    }
    
    public void test_withMonitoring() throws Exception {
        String persistenceProps = "/com/sun/jbi/engine/bpel/jbiadaptor/test/common/bpelsemonitor.properties";
        createEngine(persistenceProps);
        
        ActionableMBeanImpl obj = new ActionableMBeanImpl(mEng);
        String actionsXML = obj.getActions();
        System.out.println("actionsXML file is - \n" + actionsXML);
        
        compareFiles("actions_with_monitoring_en_US.xml", actionsXML);
    }
    
    private void compareFiles(String expectedOPFileName, String output) throws Exception {
        DocumentBuilderFactory docFactory =  DocumentBuilderFactory.newInstance();
        docFactory.setNamespaceAware(true);
        docFactory.setValidating(false);
        
        URL fileURL = getClass().getResource(expectedOPFileName);
        if (fileURL == null) {
            fail("comparision file not foune");
        }
        InputStream ipStream = fileURL.openStream();
        StreamSource source = new StreamSource(ipStream);
        String xmlString = DOMHelper.createXmlString(source);
        
        assertEquals(xmlString, output);
    }
}
