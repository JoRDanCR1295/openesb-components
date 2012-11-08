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
 * @(#)SetUpHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.InputStream;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;

import com.sun.jbi.engine.bpel.XmlResourceProviderPoolImpl;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.EngineImpl;


/**
 * @author Sun Microsystems
 */
public class SetUpHelper {
    protected Properties mConnProp = new Properties();
    protected Engine mEng = null;
    
    public SetUpHelper() {
        String bpelsePropFile = System.getProperty("bpelse.properties", "bpelse.properties");
        InputStream is = SetUpHelper.class.getResourceAsStream(bpelsePropFile);
        try {
            mConnProp.load(is);
            
            DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
            DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
                new DummyNonXATxManagerAndDataSource(mConnProp);
            BPELSERegistry registry = BPELSERegistry.getInstance();
            registry.register(TransactionManager.class.getName(), dummyTMAndDS);
            
            InitialContext ic = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);
            mEng = new EngineImpl(mConnProp, ic);
            mEng.preStart();            
            registerXmlResourceProviderPool();
            
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }


    protected void registerXmlResourceProviderPool(){
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
    
    public Engine getEngine() {
        return mEng;
    }

    public Properties getEngineProperties() {
        return mConnProp;
    }
}
