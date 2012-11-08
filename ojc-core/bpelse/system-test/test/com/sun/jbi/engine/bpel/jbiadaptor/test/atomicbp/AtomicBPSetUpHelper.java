package com.sun.jbi.engine.bpel.jbiadaptor.test.atomicbp;

import java.io.InputStream;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.EngineImpl;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.SetUpHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestContext;

public class AtomicBPSetUpHelper extends SetUpHelper {

	public AtomicBPSetUpHelper() {
        String bpelsePropFile = System.getProperty("bpelse.properties", "bpelse.properties");
        InputStream is = SetUpHelper.class.getResourceAsStream(bpelsePropFile);
        try {
            mConnProp.load(is);
            
            DummyTxManager dummyTM = new DummyTxManager(mConnProp);
            DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
                new DummyNonXATxManagerAndDataSource(mConnProp);
            BPELSERegistry registry = BPELSERegistry.getInstance();
            registry.register(TransactionManager.class.getName(), dummyTM);
            
            InitialContext ic = new TestContext(dummyNonXATMAndDS, dummyTM, mConnProp);
            mEng = new EngineImpl(mConnProp, ic);
            mEng.preStart();            
            registerXmlResourceProviderPool();
            
        } catch (Exception ex) {
            ex.printStackTrace();
        }
	}

}
