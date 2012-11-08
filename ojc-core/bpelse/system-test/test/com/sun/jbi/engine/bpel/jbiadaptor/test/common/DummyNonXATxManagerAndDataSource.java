package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.util.Properties;

public class DummyNonXATxManagerAndDataSource extends
        DummyTxManagerAndDataSource {

    public DummyNonXATxManagerAndDataSource(Properties properties) {
        super(properties);
        // instead of overriding the getConnection() APIS and making some of the member things
        // protected, chose this simpler version of setting boolean flag.
        mNonXAFlag = true;
    }

}
