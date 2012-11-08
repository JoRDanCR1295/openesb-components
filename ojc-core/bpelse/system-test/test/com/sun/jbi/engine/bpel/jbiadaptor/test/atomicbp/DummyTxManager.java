package com.sun.jbi.engine.bpel.jbiadaptor.test.atomicbp;

import java.sql.Connection;
import java.util.Properties;

import javax.transaction.NotSupportedException;
import javax.transaction.SystemException;

import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyTransaction;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyTxManagerAndDataSource;

public class DummyTxManager extends DummyTxManagerAndDataSource {

	public DummyTxManager(Properties properties) {
		super(properties);
	}

	@Override
	public void begin() throws NotSupportedException, SystemException {
		DummyTransaction tx = new DummyTransaction(this);
        mTxMap.put(Thread.currentThread().getName(), tx);
	}
}