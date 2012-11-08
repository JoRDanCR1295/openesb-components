package com.sun.transform.engine.runtime.impl;


import java.io.File;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.transform.descriptor.ProcessDescriptor;
import com.sun.transform.engine.model.impl.ProcessFactoryImpl;
import com.sun.transform.engine.runtime.InvocationUnit;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.TransformUnit;

/**
 * Unit test for {@link XsltseDescriptor}.
 * @author Kevan Simpson
 */
public class EngineImplTest extends TestCase {
	/**
	 * Creates test suite.
	 * @return test suite.
	 * @throws Exception
	 */
    public static Test suite() throws Exception {
        return new TestSuite(EngineImplTest.class);
    }

	/**
	 * @param name
	 */
	public EngineImplTest(String name) {
		super(name);
	}

	public void testActivityCount() throws Exception {
		ProcessInstance proc = createProcess("simple");
		TestEngineImpl eng = new TestEngineImpl();
		assertTrue("Engine run failed!", eng.process(proc));
		assertEquals("Wrong activity count!", 7, eng.getActivityCount());
	}
	
	public void testInvokeCompletion() throws Exception {
		ProcessInstance proc = createProcess("simple");
		TestEngineImpl eng = new TestEngineImpl(3);
		assertFalse("Engine run did not interrupt!", eng.process(proc));
		assertTrue("Engine run failed!", eng.process(proc));
		eng = new TestEngineImpl(5);
		try {
			eng.process(proc);
			fail("Complete process should not run twice!");
		}
		catch (Exception e) {
			assertTrue("Wrong exception!", e instanceof ProcessingException);
		}
		
		proc = createProcess("simple");
		assertFalse("Engine run did not interrupt!", eng.process(proc));
		assertTrue("Engine run failed!", eng.process(proc));
	}

	ProcessInstance createProcess(String rsrc) throws Exception {
		File path = new File(getClass().getResource(rsrc).toURI());
		ProcessDescriptor pd = ProcessDescriptor.parse(
		        path.getAbsolutePath(), new ProcessFactoryImpl());
		
		EndpointInfo info = new EndpointInfo(true, 
											 "TestService", 
											 QName.valueOf("{http://sun.com/XsltRRTest}xsltRRPort"), 
											 QName.valueOf("{http://sun.com/processDescriptor/engineTest}xsltse"), 
											 null);
		return new ProcessInstanceImpl(pd.lookupEndpointDef(info).getServiceDef("copyEmpl"), null, null);
	}
	
	private static class TestEngineImpl extends EngineImpl {
		private int mActCount = 0;
		private int mInterrupt = -1;
		
		public TestEngineImpl() {
			this(-1);
		}
		
		public TestEngineImpl(int interrupt) {
			mInterrupt = interrupt;
		}
		
		/** @see com.sun.transform.engine.runtime.impl.EngineImpl#process(com.sun.transform.engine.runtime.InvocationUnit) */
		public boolean process(InvocationUnit inv) throws ProcessingException {
			if (count()) {
				return super.process(inv);
			}
			return false;
		}


		/** @see com.sun.transform.engine.runtime.impl.EngineImpl#process(com.sun.transform.engine.runtime.TransformUnit) */
		public boolean process(TransformUnit tr) throws ProcessingException {
			count();
			return super.process(tr);
		}

		/** @see com.sun.transform.engine.runtime.impl.EngineImpl#registerInvocationVariables(com.sun.transform.engine.runtime.InvocationUnit) */
		protected void registerInvocationVariables(InvocationUnit unit) throws ProcessingException {
			// skip verification of message content for testing purposes
		}
		
		public int getActivityCount() {
			return mActCount;
		}
		
		private boolean count() {
			mActCount += 1;
			if (mInterrupt == mActCount) {
				mInterrupt = -1;
				return false;
			}
			return true;
		}
	}
}
