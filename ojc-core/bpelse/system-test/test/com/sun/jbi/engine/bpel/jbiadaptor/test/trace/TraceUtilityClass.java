package com.sun.jbi.engine.bpel.jbiadaptor.test.trace;

import java.util.Properties;

import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.UtilityClass;

public class TraceUtilityClass extends UtilityClass {

	public TraceUtilityClass() {
		super();
	}

	public void initiateBPInstanceForTraceWithWait(final Properties props, 
            final Engine engine, DeploymentBindings deplBindings) throws Exception {
    	initiateBPInstance(props, engine, deplBindings);
    	// The loop count has to match the bpel used in the test case.
    	for (int i = 0; i < 9; i++) {
    		waitAndContinue(engine);
    	}
    }
	
    private void waitAndContinue(Engine eng) throws Exception {
    	Object lock = new Object();
        synchronized (lock) {
            lock.wait(4000);
        }
        eng.process();
    }
}
