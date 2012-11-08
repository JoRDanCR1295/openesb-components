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
 * @(#)TraceModelTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.test.modelparsing;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
/**
 * 
 *
 *
 * @author Sun Microsystems
 */
public class SunExtensionAttrsTest extends AbstractTestCase {
	
    //private static Logger mLogger = Logger.getLogger(TraceModelTest.class.getName());

	/**
	 * 
	 */
    public SunExtensionAttrsTest(String testName) {
		super(testName);
	}

    /**
     * 
     */
	protected void setUp() throws Exception {
    }

	/**
	 * 
	 */
    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    /**
     * 
     * @return
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(SunExtensionAttrsTest.class);

        return suite;
    }

    /**
     * 
     * @throws Exception
     */
    public void testSunExtensionAttributes() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testSunExtensionAttributes");

        RBPELProcess process = loadBPELModel("sunextensionattrs/SunExtensionAttrs.bpel");
 
        // Check that the attribute IgnoreMissingFromData is yes.
        assertNotNull(process.getIgnoreMissingFromData());
        assertEquals("yes", process.getIgnoreMissingFromData());
        assertNotNull(process.getChildActivity());
        
        // Check that the attribute waitingRequestLifeSpan is 10.
        assertNotNull(process.getWaitingRequestLifeSpan());
        assertEquals("10", process.getWaitingRequestLifeSpan());
        assertNotNull(process.getChildActivity());

        // Check that the attribute persistenceOptOut is yes.
        assertNotNull(process.getPersistenceOptOut());
        assertEquals("yes", process.getPersistenceOptOut());
        assertNotNull(process.getChildActivity());
        
        Utility.logExit(getClass().getSimpleName(), "testSunExtensionAttributes");
    }
}
