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
 * @(#)EngineStateChangeTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist.cluster;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.jbi.engine.bpel.core.test.bpelpersist.AbstractTestCase;
import com.sun.jbi.engine.bpel.core.test.common.Utility;

public class EngineStateChangeTest extends AbstractTestCase {

    public EngineStateChangeTest(String testName) {
        super(testName);
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(EngineStateChangeTest.class);

        return suite;
    }

    public void testStartStopStart() {
        Utility.logEnter(getClass().getSimpleName(), "testStartStopStart");
        mEng.preStop();
        mEng.preStart();
        mEng.preStop();
        mEng.preStart();
        mEng.preStop();
        mEng.preStart();
        Utility.logExit(getClass().getSimpleName(), "testStartStopStart");
    }
}
