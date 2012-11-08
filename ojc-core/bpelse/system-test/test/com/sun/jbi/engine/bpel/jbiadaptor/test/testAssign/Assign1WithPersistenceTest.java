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
 * @(#)Assign1WithPersistenceTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.testAssign;

import junit.framework.TestSuite;

import com.sun.jbi.engine.bpel.jbiadaptor.test.common.SetUpHelper;

/**
 * @author Sun Inc
 * Jul 19, 2006
 */
public class Assign1WithPersistenceTest extends Assign1Test {
    
    public Assign1WithPersistenceTest(String arg0) {
        super(arg0);
    }
    
    public static TestSuite suite() {
        return new TestSuite(Assign1WithPersistenceTest.class);
    }
    
    protected void setUp() throws Exception {
        SetUpHelper setUp = new SetUpHelper();
        mEng = setUp.getEngine();
    }
    protected void tearDown() throws Exception {
        // don't call super.tearDown, it will set mEng to null
    }
}
