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
 * @(#)BPELModelTestSuite.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 *
 * @author Sun Microsystems
 */
public class BPELModelTestSuite {
    
    /** Creates a new instance of BPELModelTestSuite */
    public BPELModelTestSuite() {
        
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTestSuite(WSDLImportRelativePathTest.class);
        suite.addTestSuite(WSDLImportRelativePathAcrossProjectTest.class);
        suite.addTestSuite(XSDImportTest.class);
        suite.addTestSuite(BPELScopeTest.class);
        
        
        return suite;
    }
    
    /**
     * Runs the test suite using the textual runner.
     */
    public static void main(String[] args) {
        junit.textui.TestRunner.run(suite());
    }

}
