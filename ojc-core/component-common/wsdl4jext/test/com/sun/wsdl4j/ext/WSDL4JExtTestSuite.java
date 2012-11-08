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
 * @(#)WSDL4JExtTestSuite.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * WSDL4J extension unit test suite.
 * 
 * @author Jun Xu
 * @version $Revision: 1.5 $
 */
public class WSDL4JExtTestSuite {

    /**
     * Gets the test suite.
     * 
     * @return The test suite.
     */
    public static Test suite() {
        
        TestSuite suite = new TestSuite();
        
        suite.addTestSuite(SimpleTest.class);
        suite.addTestSuite(ImportTest.class);
        suite.addTestSuite(RecursiveImportTest.class);
        suite.addTestSuite(DuplicateImportTest.class);
        suite.addTestSuite(InlineSchemasTest.class);
        suite.addTestSuite(CachingTest.class);
        
        return suite;
    }
    
    /**
     * Runs the test suite
     */
    public static void main(String[] args) {
        junit.textui.TestRunner.run(suite());
    }
}
