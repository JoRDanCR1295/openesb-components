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
 * @(#)JXPathTestSuite.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.commons.jxpath.ri.JXPathCompiledExpressionTest;
import org.apache.commons.jxpath.ri.axes.RecursiveAxesTest;
import org.apache.commons.jxpath.ri.axes.SimplePathInterpreterTest;
import org.apache.commons.jxpath.ri.compiler.ContextDependencyTest;
import org.apache.commons.jxpath.ri.compiler.CoreFunctionTest;
import org.apache.commons.jxpath.ri.compiler.CoreOperationTest;
import org.apache.commons.jxpath.ri.compiler.ExtensionFunctionTest;
import org.apache.commons.jxpath.ri.compiler.VariableTest;
import org.apache.commons.jxpath.ri.model.MixedModelTest;
import org.apache.commons.jxpath.ri.model.beans.BeanModelTest;
import org.apache.commons.jxpath.ri.model.container.ContainerModelTest;
import org.apache.commons.jxpath.ri.model.dom.DOMModelTest;
import org.apache.commons.jxpath.ri.model.dynabeans.DynaBeanModelTest;
import org.apache.commons.jxpath.ri.model.dynamic.DynamicPropertiesModelTest;
import org.apache.commons.jxpath.ri.model.jdom.JDOMModelTest;
import org.apache.commons.jxpath.util.BasicTypeConverterTest;

/**
 * <p>
 *  Test Suite for the JXPath class.  The majority of these tests use
 *  instances of the TestBean class, so be sure to update the tests if you
 *  change the characteristics of that class.
 * </p>
 *
 * <p>
 *   Note that the tests are dependent upon the static aspects
 *   (such as array sizes...) of the TestBean.java class, so ensure
 *   that all changes to TestBean are reflected here and in other JXPath tests.
 * </p>
 *
 * @author Dmitri Plotnikov
 * @version  
 */

public class JXPathTestSuite extends TestCase {
    private static boolean enabled = true;

    /**
     * Exercise the whole suite
     */
    public static void main(String args[]) {
        TestRunner.run(suite());
    }

    public JXPathTestSuite(String name) {
        super(name);
    }

    /**
     * Return the tests included in this test suite.
     */
    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTestSuite(JXPathCompiledExpressionTest.class);
        suite.addTestSuite(SimplePathInterpreterTest.class);
        suite.addTestSuite(ContextDependencyTest.class);
        suite.addTestSuite(CoreFunctionTest.class);
        suite.addTestSuite(CoreOperationTest.class);
        suite.addTestSuite(ExtensionFunctionTest.class);
        suite.addTestSuite(VariableTest.class);
        suite.addTestSuite(ContainerModelTest.class);
        suite.addTestSuite(BeanModelTest.class);
        suite.addTestSuite(DynamicPropertiesModelTest.class);
        suite.addTestSuite(DOMModelTest.class);
        suite.addTestSuite(DynaBeanModelTest.class);
        suite.addTestSuite(JDOMModelTest.class);
        suite.addTestSuite(MixedModelTest.class);
        suite.addTestSuite(BasicTypeConverterTest.class);
        suite.addTestSuite(RecursiveAxesTest.class);
        return suite;
    }
}
