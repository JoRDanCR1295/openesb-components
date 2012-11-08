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
 * @(#)DynaBeanModelTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dynabeans;

import junit.framework.TestSuite;

import org.apache.commons.beanutils.WrapDynaBean;
import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.TestBean;
import org.apache.commons.jxpath.ri.model.BeanModelTestCase;

/**
 * Test for support of DynaBeans (see BeanUtils)
 *
 * @author Dmitri Plotnikov
 * @version  
 */

public class DynaBeanModelTest extends BeanModelTestCase {
    public DynaBeanModelTest(String name) {
        super(name);
    }

    public static TestSuite suite() {
        return new TestSuite(DynaBeanModelTest.class);
//        TestSuite s = new TestSuite();
//        s.addTest(new DynaBeanModelTest("testAxisParent"));
//        return s;
    }

    protected Object createContextBean() {
        return new WrapDynaBean(new TestBean());
    }

    protected AbstractFactory getAbstractFactory() {
        return new TestDynaBeanFactory();
    }
}
