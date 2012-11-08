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
 * @(#)TestMixedModelFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.NestedTestBean;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.TestBean;
import org.apache.commons.jxpath.TestMixedModelBean;

/**
 * Test AbstractFactory.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class TestMixedModelFactory extends AbstractFactory {

    /**
     * Create a new instance and put it in the collection on the parent object.
     * Return <b>false</b> if this factory cannot create the requested object.
     */
    public boolean createObject(
        JXPathContext context,
        Pointer pointer,
        Object parent,
        String name,
        int index) 
    {
        if (name.equals("nestedBean")) {
            ((TestBean) parent).setNestedBean(new NestedTestBean("newName"));
            return true;
        }
        else if (name.equals("beans")) {
            TestBean bean = (TestBean) parent;
            if (bean.getBeans() == null || index >= bean.getBeans().length) {
                bean.setBeans(new NestedTestBean[index + 1]);
            }
            bean.getBeans()[index] = new NestedTestBean("newName");
            return true;
        }
        else if (name.equals("map")) {
            ((TestBean) parent).setMap(new HashMap());
            return true;
        }
        else if (name.equals("TestKey5")) {
            TestBean tb = new TestBean();
            tb.setNestedBean(null);
            tb.setBeans(null);
            ((Map) parent).put(name, tb);
            return true;
        }
        else if (name.equals("matrix")) {
            int[][] matrix = new int[2][];
            matrix[0] = new int[1];
            //            matrix[1] = new int[2];
             ((TestMixedModelBean) parent).setMatrix(matrix);
            return true;
        }
        return false;
    }

    public boolean declareVariable(JXPathContext context, String name) {
        return false;
    }
}
