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
 * @(#)TestDynaBeanFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dynabeans;

import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.NestedTestBean;
import org.apache.commons.jxpath.Pointer;

/**
 * Test AbstractFactory.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class TestDynaBeanFactory extends AbstractFactory {

    /**
     */
    public boolean createObject(
        JXPathContext context,
        Pointer pointer,
        Object parent,
        String name,
        int index) 
    {
        if (name.equals("nestedBean")) {
            ((DynaBean) parent).set(
                "nestedBean",
                new NestedTestBean("newName"));
            return true;
        }
        else if (name.equals("beans")) {
            DynaBean bean = (DynaBean) parent;
            Object beans[] = (Object[]) bean.get("beans");
            if (beans == null || index >= beans.length) {
                beans = new NestedTestBean[index + 1];
                bean.set("beans", beans);
            }
            beans[index] = new NestedTestBean("newName");
            return true;
        }
        else if (name.equals("integers")) {
            DynaBean bean = (DynaBean) parent;
            bean.set("integers", index, new Integer(0));
            return true;
        }
        return false;
    }

    /**
     */
    public boolean declareVariable(JXPathContext context, String name) {
        context.getVariables().declareVariable(name, null);
        return true;
    }
}
