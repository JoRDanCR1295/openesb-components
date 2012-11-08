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
 * @(#)VariableFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.TestBean;

/**
 * Test AbstractFactory.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class VariableFactory extends AbstractFactory {

    /**
     */
    public boolean createObject(
        JXPathContext context,
        Pointer pointer,
        Object parent,
        String name,
        int index) 
    {
        if (name.equals("testArray")) {
            ((TestBean[]) parent)[index] = new TestBean();
            return true;
        }
        else if (name.equals("stringArray")) {
            ((String[]) parent)[index] = "";
            return true;
        }
        else if (name.equals("array")) {
            ((String[]) parent)[index] = "";
            return true;
        }
        return false;
    }

    /**
     * Create a new object and set it on the specified variable
     */
    public boolean declareVariable(JXPathContext context, String name) {
        if (name.equals("test")) {
            context.getVariables().declareVariable(name, new TestBean());
            return true;
        }
        else if (name.equals("testArray")) {
            context.getVariables().declareVariable(name, new TestBean[0]);
            return true;
        }
        else if (name.equals("stringArray")) {
            context.getVariables().declareVariable(
                name,
                new String[] { "Value1" });
            return true;
        }
        context.getVariables().declareVariable(name, null);
        return true;
    }
}
