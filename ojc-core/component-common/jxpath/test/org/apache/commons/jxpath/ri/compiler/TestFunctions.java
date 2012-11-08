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
 * @(#)TestFunctions.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.jxpath.BasicNodeSet;
import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.NestedTestBean;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.NodeSet;

/**
 * @author Dmitri Plotnikov
 * @version  
 */
public class TestFunctions {

    private int foo;
    private String bar;

    public TestFunctions() {
    }

    public TestFunctions(int foo, String bar) {
        this.foo = foo;
        this.bar = bar;
    }

    public TestFunctions(ExpressionContext context, String bar) {
        this.foo =
            ((Number) context.getContextNodePointer().getValue()).intValue();
        this.bar = bar;
    }
    
    public TestFunctions(int foo, Object object, boolean another) {
        this.foo = foo;
        bar = String.valueOf(object);
    }

    public int getFoo() {
        return foo;
    }

    public String getBar() {
        return bar;
    }

    public void doit() {
    }

    public TestFunctions setFooAndBar(int foo, String bar) {
        this.foo = foo;
        this.bar = bar;
        return this;
    }

    public static TestFunctions build(int foo, String bar) {
        return new TestFunctions(foo, bar);
    }

    public String toString() {
        return "foo=" + foo + "; bar=" + bar;
    }

    public static String path(ExpressionContext context) {
        return context.getContextNodePointer().asPath();
    }

    public String instancePath(ExpressionContext context) {
        return context.getContextNodePointer().asPath();
    }

    public String pathWithSuffix(ExpressionContext context, String suffix) {
        return context.getContextNodePointer().asPath() + suffix;
    }

    public String className(
        ExpressionContext context,
        ExpressionContext child) 
    {
        return context.getContextNodePointer().asPath();
    }

    /**
     * Returns true if the current node in the current context is a map
     */
    public static boolean isMap(ExpressionContext context) {
        Pointer ptr = context.getContextNodePointer();
        return ptr == null ? false : (ptr.getValue() instanceof Map);
    }

    /**
     * Returns the number of nodes in the context that is passed as
     * the first argument.
     */
    public static int count(ExpressionContext context, Collection col) {
        for (Iterator iter = col.iterator(); iter.hasNext();) {
            Object element = iter.next();
            if (!(element instanceof String)) {
                throw new RuntimeException("Invalid argument");
            }
        };
        return col.size();
    }
    
    public static int countPointers(NodeSet nodeSet) {
        return nodeSet.getPointers().size();
    }
    
    public static String string(String string) {
        return string;
    }
    
    public static Collection collection() {
        ArrayList list = new ArrayList();
        list.add(new NestedTestBean("foo"));
        list.add(new NestedTestBean("bar"));
        return list;
    }
    
    public static NodeSet nodeSet(ExpressionContext context) {
        JXPathContext jxpathCtx = context.getJXPathContext();
        BasicNodeSet set = new BasicNodeSet();
        set.add(jxpathCtx.getPointer("/beans[1]"));
        set.add(jxpathCtx.getPointer("/beans[2]"));
        
        return set;
    }
    
    public static Collection items(Collection arg) {
        return arg;
    }
}
