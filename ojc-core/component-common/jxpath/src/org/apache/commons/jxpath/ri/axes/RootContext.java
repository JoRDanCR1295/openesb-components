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
 * @(#)RootContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.NodeSet;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.JXPathContextReferenceImpl;
import org.apache.commons.jxpath.ri.NamespaceResolver;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * EvalContext that is used to hold the root node for the path traversal.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class RootContext extends EvalContext {
    private JXPathContextReferenceImpl jxpathContext;
    private NodePointer pointer;
    private Object registers[];
    private int availableRegister = 0;
    private NamespaceResolver namespaceResolver;
    public static final Object UNKNOWN_VALUE = new Object();
    private static final int MAX_REGISTER = 4;

    public RootContext(
        JXPathContextReferenceImpl jxpathContext,
        NodePointer pointer) 
    {
        super(null);
        this.jxpathContext = jxpathContext;
        this.pointer = pointer;
        if (pointer != null) {
            pointer.setNamespaceResolver(jxpathContext.getNamespaceResolver());
        }
    }

    public JXPathContext getJXPathContext() {
        return jxpathContext;
    }

    public RootContext getRootContext() {
        return this;
    }
    
    public EvalContext getAbsoluteRootContext() {
        return jxpathContext.getAbsoluteRootContext();
    }

    public NodePointer getCurrentNodePointer() {
        return pointer;
    }
    
    public Object getValue() {
        return pointer;
    }
    
    public int getCurrentPosition() {
        throw new UnsupportedOperationException();
    }

    public boolean nextNode() {
        throw new UnsupportedOperationException();
    }

    public boolean nextSet() {
        throw new UnsupportedOperationException();
    }

    public boolean setPosition(int position) {
        throw new UnsupportedOperationException();
    }

    public EvalContext getConstantContext(Object constant) {
        if (constant instanceof NodeSet) {
            return new NodeSetContext(
                new RootContext(jxpathContext, null),
                (NodeSet) constant);
        }
                
        NodePointer pointer;
        if (constant instanceof NodePointer) {
            pointer = (NodePointer) constant;
        }
        else {
            pointer = NodePointer.newNodePointer(
                    new QName(null, ""),
                    constant,
                    null);
        }
        return new InitialContext(new RootContext(jxpathContext, pointer));
    }

    public EvalContext getVariableContext(QName variableName) {
        return new InitialContext(
            new RootContext(
                jxpathContext,
                jxpathContext.getVariablePointer(variableName)));
    }

    public Function getFunction(QName functionName, Object[] parameters) {
        return jxpathContext.getFunction(functionName, parameters);
    }

    public Object getRegisteredValue(int id) {
        if (registers == null || id >= MAX_REGISTER || id == -1) {
            return UNKNOWN_VALUE;
        }
        return registers[id];
    }

    public int setRegisteredValue(Object value) {
        if (registers == null) {
            registers = new Object[MAX_REGISTER];
            for (int i = 0; i < MAX_REGISTER; i++) {
                registers[i] = UNKNOWN_VALUE;
            }
        }
        if (availableRegister >= MAX_REGISTER) {
            return -1;
        }
        registers[availableRegister] = value;
        availableRegister++;
        return availableRegister - 1;
    }

    public String toString() {
        return super.toString() + ":" + pointer.asPath();
    }
}
