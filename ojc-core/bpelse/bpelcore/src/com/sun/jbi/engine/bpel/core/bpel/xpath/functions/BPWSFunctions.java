/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
 /*
  * $Id: BPWSFunctions.java,v 1.10 2009/04/24 19:42:00 ksimpson Exp $
  *
  * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import java.util.Set;
import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.Functions;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.model.VariablePointer;
import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaType;

import com.sun.bpel.model.BPELElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;
import com.sun.jbi.engine.bpel.core.bpel.util.XslCache;
import com.sun.jbi.engine.bpel.core.bpel.xpath.dom.BPELSEDOMNodePointer;

/**
 * BPEL function set to be added to JXPath context.
 * 
 * @author Kevan Simpson
 */
public class BPWSFunctions implements Functions {
    private static XPath2Functions xpathFunctions = new XPath2Functions();

    private Functions bpelExtFunctions;
    private VariableScope mVarScope = null;
    private BPELElement mElement = null;
    private String mSrvcUnitName;
    private XslCache mXslCache;
    
    public BPWSFunctions() {}
    
    public BPWSFunctions(VariableScope variableScope, BPELElement element) {
    	mVarScope = variableScope;
    	mElement = element;
    	BPELProcessInstance inst = ((Context) variableScope).getProcessInstance();
		String bpInstanceId = inst.getId();
		mSrvcUnitName = inst.getBPELProcessManager().getServiceUnitName();
		mXslCache = inst.getBPELProcessManager().getXslCache();
    	bpelExtFunctions = new BPWSExtFunctions(bpInstanceId);
    }
    
	/** @see org.apache.commons.jxpath.Functions#getFunction(java.lang.String, java.lang.String, java.lang.Object[]) */
    public Function getFunction(String namespace, String name,
            Object[] parameters) {
        if (name.equals("doXslTransform")) {
            return new DoXslTransformFunction(mSrvcUnitName, mXslCache);
        } else if (name.equals("getVariableProperty") || 
        		   name.equals("getVariableNMProperty")) {
            return new GetVariableProperty(mVarScope, mElement);
        } else {
            Function funct = xpathFunctions.getFunction(namespace, name,
                    parameters);
            if (funct == null) {
                funct = bpelExtFunctions.getFunction(namespace, name,
                        parameters);
            }
            if (funct == null) {
                /* If name space starts with java:// for example java://java.lang.String
                 * it is POJO call
                 * BPELXPathContextImpl overrides getFunction to call with
                 * namespace instead of prefix String 
                 * ns = context.getJXPathContext().getNamespaceURI(nsPrefix);
                 */
                if (namespace.indexOf("//") > 0) {
                    String className = namespace.substring(namespace.indexOf("//") + 2);
                    funct = new POJOFunction(className, name, mSrvcUnitName);
                }
            }
            return funct;
        }
    }

    /** @see org.apache.commons.jxpath.Functions#getUsedNamespaces() */
    public Set getUsedNamespaces() {
        // no special behavior in this class, forward to xpath2functions
        return xpathFunctions.getUsedNamespaces();
    }
    
    public static Object convertParam(Object src) {
        if (src instanceof Pointer) {
        	Pointer cPtr = (Pointer) src;
        	
        	if (cPtr instanceof VariablePointer) {
        		VariablePointer varPtr = (VariablePointer) cPtr;
        		cPtr = varPtr.getValuePointer();
        	}
        	
            if (cPtr instanceof BPELSEDOMNodePointer) {
                BPELSEDOMNodePointer bptr = (BPELSEDOMNodePointer) cPtr;
                SchemaType schType = null;
                Object type = bptr.getSchemaType();
                if (type instanceof SchemaType) {
                    schType = (SchemaType) type;
                } else if (type instanceof SchemaField) {
                    schType = ((SchemaField) type).getType();
                }
                if (schType != null && schType.isSimpleType()) {
                	Object value = bptr.getValue();
                	if (schType.isNumeric()) {
                		// Convert to Double.
                		return new Double(value.toString());
                	}
                    return value;
                }
            }
            
            return ((Pointer) src).getNode();
            
        } else if (src instanceof EvalContext) {
            return convertParam(((EvalContext) src).getCurrentNodePointer());
        }
        return src;
    }
}
