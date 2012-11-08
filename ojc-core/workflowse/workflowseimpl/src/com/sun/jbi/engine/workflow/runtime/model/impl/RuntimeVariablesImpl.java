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
 * @(#)$Id: RuntimeVariablesImpl.java,v 1.4 2010/02/15 19:24:13 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model.impl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.jxpath.BasicVariables;

import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.workflow.model.RuntimeVariables;

public class RuntimeVariablesImpl  implements RuntimeVariables {
	
	    /**
	     * Contains the values of declared variables
	     */
	private HashMap vars = new HashMap();
	
	protected Map<String, VariableType> mTypeMap = new HashMap<String, VariableType> ();

	public void declareVariable(String varName, Object value, VariableType type) {
		// TODO Auto-generated method stub
		vars.put(varName, value);
		mTypeMap.put(varName, type);
	}
	
    public void declareVariable(String varName, Object value) {
    	vars.put(varName, value);
		VariableType type = VariableType.getType(value);
		if (type == null) {
			throw new RuntimeException (I18n.loc("WLM-6067: Unsupported object type : {0}", value.getClass().getName()));
		}
		mTypeMap.put(varName, type);
    }
	
	public void undeclareVariable(String varName) {
		// TODO Auto-generated method stub
		vars.remove(varName);
		mTypeMap.remove (varName);
	}
	public VariableType getVariableType(String varName) {
		// TODO Auto-generated method stub
		return mTypeMap.get(varName);
	}

	public int size() {
		// TODO Auto-generated method stub
		return vars.size();
	}

	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return vars.toString();
	}

	public Set getVars() {
		// TODO Auto-generated method stub
		return new HashSet (vars.values());
	}

	public HashMap<String, Object> getVarMap() {
		// TODO Auto-generated method stub
		return vars;
	}

	public Object getVariable(String varName) {
		// TODO Auto-generated method stub
		return vars.get(varName);
	}

	public boolean isDeclaredVariable(String varName) {
		// TODO Auto-generated method stub
		return vars.containsKey(varName);
	} 
	
	

}
