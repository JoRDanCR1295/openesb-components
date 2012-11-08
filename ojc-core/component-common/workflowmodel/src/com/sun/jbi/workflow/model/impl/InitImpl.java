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
 * @(#)$Id: InitImpl.java,v 1.4 2009/01/16 20:49:35 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.xmlbeans.XmlObject;

import com.sun.jbi.workflow.model.Init;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.ChangeVariables;
import com.sun.jbi.workflow.model.Variables;
import com.sun.jbi.workflow.model.xmlbeans.TChangeVariables;
import com.sun.jbi.workflow.model.xmlbeans.TInit;


public class InitImpl extends ModelElementImpl implements Init {

	TInit mInit;
	List<ChangeVariables> mVariableInit;
	Variables mVariables;
	
	public InitImpl(TInit delegate, ModelElement parent) {
		super(delegate, parent);
		// TODO Auto-generated constructor stub
		mInit = delegate;   
        if (mInit == null) {
            mVariableInit = new ArrayList <ChangeVariables> ();
        } else {
            mVariableInit = new ArrayList <ChangeVariables> ();
            List<TChangeVariables> mInits = mInit.getVariableInitList();
            if (mInits != null && mInits.size() > 0) {
                for (int i = 0; i< mInits.size(); i ++) {
                    ChangeVariables vinit = new ChangeVariablesImpl (mInits.get(i), this);
                    mVariableInit.add(vinit);
                }
            }  
            if (mInit.getVariables() != null) {
                mVariables = new VariablesImpl (mInit.getVariables(), this);       
            }
        }        
	}
	public List<ChangeVariables> getChangeVariables() {
		// TODO Auto-generated method stub		
		return mVariableInit;
	}

	public Variables getVariables() {
		// TODO Auto-generated method stub
		return mVariables;
	}

}
