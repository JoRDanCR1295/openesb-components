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
 * @(#)$Id: ChangeVariablesImpl.java,v 1.2 2009/01/16 20:49:35 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.workflow.model.ChangeVariables;
import com.sun.jbi.workflow.model.Copy;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.xmlbeans.TChangeVariables;
import com.sun.jbi.workflow.model.xmlbeans.TCopy;


public class ChangeVariablesImpl extends ModelElementImpl implements ChangeVariables {

    TChangeVariables mChangeVariables;
	List<Copy> mCopies;
	
	public ChangeVariablesImpl(TChangeVariables delegate, ModelElement parent) {
		super(delegate, parent);
		// TODO Auto-generated constructor stub
		mChangeVariables = delegate;
        mCopies = new ArrayList<Copy> ();
        List<TCopy> copies = mChangeVariables.getCopyList();
        if (copies != null && copies.size() > 0) {
            for (int i = 0; i < copies.size(); i++) {
                Copy copy = new CopyImpl (copies.get(i), this);
                mCopies.add(copy);
            }           
        }        
	}

	public List<Copy> getCopies() {
		// TODO Auto-generated method stub
		return mCopies;
	}

}
