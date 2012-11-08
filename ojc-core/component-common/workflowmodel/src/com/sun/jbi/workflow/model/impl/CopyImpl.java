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
 * @(#)$Id: CopyImpl.java,v 1.4 2009/01/16 20:49:35 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import com.sun.jbi.workflow.model.Copy;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.utl.ModelUtil;
import com.sun.jbi.workflow.model.xmlbeans.TCopy;

public class CopyImpl extends ModelElementImpl implements Copy {

	TCopy mCopyType;
	public CopyImpl(TCopy delegate, ModelElement parent) {
		super(delegate, parent);
		// TODO Auto-generated constructor stub
		mCopyType =delegate;
	}

	public String getFromExpr() {
		// TODO Auto-generated method stub
		return ModelUtil.getText(mCopyType.getFrom().getDomNode());
	}

	public String getToExpr() {
		// TODO Auto-generated method stub
		return ModelUtil.getText(mCopyType.getTo().getDomNode());
	}

}
