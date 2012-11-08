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
 * @(#)ProcessDefImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model.impl;

import java.util.ArrayList;
import java.util.List;

import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.ProcessDef;

/**
 * Default implementation of a {@link ProcessDef} activity.
 * @author Kevan Simpson
 */
public class ProcessDefImpl extends AbstractActivity implements ProcessDef {
	private List<Activity> mActivities;
	private Invocation mInvoke;
	
	public ProcessDefImpl(Invocation invoke) {
	    super("Process["+ invoke.getOperation().getName() +"]");
		mInvoke = invoke;
		mActivities = new ArrayList<Activity>();
	}

	/** @see com.sun.transform.engine.model.ProcessDef#addActivity(com.sun.transform.engine.model.Activity) */
	public void addActivity(Activity act) {
		if (act != null) {
			mActivities.add(act);
		}
	}

	/** @see com.sun.transform.engine.model.ProcessDef#countActivities() */
	public int countActivities() {
		return mActivities.size();
	}

	/** @see com.sun.transform.engine.model.ProcessDef#getActivity(int) */
	public Activity getActivity(int index) {
		return (index >= 0 && index < mActivities.size()) ? mActivities.get(index) : null;
	}

	/** @see com.sun.transform.engine.model.ProcessDef#getInvocation() */
	public Invocation getInvocation() {
		return mInvoke;
	}

    /** @see com.sun.transform.engine.model.ProcessDef#getTargetNamespace() */
    public String getTargetNamespace() {
        return getInvocation().getInfo().getServiceName().getNamespaceURI();
    }
}
