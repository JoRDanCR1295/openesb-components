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
 * @(#)TransformImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.runtime.Engine;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.TransformUnit;

/**
 * Represents a runtime transformation activity.
 * @author Kevan Simpson
 */
public class TransformUnitImpl extends AbstractActivityUnit 
							   implements TransformUnit {
	
	public TransformUnitImpl(Transform tr, ProcessInstance proc) {
		super(tr, proc);
	}

	/** @see com.sun.transform.engine.runtime.ActivityUnit#execute(com.sun.transform.engine.runtime.Engine) */
	public boolean execute(Engine eng) throws ProcessingException {
		if (eng != null) {
			return eng.process(this);
		}
		return false;
	}
}
