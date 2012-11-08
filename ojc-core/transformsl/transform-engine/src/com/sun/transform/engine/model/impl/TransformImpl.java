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

package com.sun.transform.engine.model.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jbi.management.DeploymentException;

import com.sun.transform.I18n;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.Param;
import com.sun.transform.engine.model.Transform;

/**
 * Default implementation of a {@link Transform} activity.
 * 
 * @author Kevan Simpson
 */
public abstract class TransformImpl<T> extends AbstractActivity implements Transform<T> {
	private String mFile, mSource, mResult;
	private List<Param> mParams = new ArrayList<Param>();
	private Map<String, Invocation> mInvokes = new HashMap<String, Invocation>();
	
	protected TransformImpl(String name, String file, String source, String result) {
	    super(name);
		mFile = file;
		mSource = source;
		mResult = result;
	}
	
	/** @see com.sun.transform.engine.model.Transform#addInvocation(com.sun.transform.engine.model.Invocation) */
    public void addInvocation(Invocation invoke) {
        if (invoke != null) {
            int ix = invoke.getName().indexOf(".");
            String key = invoke.getName().substring(ix + 1);
            mInvokes.put(key, invoke);
        }
    }

    /** @see com.sun.transform.engine.model.Transform#addParam(com.sun.transform.engine.model.Param) */
	public void addParam(Param p) {
		if (p != null) {
			mParams.add(p);
		}
	}

	/** @see com.sun.transform.engine.model.Transform#getFile() */
	public String getFile() {
		return mFile;
	}

    /** @see com.sun.transform.engine.model.Transform#getInvocations() */
    public Map<String, Invocation> getInvocations() {
        return mInvokes;
    }

	/** @see com.sun.transform.engine.model.Transform#getParams() */
	public Param[] getParams() {
		Param[] p = new Param[mParams.size()];
		mParams.toArray(p);
		return p;
	}

	/** @see com.sun.transform.engine.model.Transform#getResult() */
	public String getResult() {
		return mResult;
	}

	/** @see com.sun.transform.engine.model.Transform#getSource() */
	public String getSource() {
		return mSource;
	}
	
	/** @see java.lang.Object#toString() */
	@Override
	public String toString() {
		StringBuffer buff = new StringBuffer();
		buff.append(getName()).append("[").append(getSource())
			.append(" --> ").append(getFile())
			.append(" --> ").append(getResult()).append("]");
			
		return buff.toString();
	}

	protected boolean verifyFileExists(String rootPath, String fileName) throws DeploymentException {
        // null filename indicates no transformation applied; feature not a bug
        if (fileName == null || fileName.trim().length() == 0) return false;

        File file = new File(rootPath, fileName);
        if (!file.exists()) {
            throw new DeploymentException(I18n.loc(
                    "TRANSL-6020: Cannot locate XSL stylesheet: {0}", 
                    file.getAbsolutePath()));
        }
        
        return true;
	}
}
