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
 * @(#)RThrowImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.impl.ThrowImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RThrow;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;


/**
 * Runtime Throw implementation
 *
 * @author Sun Microsystems
 */
public class RThrowImpl extends ThrowImpl implements RActivity, RThrow, RVariableElement {
    private long mUniqueID;
    private RVariable mVariable;

    /**
     * Creates a new instance of RThrowImpl
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique ID
     */
    public RThrowImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /** @see com.sun.bpel.model.meta.RActivity#getNextActivity()
     */
    public RActivity getNextActivity() {
        //There is no next activity for throw
        return null;
    }

    /** @see com.sun.bpel.model.meta.RActivity#setNextActivity(com.sun.bpel.model.meta.RActivity)
     */
    public void setNextActivity(RActivity act) {
        //There is no next activity for throw 
        // No-Op
    }

    /** @see com.sun.bpel.model.meta.Common#getUniqueId()
     */
    public long getUniqueId() {
        return mUniqueID;
    }


    /**
     * gets fault name
     *
     * @return QName fault name
     */
    public QName getRFaultName() {
    	return super.getFaultName();
    }

    /**
     * gets variable
     *
     * @return RVariable variable
     */
    public RVariable getRVariable() {
        return mVariable;
    }

    /**
     * sets variable
     *
     * @param var variable
     */
    public void setRVariable(RVariable var) {
        mVariable = var;
    }
}
