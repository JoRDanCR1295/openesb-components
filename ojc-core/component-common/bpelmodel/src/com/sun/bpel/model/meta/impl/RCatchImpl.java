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
 * @(#)RCatchImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import com.sun.bpel.model.impl.CatchImpl;

import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;
import com.sun.bpel.model.common.MessageManager;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class RCatchImpl extends CatchImpl implements RVariableElement, ScopingElement {

    /** MessageManager for localized strings. */    
    private static MessageManager MESSAGES = MessageManager.getManager(RCatchImpl.class);
    
    private long mUniqueID;

    /**
     * Creates a new instance of RTOImpl
     *
     * @param bpeldoc runtime BPEL document
     * @param id DOCUMENT ME!
     */
    public RCatchImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /**
     * @see com.sun.bpel.model.meta.RVariableElement#getRVariable()
     */
    public RVariable getRVariable() {
        RVariable var = null;
        var = (RVariable) super.getBPELFaultVariable();

        if (var != null) {
            var.setScopeId(getScopeId());
        }

        return var;
    }

    /**
     * @see RVariableElement#setRVariable(com.sun.bpel.model.meta.RVariable)
     */
    public void setRVariable(RVariable var) {
        throw new UnsupportedOperationException(
            getClass().getName() + MESSAGES.getString("RCatchImpl_NOT_SUPPORTED")
        ); //$NON-NLS-1$
    }

    /**
     * @see com.sun.bpel.model.meta.ScopingElement#getScopeId()
     */
    public long getScopeId() {
        return mUniqueID;
    }
}
