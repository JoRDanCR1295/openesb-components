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
 * @(#)RCorrelationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.impl.CorrelationImpl;

import com.sun.bpel.model.meta.RCorrelation;
import com.sun.bpel.model.meta.RCorrelationSet;
import com.sun.bpel.xml.common.model.XMLDocument;



/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class RCorrelationImpl extends CorrelationImpl implements RCorrelation {
    private CorrelationSet mCorrSet = null;

    /**
     * Constructs new instance of RCorrelation element
     *
     * @param d Owner document.
     */
    public RCorrelationImpl(XMLDocument d) {
        super(d);
    }

    /**
     * @see com.sun.bpel.model.meta.RCorrelation#getCorrelationSet()
     */
    public RCorrelationSet getCorrelationSet() {
        return (RCorrelationSet) mCorrSet;
    }

    /**
     * @see com.sun.bpel.model.meta.RCorrelation#setCorrelationSet(RCorrelationSet)
     */
    public void setCorrelationSet(RCorrelationSet set) {
        mCorrSet = set;
    }
}
