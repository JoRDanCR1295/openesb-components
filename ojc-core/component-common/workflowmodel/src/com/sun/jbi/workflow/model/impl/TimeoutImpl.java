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
 * @(#)TimeoutImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import java.util.Date;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.xmlbeans.GDuration;

import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.Timeout;
import com.sun.jbi.workflow.model.utl.ModelUtil;
import com.sun.jbi.workflow.model.xmlbeans.TDeadlineExpr;
import com.sun.jbi.workflow.model.xmlbeans.TDurationExpr;
import com.sun.jbi.workflow.model.xmlbeans.TTimeout;

/**
 * 
 * 
 */
public class TimeoutImpl extends ModelElementImpl implements Timeout {

    private TTimeout mTimeout;

    private Task mParent;


    /** Creates a new instance of TimeoutImpl */
    public TimeoutImpl(TTimeout timeout, Task parent) {
        super(timeout, parent);
        this.mTimeout = timeout;
        this.mParent = parent;
    }

    public synchronized Date getDeadlineObject(JXPathContext context)
            throws ModelException {
        Date mDeadline = null;
        try {
                TDeadlineExpr deadlineExp = this.mTimeout.getDeadline();
                mDeadline = ModelUtil.getDeadlineObject(deadlineExp,
                        context, getTask());
        } catch (Exception ex) {
            throw new ModelException(ex);
        }
        return mDeadline;
    }

    public synchronized GDuration getDurationObject(JXPathContext context)
            throws ModelException {
        GDuration mDuration = null;
        try {
                TDurationExpr durationExp = this.mTimeout.getDuration();
                mDuration = ModelUtil.getDurationObject(durationExp,
                        context, getTask());
        } catch (Exception ex) {
            throw new ModelException(ex);
        }

        return mDuration;
    }

    public synchronized Date getDurationDate(JXPathContext context)
            throws ModelException {
        return ModelUtil.getDurationDate(getDurationObject(context));
    }

}
