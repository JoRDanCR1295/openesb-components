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
 * @(#)EscalationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import com.sun.jbi.workflow.model.Assignment;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.LocalNotification;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.utl.ModelUtil;
import com.sun.jbi.workflow.model.xmlbeans.TDeadlineExpr;
import com.sun.jbi.workflow.model.xmlbeans.TDurationExpr;
import com.sun.jbi.workflow.model.xmlbeans.TEscalation;
import com.sun.jbi.workflow.model.xmlbeans.TLocalNotification;
import java.util.ArrayList;

import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.xmlbeans.GDuration;

/**
 * 
 * 
 */
public class EscalationImpl extends ModelElementImpl implements Escalation {

    private TEscalation mEscalation;

//    private Date mDeadline;
//
//    private GDuration mDuration;
//
//    private Assignment mAssignment;

    private List<LocalNotification> mLocalNotifications = new ArrayList<LocalNotification>();

    /** Creates a new instance of EscalationImpl */
    public EscalationImpl(TEscalation escalation, Task parent) {
        super(escalation, parent);
        this.mEscalation = escalation;
        List<TLocalNotification> lnList = this.mEscalation
                .getLocalNotificationList();
        if (lnList != null) {
            Iterator<TLocalNotification> it = lnList.iterator();
            while (it.hasNext()) {
                TLocalNotification ln = it.next();
                LocalNotification localNotification = new LocalNotificationImpl(
                        ln, this);
                this.mLocalNotifications.add(localNotification);
            }
        }
    }

    public synchronized Date getDeadlineObject(JXPathContext context)
            throws ModelException {
        Date mDeadline = null;
        try {
                TDeadlineExpr deadlineExp = this.mEscalation.getDeadline();
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
                TDurationExpr durationExp = this.mEscalation.getDuration();
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

    public synchronized Assignment getAssignment(JXPathContext context)
            throws ModelException {
        Assignment mAssignment = null;
        if ( this.mEscalation.getAssignment() != null) {
            mAssignment = new AssignmentImpl(this.mEscalation
                    .getAssignment(), this);
        }

        return mAssignment;
    }

    public List<LocalNotification> getLocalNotifications() {
        return this.mLocalNotifications;
    }

}
