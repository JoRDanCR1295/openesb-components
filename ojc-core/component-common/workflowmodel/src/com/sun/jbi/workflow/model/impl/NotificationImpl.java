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
 * @(#)$Id: NotificationImpl.java,v 1.8 2009/01/16 20:49:35 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import javax.xml.namespace.QName;

import com.sun.jbi.workflow.model.Message;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.Subject;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;
import com.sun.jbi.workflow.model.xmlbeans.TNotification;

public class NotificationImpl extends ModelElementImpl implements Notification {
    
    protected  TNotification  mNotificationType;

    protected  QName mQName;
    
    protected  Type mType;
    
    public NotificationImpl(TNotification notification, ModelElement parent) {
        super(notification, parent);
        this.mNotificationType = notification;
        mQName = new QName (getTask().getTargetNamespace(), mNotificationType.getName());
        if (mNotificationType.getEmail() != null) {
            mType = Type.Email;
        }
    }
        

    public Subject getSubject() {
        TExpression sub = this.mNotificationType.getMessage().getSubject();
        Subject subject = null;
        if(sub != null) {
            subject = new SubjectImpl(sub, this);
        }
        
        return subject;
    }
    
    public Message getMessage() {
        TExpression msg = this.mNotificationType.getMessage().getBody();
        Message message = null;
        if(msg != null) {
            message = new MessageImpl(msg, this);
        }
        return message;
    }

    public QName getQName() {
        // TODO Auto-generated method stub
        return mQName;
    }

    public Type getType() {
        // TODO Auto-generated method stub
        return mType;
    }


    public String getName() {
        // TODO Auto-generated method stub
        return mNotificationType.getName();
    }

}
