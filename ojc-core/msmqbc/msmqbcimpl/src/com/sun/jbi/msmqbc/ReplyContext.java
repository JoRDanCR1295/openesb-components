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
 * @(#)ReplyContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc;

import javax.xml.namespace.QName;

import com.sun.jbi.msmqbc.extensions.MSMQInput;
import com.sun.jbi.msmqbc.extensions.MSMQOutput;
import com.sun.jbi.msmqbc.extensions.MSMQMessage;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;

/**
 * Class to hold message exchange context for JMS message reply
 * 
 * @author Sun Microsystems
 */
public class ReplyContext {

    private MSMQMessage msmqMessage;

    private MSMQOperation msmqOp;

    private QName bindingOpQName;

    private MSMQInput msmqIn;

    private MSMQOutput msmqOut;

    public ReplyContext(MSMQMessage msmqMessage, QName bindingOpQName, MSMQOperation msmqOp, MSMQInput msmqIn,
            MSMQOutput msmqOut) {
        this.msmqMessage = msmqMessage;
        this.bindingOpQName = bindingOpQName;
        this.msmqOp = msmqOp;
        this.msmqIn = msmqIn;
        this.msmqOut = msmqOut;
    }

    public MSMQMessage getMSMQMessage() {
        return msmqMessage;
    }

    public QName getBindingOperationQName() {
        return bindingOpQName;
    }

    public MSMQOperation getMSMQOperation() {
        return msmqOp;
    }

    public MSMQInput getMSMQInput() {
        return msmqIn;
    }

    public MSMQOutput getMSMQOutput() {
        return msmqOut;
    }

}
