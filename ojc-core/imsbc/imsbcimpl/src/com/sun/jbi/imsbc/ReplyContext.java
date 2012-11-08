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

package com.sun.jbi.imsbc;

import javax.xml.namespace.QName;

import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSOutput;
import com.sun.jbi.imsbc.extensions.IMSMessage;
import com.sun.jbi.imsbc.extensions.IMSOperation;

/**
 * Class to hold message exchange context for IMS message reply
 * 
 * @author Sun Microsystems
 */
public class ReplyContext {

    private IMSMessage imsMessage;

    private IMSOperation imsOp;

    private QName bindingOpQName;

    private IMSInput imsIn;

    private IMSOutput imsOut;

    public ReplyContext(IMSMessage imsMessage, QName bindingOpQName, IMSOperation imsOp, IMSInput imsIn,
            IMSOutput imsOut) {
        this.imsMessage = imsMessage;
        this.bindingOpQName = bindingOpQName;
        this.imsOp = imsOp;
        this.imsIn = imsIn;
        this.imsOut = imsOut;
    }

    public IMSMessage getIMSMessage() {
        return imsMessage;
    }

    public QName getBindingOperationQName() {
        return bindingOpQName;
    }

    public IMSOperation getIMSOperation() {
        return imsOp;
    }

    public IMSInput getIMSInput() {
        return imsIn;
    }

    public IMSOutput getIMSOutput() {
        return imsOut;
    }

}
