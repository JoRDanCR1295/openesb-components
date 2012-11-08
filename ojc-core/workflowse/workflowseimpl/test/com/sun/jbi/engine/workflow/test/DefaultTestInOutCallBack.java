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
 * @(#)DefaultTestInOutCallBack.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.test;

import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.process.ConsumerCallBack;
import com.sun.jbi.engine.workflow.process.InOutCallBack;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import javax.xml.transform.dom.DOMSource;

/**
 * 
 * 
 */
public class DefaultTestInOutCallBack implements InOutCallBack,
        ConsumerCallBack {

    private boolean gotFault = false;

    private boolean gotNotified = false;

    public boolean isFaulted() {
        return this.gotFault;
    }

    public boolean isNotified() {
        return this.gotNotified;
    }

    public void  onFault(String meId, DOMSource fault) {
        // TODO Auto-generated method stub
        org.w3c.dom.Node el = fault.getNode();
        String faultStr = null;
        if (el != null) {
            faultStr = XmlUtil.toXml(el, "UTF-8", false);
        }
        System.out.println("onFault, msgId :" + meId + " xml: " + faultStr);

        gotFault = true;
    }

    public void onReply(String meId, DOMSource reply) {
        org.w3c.dom.Node el = reply.getNode();
        String replyStr = null;
        if (el != null) {
            replyStr = XmlUtil.toXml(el, "UTF-8", false);
        }
        System.out.println("onReply, msgId :" + meId + " xml: " + replyStr);
    }

    public void onTimeout(String meId, DOMSource timeout) {
        org.w3c.dom.Node el = timeout.getNode();
        String timeoutStr = null;
        if (el != null) {
            timeoutStr = XmlUtil.toXml(el, "UTF-8", false);
        }
        System.out.println("onTimeout, msgId :" + meId + " xml: " + timeoutStr);
    }

    public void onNotify(WorkflowMapEntry entry, DOMSource reply) {
        org.w3c.dom.Node el = reply.getNode();
        String notifyStr = null;
        if (el != null) {
            notifyStr = XmlUtil.toXml(el, "UTF-8", false);
        }
        System.out.println("onNotify, entry :" + entry + " xml: " + notifyStr);

    }

}
