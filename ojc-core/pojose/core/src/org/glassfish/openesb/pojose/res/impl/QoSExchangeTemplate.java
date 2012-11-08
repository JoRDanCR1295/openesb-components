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
 * @(#)QoSExchangeTemplate.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.res.impl;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.ExchangeTemplates;
import java.lang.String;
import java.util.Iterator;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

/**
 * Not thread safe, suppose to be called by one thread at a time.
 *
 * @author gpatil
 */
public class QoSExchangeTemplate implements ExchangeTemplates{ 
    protected String msgId;
    protected boolean oneWay = false;
    protected Source src;
    protected QName opName;
    protected ServiceEndpoint se;
    protected MessageExchange origME;
    protected MessageExchangeFactory mef;
    protected MessageExchange currME;
    protected NormalizedMessage origNM = null;

    public QoSExchangeTemplate(MessageExchangeFactory mef, MessageExchange omex) {
        this.mef = mef;
        this.origME = omex;
        if (this.origME instanceof InOnly){
            this.oneWay = true;
        }
        
        this.msgId = (String) origME.getProperty(ServiceQuality.MESSAGE_ID);
        if ((msgId == null) || msgId.trim().equals("")){
            this.msgId = origME.getExchangeId();
        }
        this.opName = origME.getOperation();
        this.se = origME.getEndpoint();
        if (oneWay) {
            origNM = ((InOnly)origME).getInMessage();
        } else {
            origNM = ((InOut)origME).getInMessage();
        }

        this.src = origNM.getContent();
    }


//    public QoSExchangeTemplate(MessageExchangeFactory mef, ServiceEndpoint se, Source src) {
//        this.mef = mef;
//        this.src = src;
//    }

    public MessageExchange createExchange() throws MessagingException {
        if (oneWay) {
            currME = mef.createInOnlyExchange();
        } else {
            currME = mef.createInOutExchange();
        }

        currME.setProperty(ServiceQuality.MESSAGE_ID, msgId);
        currME.setOperation(opName);
        currME.setEndpoint(se);

        //populate MEx properties if they are set
        Iterator<String> itr  = origME.getPropertyNames().iterator();
        while (itr.hasNext()){
            String k = itr.next();
            currME.setProperty(k, origME.getProperty(k));
        }

        //set input
        NormalizedMessage currNM = currME.createMessage();
        currNM.setContent(src);
        if (oneWay) {
            ((InOnly)currME).setInMessage(currNM);
        } else {
            ((InOut)currME).setInMessage(currNM);
        }

        if (oneWay) {
            origNM = ((InOnly)origME).getInMessage();
        } else {
            origNM = ((InOut)origME).getInMessage();
        }

        //populate N MSG properties if they are set
        itr = origNM.getPropertyNames().iterator();
        while (itr.hasNext()){
            String k = itr.next();
            currNM.setProperty(k, origNM.getProperty(k));
        }

        return currME;
    }

    public String getUniqueId() {
        return msgId;
    }

}
