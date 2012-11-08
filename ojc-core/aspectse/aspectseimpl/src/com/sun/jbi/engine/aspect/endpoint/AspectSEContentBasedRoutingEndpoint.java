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
 * @(#)AspectSEContentBasedRoutingEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import java.util.Iterator;
import java.util.List;

import javax.xml.transform.Source;

import javax.jbi.JBIException;
import javax.management.Notification;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectSEContentBasedRoutingEndpointHandler;
import com.sun.jbi.engine.aspect.endpoint.support.CacheSEUtil;

//import com.sun.jbi.engine.aspect.endpoint.support.Ruleset;

/**
 * <p>
 * <b>Content-Based Routing Aspect</b> The Content-Based Routing Aspect is used
 * to forward a message to the correct destination based on the content of the
 * message payload. For example, a typical scenario for this may be that on
 * receipt of a purchase order, the message needs to be routed to the
 * appropriate order management system, based on the type of item that has been
 * ordered (which will be obtained by parsing an XPath expression in the message
 * payload). The routing will happen based on a configurable Rule Set which can
 * be configured from the Web Console.
 * <p>
 * This pattern ensures the Content-Based Routing policy acts between the client
 * and the multiple services that are involved to service the operation.
 * Therefore, if this aspect is configured, all messages flowing between the
 * client and the service providers flow through the Content-Based Routing
 * engine core.
 * <p>
 * Composite Apps requiring use of this pattern will have to employ either the
 * <b>Filter Request/Reply</b> or the <b>Filter One-Way</b> exchange pattern.
 *
 *
 * @author Sujit Biswas
 *
 */
public class AspectSEContentBasedRoutingEndpoint extends AspectSEEndpoint {

    //private Ruleset ruleset;

    public AspectSEContentBasedRoutingEndpoint(EndpointInfo info) {
        super(info);
        setHandler(new AspectSEContentBasedRoutingEndpointHandler(this));
        // TODO Auto-generated constructor stub
    }



    @Override
    public void init() {
        getHandler().parse();
    }

    @Override
    public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
    throws JBIException {
        StringBuffer exMsg = new StringBuffer();
        Source source = inOnly.getInMessage().getContent();
        List<AspectSEEndpoint> invokes = getRuleset().evaluate(
                CacheSEUtil.convertToDOMSource(source, ctx));
        Iterator it = invokes.iterator();
        while(it.hasNext()) {
            AspectSEEndpoint endpt = (AspectSEEndpoint)it.next();
            setInvoke(endpt);
            try {
                super.handleFilterInvoke(inOnly, ctx);
            } catch (JBIException ex) {
                // collect the exception msg and throw as a single exception.
                exMsg.append(ex.getMessage() + ";");
            }
        }

        // throw JBIException if exMsg has some msg.
        if(exMsg.length() != 0) {
            throw new JBIException(exMsg.toString());
        }
    }

    @Override
    public void handleFilterInvoke(CRLInOut inOut, ExchangeContext ctx)
    throws JBIException {
        Source source = inOut.getInMessage().getContent();
        List<AspectSEEndpoint> invokes = getRuleset().evaluate(
                CacheSEUtil.convertToDOMSource(source, ctx));
        Iterator it = invokes.iterator();
        while(it.hasNext()) {
            AspectSEEndpoint endpt = (AspectSEEndpoint)it.next();
            setInvoke(endpt);
            super.handleFilterInvoke(inOut, ctx);
        }
    }

    @Override
    public void handleFilterResponse(CRLInOut inOut, CRLInOut originalInOut, ExchangeContext ctx)
    throws JBIException {

        // TODO Auto-generated method stub
        super.handleFilterResponse(inOut, originalInOut,ctx);
    }

    @Override
    public void registerMbean() {
        // TODO Auto-generated method stub

    }

    public void handleNotification(Notification notification, Object handback) {
        // TODO Auto-generated method stub

    }
}
