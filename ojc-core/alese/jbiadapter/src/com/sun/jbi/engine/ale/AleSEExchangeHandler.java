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
 * @(#)AleSEExchangeHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.ale;

import java.sql.Timestamp;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.Payload;
import com.sun.jbi.common.tale.core.domain.SourceInfo;
import com.sun.jbi.common.tale.core.domain.Payload.Encode;
import com.sun.jbi.common.tale.core.domain.Payload.EncodeMode;
import com.sun.jbi.common.tale.core.domain.Payload.PayloadType;
import com.sun.jbi.common.tale.core.domain.service.TaleService;
import com.sun.jbi.common.tale.core.util.TaleException;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler;
import com.sun.jbi.component.toolkit.util.PatternRoleStatus;

/**
 * Handles exchanges for AleSE.
 * @author Kevan Simpson
 */
public class AleSEExchangeHandler extends AbstractExchangeHandler {
    /** Constructs an {@link ExchangeHandler} for AleSE. */
    public AleSEExchangeHandler(ComponentContext ctx) {
        super(ctx);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#handleExchange(javax.jbi.messaging.MessageExchange) */
    public void handleExchange(MessageExchange mex) throws JBIException {
        try {
            PatternRoleStatus prs = PatternRoleStatus.valueOf(mex);
            switch (prs) {
                case IN_ONLY_PROVIDER_ACTIVE: { // incoming msg to provision
                    Endpoint<TaleService> endpt = findProvisioningEndpoint(mex);
                    TaleService srvc = endpt.getServiceDef(mex.getOperation().getLocalPart());
                    // TODO null check for TaleService def
                    srvc.execute(createRequest(mex));
                    mex.setStatus(ExchangeStatus.DONE);
                    getContext().getMessagingChannel().send(mex);
                    break;
                }
                default: {
                    // for now, we only support incoming one-way ALE messages
                    throw new JBIException(I18n.loc(
                            "ALESE-6002: ALE Service Engine does not support the pattern: {0}", 
                            prs));
                }
            }
        }
        catch (Exception e) {
            String i18n = I18n.loc("ALESE-6003: Failed to handle message exchange ({0}): {1} : {2}",
                                   mex.getExchangeId(), e.getMessage(), e.getCause());
            log().log(Level.WARNING, i18n, e);
            // ExchangeUtil.setErrorData(mex, error, FaultCode.Server, detail, actor);
            // throw new JBIException(i18n, e);
            mex.setError(new JBIException(i18n, e));
            getContext().getMessagingChannel().send(mex);
        }
    }
    
    protected TaleRequest createRequest(MessageExchange mex) throws TaleException {
        try {
            // unwrap part elements
            Element[] parts = ExchangeUtil.extractParts(mex);
            // first part is source info
            SourceInfo info = deriveSourceInfo(parts[0]);
            // second part is request data
            int code = Integer.parseInt(deriveTextContent(parts[1], "code"));
            String details = deriveTextContent(parts[1], "details");
            String display = deriveTextContent(parts[1], "displayMessage");
            // last part is payload (optional)
            Payload payload = (parts.length > 2) ? derivePayload(parts[2]) : null;
            
            return new TaleRequest(info, code, details, display, payload);
        }
        catch (Exception e) {
            String err = I18n.loc(
                    "ALESE-6004: Failed to create request from message exchange: {0}", 
                    e.getMessage());
            log().warning(err);
            throw new TaleException(err, e);
        }
    }
    
    private Payload derivePayload(Element elem) {
        Payload payload = new Payload();
        payload.setEncodeFlag(Encode.valueOf(
                deriveTextContent(elem, "EncodeFlag")));
        String encodeMode = deriveTextContent(elem, "EncodeMode");
        if (!Util.isEmpty(encodeMode)) {
            payload.setEncodeMode(EncodeMode.valueOf(encodeMode));
        }
        payload.setPayloadType(PayloadType.valueOf(
                deriveTextContent(elem, "PayloadType")));
        payload.setPayloadMessage(
                deriveTextContent(elem, "OriginalMessage"));
        //payload.setTransformedMessage(
                //deriveTextContent(elem, "TransformedMessage"));

        return payload;
    }
    
    private SourceInfo deriveSourceInfo(Element elem) {
        SourceInfo info = new SourceInfo();
        info.setProjectName(deriveTextContent(elem, "ProjectName"));
        info.setApplicationType(deriveTextContent(elem, "ApplicationType"));
        info.setApplicationName(deriveTextContent(elem, "ApplicationName"));
        info.setServiceName(deriveTextContent(elem, "ServiceName"));
        info.setModuleName(deriveTextContent(elem, "ModuleName"));
        info.setUnitName(deriveTextContent(elem, "UnitName"));
        // TODO what is the correct value to set?
        info.setDateTimeStamp(new Timestamp(System.currentTimeMillis()));
//        info.setDateTimeStamp(deriveTextContent(elem, "DateTimeStamp"));
        info.setAppMessageID(deriveTextContent(elem, "MessageID"));

        return info;
    }
    
    private String deriveTextContent(Element parent, String child) {
        NodeList nodes = parent.getElementsByTagNameNS(TaleService.ALE_TYPE_NS, child);
        if (nodes == null || nodes.getLength() < 1) {
            return null;
        }
        else {
            return ((Element) nodes.item(0)).getTextContent();
        }
    }
}
