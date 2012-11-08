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
 * @(#)WebServiceInvocationOnEsb.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.Request;
import com.sun.jbi.engine.iep.core.runtime.operator.Response;
import com.sun.jbi.engine.iep.core.runtime.operator.WebServiceInvocation;
import com.sun.jbi.engine.iep.core.runtime.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 *
 * @author Bing Lu
 */
public class IEPSEWebServiceInvocation implements WebServiceInvocation {

    private static final Messages mMessages = Messages.getMessages(IEPSEWebServiceInvocation.class);
    private static Logger mLogger = Messages.getLogger(IEPSEWebServiceInvocation.class);
    private Request mRequest;
    private Response mResponse;
    private ExtendedComponentContext mExtendedContext;

    public IEPSEWebServiceInvocation(Request request, ExtendedComponentContext extendedContext) {
        mRequest = request;
        mExtendedContext = extendedContext;
    }

    public void invoke() {
        Source input = new DOMSource(mRequest.getDoc());
        String msgId = mRequest.getId();
        ServiceEndpoint endpoint = getServiceEndpoint();
        String operation = getOperation();
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEOutOnlyThread.msgId_service_operation",
                    new Object[]{msgId, endpoint.getServiceName(), operation}));
        }
        try {
            QName qualOpName = new QName(endpoint.getServiceName().getNamespaceURI(), operation);
            InOut inOut = mExtendedContext.mTempMEFactory.createInOutExchange();

            // Create new message and populate its content
            NormalizedMessage msgIn = inOut.createMessage();
            msgIn.setContent(input);

            // Set message as out reference in exchange
            inOut.setInMessage(msgIn);

            inOut.setEndpoint(endpoint);
            inOut.setOperation(qualOpName);

            // Initiate the message exchange
            mExtendedContext.mTempChannel.sendSync(inOut);
            
            NormalizedMessage msgOut = inOut.getOutMessage();
            Document doc = XmlUtil.createDocumentFromSource(msgOut.getContent());
            Element objNode = WrapperUtil.getPartElement(doc);
            mResponse = new Response(mRequest, objNode);
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, 
                mMessages.getString("IEPSEWebServiceInvocation.Fail_to_invoke_service",
                    new Object[]{msgId, endpoint.getServiceName(), operation}),
                e);
        }
    }

    public Response getResponse() {
        return mResponse;
    }

    private String getOperation() {
        return ((Operator) mRequest.getInvoker()).getOperation();
    }

    private ServiceEndpoint getServiceEndpoint() {
        Operator op = (Operator) mRequest.getInvoker();
        String opName = op.getOperation();
        String serviceNamespaceURI = op.getPlan().getInstanceId();
        String serviceLocalPart = "RequestReplyPl_" + opName;
        QName service = new QName(serviceNamespaceURI, serviceLocalPart);

        String endpointName = "RequestReplyRn_" + opName;

        return mExtendedContext.getComponentContext().getEndpoint(service, endpointName);
    }
}
