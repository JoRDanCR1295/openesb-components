/**
 *   sip-binding-component - SIP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.sip.component;

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.nmr.NmrWrapperUtils;
import com.gestalt.jbi.sip.Observer;
import com.gestalt.jbi.sip.SIPObservable;
import com.gestalt.jbi.sip.extensions.SIPOperation;
import com.gestalt.jbi.sip.extensions.SIPOperationInput;
import com.gestalt.sip.utilities.message.SipUtil;
import com.sun.jbi.internationalization.Messages;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.sip.RequestEvent;
import javax.sip.ResponseEvent;
import javax.sip.address.URI;
import javax.sip.header.CSeqHeader;
import javax.sip.header.FromHeader;
import javax.sip.header.ToHeader;
import javax.sip.message.Request;
import javax.sip.message.Response;
import javax.wsdl.*;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.io.StringWriter;


/**
 * @author : csturtz
 */
public class SIPConsumerHandler extends AbstractMessageExchangeHandler
    implements Observer {
    private static final Logger log = Messages.getLogger(SIPConsumerHandler.class);
    private static Messages messages = Messages.getMessages(SIPConsumerHandler.class);

    public SIPConsumerHandler(SIPEndpoint endpoint) {
        super(endpoint);

        String localUri = endpoint.getSipConnection().getRequestGenerator()
                                  .getUserURI();
        SIPObservable.addObserver(localUri, this);
    }

    protected void processError(Exception exception) {
        if (exception != null) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00415.exceptionReceived", exception.getMessage()));
        }
    }

    protected void processDone() {
    }

    protected void processMessage() {
    }

    protected void processFault(Fault fault) {
    }

    /**
     * When the SipServer receives a RequestEvent, this class will be notified
     * and this method called.
     *
     * @param requestEvent
     */
    @SuppressWarnings("unchecked")
    private void sendRequest(RequestEvent requestEvent) {

        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER,"Sending a request to the consumer");
        }

        Request request = requestEvent.getRequest();
        String method = request.getMethod();

        QName operationKey = findOperationKey(SIPOperation.OperationName.receiveRequest);
        NormalizedMessage in = prepareExchange(operationKey);
        SIPOperationInput sipOperationInput = getOperationInput(operationKey);
        QName messageType = getMessageQName(operationKey);

        URI from = SipUtil.getURIFromHeader(request, FromHeader.NAME);
        String remoteUri = SipUtil.buildUserAtHost(from);
        byte[] rawContent = request.getRawContent();
        String content;

        if ((rawContent == null) || (rawContent.length == 0)) {
            content = "";
        } else {
            content = new String(rawContent);
        }

        Definition wsdl = endpoint.getWsdlDefinition();
        Message wsdlMessageDefinition = wsdl.getMessage(messageType);

        NmrWrapperUtils wrapper = new NmrWrapperUtils();
        wrapper.init(messageType, null);

        List<Part> messageParts = (List<Part>) wsdlMessageDefinition.getOrderedParts(null);

        for (Object messagePart : messageParts) {
            Part part = (Part) messagePart;

            if (part.getName().equals(sipOperationInput.getRequestMethod())) {
                wrapper.addPart(null, method);
            } else if (part.getName().equals(sipOperationInput.getRemoteUri())) {
                wrapper.addPart(null, remoteUri);
            } else if (part.getName().equals(sipOperationInput.getContent())) {
                wrapper.addPart(null, content);
            }
        }

        setContentOnMessage(in, wrapper);
        sendInMessage(in);
    }

    /**
     * When the SipServer receives a ResponseEvent, this class will be notified
     * and this method called.
     *
     * Currently, all responses other than those to a REGISTER Request are
     *
     * @param responseEvent
     */
    @SuppressWarnings("unchecked")
    private void sendResponse(ResponseEvent responseEvent) {

        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER,"Sending a response to the consumer");

        }
        Response response = responseEvent.getResponse();
        String requestMethod = ((CSeqHeader) response.getHeader(CSeqHeader.NAME)).getMethod();

        int status = responseEvent.getResponse().getStatusCode();
        String remoteUri = SipUtil.buildUserAtHost(((ToHeader) response
                                                    .getHeader(ToHeader.NAME)).getAddress()
                                                    .getURI());

        if (requestMethod.equals(Request.REGISTER)) {

            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Received a(n)" + status +
                    " response to a REGISTER request.  No notifications will be sent");

            }
            return;
        }

        String content = "";
        byte[] rawcontent = response.getRawContent();

        if (rawcontent != null) {
            content = new String(rawcontent);
        }

        QName operationKey = findOperationKey(SIPOperation.OperationName.receiveResponse);
        NormalizedMessage in = prepareExchange(operationKey);
        SIPOperationInput sipOperationInput = getOperationInput(operationKey);
        QName messageType = getMessageQName(operationKey);

        Definition wsdl = endpoint.getWsdlDefinition();
        Message wsdlMessageDefinition = wsdl.getMessage(messageType);

        NmrWrapperUtils wrapper = new NmrWrapperUtils();
        wrapper.init(messageType, null);

        List<Part> messageParts = (List<Part>) wsdlMessageDefinition.getOrderedParts(null);

        for (Object messagePart : messageParts) {
            Part part = (Part) messagePart;

            if (part.getName().equals(sipOperationInput.getResponseStatus())) {
                wrapper.addPart(null, String.valueOf(status));
            } else if (part.getName()
                               .equals(sipOperationInput.getResponseMethod())) {
                wrapper.addPart(null, requestMethod);
            } else if (part.getName().equals(sipOperationInput.getRemoteUri())) {
                wrapper.addPart(null, remoteUri);
            } else if (part.getName().equals(sipOperationInput.getContent())) {
                wrapper.addPart(null, content);
            }
        }

        setContentOnMessage(in, wrapper);
        sendInMessage(in);
    }

    private NormalizedMessage prepareExchange(QName operationKey) {
        ServiceEndpoint serviceEndpoint = getServiceEndpoint();

        try {
            exchange = channel.createExchangeFactoryForService(serviceEndpoint.getServiceName())
                              .createInOnlyExchange();
        } catch (MessagingException me) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00416.receivedMessagingExceptionErrorWhileTryingToInstantiateMessageExchange"),me);
            sendError(me);
        }

        exchange.setEndpoint(serviceEndpoint);
        exchange.setOperation(operationKey);

        NormalizedMessage in = createInMessage();

        return in;
    }

    private ServiceEndpoint getServiceEndpoint() {
        final QName fullServiceName = endpoint.getServiceName();
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"endpoint.getServiceName(): " + fullServiceName);
        }

        final String endpointName = endpoint.getEndpointName();
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"endpoint.getEndpointName(): " + endpointName);
        }

        return ((AbstractComponentLifeCycle) endpoint.getServiceUnit()
                .getComponent().getLifeCycle()).getComponentContext()
                .getEndpoint(fullServiceName, endpointName);
    }

    private PortType getPortType() {
        Service service = endpoint.getWsdlDefinition()
                                  .getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName())
                                         .getLocalPart());

        return port.getBinding().getPortType();
    }

    private QName getMessageQName(QName operationKey) {
        QName type = null;

        try {
            PortType portType = getPortType();

            Operation op = null;

            for (Object o : portType.getOperations()) {
                Operation myOperation = (Operation) o;

                if (myOperation.getName()
                                   .equalsIgnoreCase(operationKey.toString())) {
                    op = myOperation;
                }
            }

            if (op == null) {
                sendError(new Exception(
                        "Unable to locate the correct operation in the PortType"));
            }

            type = op.getInput().getMessage().getQName();
        } catch (Exception e) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00417.exceptionGettingTheMessageQName"),e);
        }

        return type;
    }

    private QName findOperationKey(SIPOperation.OperationName operationName) {
        QName operationKey = null;

        for (Object o : endpoint.getOperations().keySet()) {
            SIPOperation operation = (SIPOperation) endpoint.getOperations()
                                                            .get(o);

            if (operation.getOperationName().equals(operationName)) {
                operationKey = (QName) o;
            }
        }

        if (operationKey == null) {
            sendError(new Exception("Unable to locate the correct operation"));
        }

        return operationKey;
    }

    private SIPOperationInput getOperationInput(QName operationKey) {
        SIPOperation operation = (SIPOperation) endpoint.getOperations()
                                                        .get(operationKey);

        return ((SIPEndpoint) endpoint).getSIPOperationInput(operation);
    }

    private NormalizedMessage createInMessage() {
        NormalizedMessage in = null;

        try {
            in = exchange.createMessage();

            if (in == null) {
                throw new MessagingException(
                    "Created NormalizedMessage was NULL");
            }
        } catch (MessagingException me) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00419.receivedAMessagingExceptionErrorWhileTryingToInstantiateNormalizedMessage"),me);
            sendError(me);
        }

        return in;
    }

    private void setContentOnMessage(NormalizedMessage nm,
        NmrWrapperUtils wrapper) {
        try {
            DOMSource dSource = new DOMSource(wrapper.getResult());
            nm.setContent(dSource);

            // log normalized message
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(dSource, result);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER, "Normalizing Message: " + result.getWriter().toString());
            }
        } catch (Exception e) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00420.anExceptionWasThrownSettingTheContentOnTheMessage"),e);
            sendError(e);
        }
    }

    private void sendInMessage(NormalizedMessage in) {
        try {
            exchange.setMessage(in, "in");
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE, "Sending Message " + exchange.getExchangeId() + "; pattern is " + exchange.getPattern().toString().trim());
            }
            sendSync(exchange);
        } catch (Exception me) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00421.receivedAnExceptionWhileTryingToInstantiateNormalizedMessage"), me);
            sendError(me);
        }
    }


    public void update(Object arg) {
        if (arg instanceof RequestEvent) {
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER, "Received notification of a RequestEvent");
            }
            sendRequest((RequestEvent) arg);
        }

        if (arg instanceof ResponseEvent) {
            if (log.isLoggable(Level.FINER)){   
                log.log(Level.FINER,"Received notification of a ResponseEvent");
            }
            sendResponse((ResponseEvent) arg);
        }
    }
}
