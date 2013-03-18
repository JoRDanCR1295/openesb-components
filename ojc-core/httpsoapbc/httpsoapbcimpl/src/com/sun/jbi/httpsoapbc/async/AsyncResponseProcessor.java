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
 *
 * @author Alexander Lomov
 *
 * Copyright 2011 Open-ESB Community.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.httpsoapbc.async;

import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.HttpNormalizer;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.NormalizedMessageProperties;
import com.sun.jbi.httpsoapbc.SoapNormalizer;
import com.sun.jbi.httpsoapbc.SoapNormalizerImpl;
import com.sun.jbi.httpsoapbc.util.TransactionsUtil;
import com.sun.jbi.httpsoapbc.util.Util;
import java.util.Iterator;
import java.util.logging.Logger;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.Source;
import javax.xml.ws.soap.SOAPFaultException;
import net.java.hulp.measure.Probe;
import com.sun.jbi.internationalization.Messages;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.activation.DataSource;

public class AsyncResponseProcessor<T> implements Runnable {

    private T result;
    private AsyncRequestContext context;
    private boolean txResumed;
    private final Logger logger = Logger.getLogger(AsyncResponseProcessor.class.getName());
    private static final String SERVER = "Server";
    private static final String CLIENT = "Client";

    public AsyncResponseProcessor(T operationResult, AsyncRequestContext context) {
        this.result = operationResult;
        this.context = context;
        this.txResumed = false;
    }

    public void run() {
        try {
            resumeTransaction();

            if (result instanceof SOAPFaultException) {
                context.getEndpoint().getEndpointStatus().incrementReceivedErrors();
                replyFault((SOAPFaultException) result);
            } else if (result instanceof Throwable) {
                context.getEndpoint().getEndpointStatus().incrementReceivedErrors();
                replyError((Throwable) result);
            } else if (result instanceof SOAPMessage) {
                context.getEndpoint().getEndpointStatus().incrementReceivedReplies();
                replySOAP((SOAPMessage) result);
            } else if (result instanceof Source) {
                context.getEndpoint().getEndpointStatus().incrementReceivedReplies();
                replyHTTP((Source) result);
            } else if (result instanceof DataSource) {
                context.getEndpoint().getEndpointStatus().incrementReceivedReplies();
                replyHTTP((DataSource) result);
            } else {
                final String err = Messages.getMessages(AsyncResponseProcessor.class).getString("HTTPBC-E1001");
                logger.warning(err);
                return;
            }
            try {
                context.getChannel().send(context.getInout());
            } catch (MessagingException ex) {
                final String err = Messages.getMessages(AsyncResponseProcessor.class).getString("HTTPBC-E1002");
                logger.warning(err);
            }

        } finally {
            suspendTransaction();
        }

    }

    private void replyFault(SOAPFaultException faultEx) {
        Fault f = null;
        final SOAPFault soapFault = faultEx.getFault();
        try {            
            f = constructFault(soapFault);
        } catch (SOAPException se) {
            logger.warning(se.getLocalizedMessage());
            setExchangeFaulty(se, SERVER, "", se.getLocalizedMessage());
        } catch (MessagingException me) {
            logger.warning(me.getLocalizedMessage());
            setExchangeFaulty(me, SERVER, "", me.getLocalizedMessage());
        }

        if (f != null) {
            try {
                InOut inout = context.getInout();
                inout.setFault(f);
                setMessageExchangeSOAPFaultProperties(f, soapFault);
            } catch (MessagingException me) {
                logger.warning(me.getLocalizedMessage());
                setExchangeFaulty(me, SERVER, "", me.getLocalizedMessage());
            }
        } else {
            String errMsg = faultEx.getLocalizedMessage();
            SOAPFault sf = faultEx.getFault();
            setExchangeFaulty(faultEx, sf.getFaultCode(), sf.getFaultActor(), errMsg);
        }
    }

    private void replyFault(SOAPFault soapFault) {
        Fault f = null;

        try {
            f = constructFault(soapFault);
        } catch (SOAPException se) {
            logger.warning(se.getLocalizedMessage());
            setExchangeFaulty(se, SERVER, "", se.getLocalizedMessage());
        } catch (MessagingException me) {
            logger.warning(me.getLocalizedMessage());
            setExchangeFaulty(me, SERVER, "", me.getLocalizedMessage());
        }

        if (f != null) {
            try {
                InOut inout = context.getInout();
                inout.setFault(f);
                setMessageExchangeSOAPFaultProperties(f, soapFault);

            } catch (MessagingException me) {
                logger.warning(me.getLocalizedMessage());
                setExchangeFaulty(me, SERVER, "", me.getLocalizedMessage());
            }
        } else {
            String errMsg = soapFault.getFaultString();
            setExchangeFaulty(new RuntimeException(errMsg), soapFault.getFaultCode(), soapFault.getFaultActor(), errMsg);
        }

    }

    private void setMessageExchangeSOAPFaultProperties(NormalizedMessage msg, SOAPFault soapFault) {
        msg.setProperty(NormalizedMessageProperties.SOAP_FAULTCODE_PROPERTY, soapFault.getFaultCode());
        msg.setProperty(NormalizedMessageProperties.SOAP_FAULTSTRING_PROPERTY, soapFault.getFaultString());
        msg.setProperty(NormalizedMessageProperties.SOAP_FAULTACTOR_PROPERTY, soapFault.getFaultActor());
    }

    private Fault constructFault(SOAPFault soapFault) throws SOAPException, MessagingException {

        Probe normalizationMeasurement = startNormalizationMeasurement();
        SoapNormalizer normalizer = new SoapNormalizerImpl();
        Fault fault = null;

        fault = normalizer.normalizeFault(soapFault, context.getInout(), context.getMetadata(), true);

        if (normalizationMeasurement != null) {
            normalizationMeasurement.end();
        }

        return fault;

    }

    private void replyError(Throwable t) {
        setExchangeFaulty((Exception) t, CLIENT, "", t.getLocalizedMessage());
    }

    private void replySOAP(SOAPMessage response) {
        Probe measure = null;
        try {
            if (response.getSOAPBody().hasFault()) {
                replyFault(response.getSOAPBody().getFault());
                return;
            }
            SoapNormalizer norm = new SoapNormalizerImpl();
            measure = startNormalizationMeasurement();
            norm.setPropagateSoapHeader(context.getEndpoint().getPropagateSoapHeader());
            InOut inout = context.getInout();
            NormalizedMessage msgOut = norm.normalize(response, inout, context.getMetadata(), false, null);
            
            setReponseHeaders(msgOut, context.getResponseHeaders());

            inout.setOutMessage(msgOut);
            context.getEndpoint().getEndpointStatus().incrementSentReplies();
        } catch (SOAPException ex) {
            logger.warning(ex.getLocalizedMessage());
            setExchangeFaulty(ex, SERVER, "", ex.getLocalizedMessage());
        } catch (MessagingException me) {
            logger.warning(me.getLocalizedMessage());
            setExchangeFaulty(me, CLIENT, "", me.getLocalizedMessage());
        } finally {
            if (measure != null) {
                measure.end();
            }
        }
    }

    private void replyHTTP(Source reponse) {
        Probe measure = null;
        try {
            HttpNormalizer norm = new HttpNormalizer();
            measure = startNormalizationMeasurement();

            NormalizedMessage outMsg = norm.normalize(reponse, context.getInout(), context.getMetadata(), false);
            setReponseHeaders(outMsg, context.getResponseHeaders());
            context.getInout().setOutMessage(outMsg);
        } catch (MessagingException me) {
            logger.warning(me.getLocalizedMessage());
            setExchangeFaulty(me, CLIENT, "", me.getLocalizedMessage());
        } finally {
            if (measure != null) {
                measure.end();
            }
        }

    }

    private void replyHTTP(DataSource reponse) {
        Probe measure = null;
        try {
            HttpNormalizer norm = new HttpNormalizer();
            measure = startNormalizationMeasurement();

            NormalizedMessage outMsg = norm.normalize(reponse, context.getInout(), context.getMetadata(), false);
            setReponseHeaders(outMsg, context.getResponseHeaders());
            context.getInout().setOutMessage(outMsg);
        } catch (MessagingException me) {
            logger.warning(me.getLocalizedMessage());
            setExchangeFaulty(me, CLIENT, "", me.getLocalizedMessage());
        } finally {
            if (measure != null) {
                measure.end();
            }
        }

    }

    private void resumeTransaction() {
        javax.transaction.Transaction tx = (Transaction) context.getInout().getProperty(context.getInout().JTA_TRANSACTION_PROPERTY_NAME);
        if (tx != null) {
            TransactionsUtil.resumeTransaction(tx);
        }
    }

    private void suspendTransaction() {
        try {
            if (txResumed) {
                TransactionsUtil.suspendTransaction();
            }
        } catch (SystemException ex) {/* I hope noone uses misterious XA*/

        }
    }

    private void setExchangeFaulty(Exception ex, final String faultCode, final String faultActor, final String detail) {
        InOut exchange = context.getInout();
        exchange.setError(ex);
        try {
            exchange.setStatus(ExchangeStatus.ERROR);
        } catch (MessagingException e) {
        }
        exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTCODE, faultCode);
        exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTSTRING, ex != null ? ex.getLocalizedMessage() : detail);
        exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTACTOR, (faultActor == null || faultActor.equals("")) ? "sun-http-binding" : faultActor);
        exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTDETAIL, detail);

        javax.transaction.Transaction tx = (Transaction) context.getInout().getProperty(context.getInout().JTA_TRANSACTION_PROPERTY_NAME);
        if (tx != null) {
            try {
                tx.setRollbackOnly();
            } catch (IllegalStateException ex1) {
            } catch (SystemException ex1) {
            }
        }
    }

    private void setExchangeFaulty(Exception ex, SOAPFault fault) {
        setExchangeFaulty(ex, fault.getFaultCode(), fault.getFaultActor(), buildFaultDetail(fault));
    }

    private String buildFaultDetail(SOAPFault fault) {
        StringBuffer buf = new StringBuffer();
        javax.xml.soap.Detail details = fault.getDetail();
        if (details != null) {
            Iterator i = details.getDetailEntries();
            while (i.hasNext()) {
                DetailEntry detailEntry = (DetailEntry) i.next();
                try {
                    buf.append(Util.toXml(detailEntry, "UTF-8", true));
                } catch (Exception e) {
                    break;
                }
            }
            return buf.toString();
        }
        return "";
    }

    private Probe startNormalizationMeasurement() {
        Probe normalizationMeasurement = Probe.info(getClass(),
                context.getEndpoint().getUniqueName(),
                HttpSoapBindingLifeCycle.PERF_CAT_NORMALIZATION);
        return normalizationMeasurement;

    }

    private void setReponseHeaders(NormalizedMessage message, Map<String, List<String>> headers){
        Map<String, String> msgHeaders = new HashMap<String, String>(headers.size());
        if (headers != null && message != null){

            for (Map.Entry<String, List<String>> header : headers.entrySet()){
                final String headerName = header.getKey();
                final List<String> headerTemp = header.getValue();
                if (headerName == null || headerName.equals(""))
                    continue;
                final String headerValue = headerTemp.isEmpty() ? "" :
                    headerTemp.get(0);
                msgHeaders.put(headerName.toLowerCase(), headerValue);
            }

            message.setProperty(NormalizedMessageProperties.RESPONSE_HTTP_HEADERS_PROPERTY, msgHeaders);

        }
    }
}
