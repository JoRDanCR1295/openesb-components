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
 * @(#)DTELSEInOutThread.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.dtel;

import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.InOnly;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.wsdl.Output;
import javax.wsdl.Message;
import javax.wsdl.Part;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


public class DTELSEInOutThread extends Thread {
    private static final Logger mLogger = Logger.getLogger(DTELSEInOutThread.class.getName());
    private static String OutputPartName = "output";
    private static String OutputPartNode = "ns0:outElement";

    private DtelMapEntryTable mDtelMapEntryTable;
    private MessageExchangeFactory mMessageExchangeFactory;
    private DeliveryChannel mChannel;
    private boolean mRunFlag;

    /** Map of invocation requests that are waiting for a response.  Key is message exchange id
     * value is a InOut message
     */
    private Map mCorrelationMap;

    /** Creates a new instance of DTELSEInOutThread */
    public DTELSEInOutThread(DeliveryChannel channel, DtelMapEntryTable dtelMapEntryTable) {
        try {
            mRunFlag = true;
            mChannel = channel;
            mMessageExchangeFactory = mChannel.createExchangeFactory();
            mDtelMapEntryTable = dtelMapEntryTable;
            mCorrelationMap = new HashMap();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    private static DocumentBuilder mBuilder = null;
    private static Document newDocument() throws ParserConfigurationException {
        if (mBuilder == null) {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            mBuilder = factory.newDocumentBuilder();
        }
        return mBuilder.newDocument();
    }

    private void process(InOut inOut) {
        if(inOut == null) {
            return;
        }
        QName operation = inOut.getOperation();
        QName service = inOut.getEndpoint().getServiceName();
        DtelMapEntry dtelMapEntry = mDtelMapEntryTable.findDtelEntry(operation, service);
        if (dtelMapEntry == null) {
            String errMsg = "Cannot find dtel entry for operation=" + operation + ", service=" + service;
            mLogger.info(errMsg);
            // FIX ME: inOut.setStatus(ExchangeStatus.ERROR)?
            return;
        }
        if (!dtelMapEntry.isStarted()) {
            // The containing service unit is not started
            return;
        }
        try {
            NormalizedMessage request = inOut.getInMessage();
            if(request == null) {
                return;
            }

            // Calculate processed content
            Source content = request.getContent();
            Source processedContent = null;
            if (content instanceof DOMSource) {
                DOMSource xmlSource = (DOMSource)content;
                DOMResult xmlResult = new DOMResult(newDocument());

                Node rn = xmlSource.getNode();
                if (rn instanceof Document) {
                   mLogger.info("InputXML: " + XmlUtil.toXml(((Document) rn).getDocumentElement(), "UTF-8", false));
                } else if (rn instanceof Element) {
                    mLogger.info("InputXML: " + XmlUtil.toXml(rn, "UTF-8", false));
                }

                DTELEngine dtelEngine = dtelMapEntry.getDTELEngine();

                // processedContent = new DOMSource();
                //String outp = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><dtelOperation><output><ns0:outElement xmlns:ns0=\"untitled_dtel\"><Risk>0.0</Risk></ns0:outElement></output></dtelOperation>";
                //String outp = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><dtelOperation><output><outElement><Risk>0.0</Risk></outElement></output></dtelOperation>";
                processedContent = new DOMSource(XmlUtil.createDocument(true));

                dtelEngine.execute(xmlSource, (DOMSource) processedContent);
                dtelMapEntry.getEndpointStatus().incrementReceivedRequests();
            } else {
                String errMsg = "Unknown XML Source type: " + content.getClass().getName();
                mLogger.info(errMsg);
                dtelMapEntry.getEndpointStatus().incrementReceivedErrors();
                throw new Exception(errMsg);
            }
            // Send processed content;
            if (dtelMapEntry.getType().equals(DtelMapEntry.REQUEST_REPLY_SERVICE)) {

                Element outRoot = ((Document)((DOMSource) processedContent).getNode()).getDocumentElement();
                NodeList nodeList = outRoot.getElementsByTagName(OutputPartNode);
                Element partNode = (Element) nodeList.item(0);
                Output output = dtelMapEntry.getOutput();
                WrapperBuilder builder = HelperFactory.createBuilder();
                builder.initialize(newDocument(), output.getMessage(), output.getName());
                builder.addPart(OutputPartName, partNode);
                Document normalizedDoc = builder.getResult();
                mLogger.info("NormOut: " + XmlUtil.toXml(normalizedDoc, "UTF-8", false));

                NormalizedMessage response = inOut.createMessage();
                response.setContent(new DOMSource(normalizedDoc)); // processedContent);
                inOut.setOutMessage(response);
                mChannel.send(inOut);
                dtelMapEntry.getEndpointStatus().incrementReceivedDones();
                return;
            }

            mLogger.info("Service type: " + dtelMapEntry.getType() + " doesn't support InOut");

        } catch(Throwable t) {
            String msg = "caught unexpected exception";
            mLogger.log(Level.SEVERE, msg, t);
            try {
                inOut.setStatus(ExchangeStatus.ERROR);
                mChannel.send(inOut);
            } catch (MessagingException ex) {
                mLogger.log(Level.SEVERE, "unable to set error status on inOut", ex);
            }
        }
    }

    void showNodes(Element e, int level) {
        NodeList nodeList = e.getChildNodes();
        for (int i = 0; i < nodeList.getLength(); i++) {
            Node node = nodeList.item(i);
            mLogger.info(level+" Node["+i+"]: " + node);
            if (node instanceof Element) {
               showNodes( (Element) node, level+1);
            }
        }
    }

    public void run() {
        mLogger.info("Started DTEL service engine in-out thread");
        while (mRunFlag) {
            // when receiving an IN-OUT, IN-ONLY, ROBUST-IN message, it means that BC is invoking SE.
            // when receiving an OUT-IN message, it means that the SE has invoked out, and this message exchange
            // is the response.
            // There shouldn't be OUT-ONLY or ROBUST-OUT because only BC are suppose to receive such messages
            if (mChannel == null) {
                continue;
            }
            try {
                MessageExchange msgEx = mChannel.accept();
                URI pattern = msgEx.getPattern();
                mLogger.info("Status: " + msgEx.getStatus().toString() + ", Pattern for exchange Id " + msgEx.getExchangeId() + " is " + pattern);
                if (msgEx.getStatus().equals(ExchangeStatus.ACTIVE)) {
                    String pat = pattern.toString().trim();
                    if(pat.equals("http://www.w3.org/2004/08/wsdl/in-out".trim())) {
                        mLogger.info("Received in-out message " + msgEx.getExchangeId());
                        process((InOut)msgEx);
                    } else if(pat.equals("http://www.w3.org/2004/08/wsdl/in-only".trim())) {
                        mLogger.info("In-only is not supported " + msgEx.getExchangeId());
                        msgEx.setStatus(ExchangeStatus.ERROR);
                        mChannel.send(msgEx);
                    } else if(pat.equals("http://www.w3.org/2004/08/wsdl/robust-in-only".trim())) {
                        mLogger.info("Robust in-only is not supported " + msgEx.getExchangeId());
                        msgEx.setStatus(ExchangeStatus.ERROR);
                        mChannel.send(msgEx);
                    } else if(pat.equals("http://www.w3.org/2004/08/wsdl/out-in".trim())) {
                        mLogger.info("out-in is not supported " + msgEx.getExchangeId());
                        msgEx.setStatus(ExchangeStatus.ERROR);
                        mChannel.send(msgEx);
                    } else if(pat.equals("http://www.w3.org/2004/08/wsdl/out-only".trim())) {
                        mLogger.info("Out-only is not supported " + msgEx.getExchangeId());
                        msgEx.setStatus(ExchangeStatus.ERROR);
                        mChannel.send(msgEx);
                    } else if(pat.equals("http://www.w3.org/2004/08/wsdl/robust-out-only".trim())) {
                        mLogger.info("Robust out-only is not supported " + msgEx.getExchangeId());
                        msgEx.setStatus(ExchangeStatus.ERROR);
                        mChannel.send(msgEx);
                    } else {
                        mLogger.severe("Received invalid pattern info " + msgEx.getExchangeId());
                        msgEx.setStatus(ExchangeStatus.ERROR);
                        mChannel.send(msgEx);
                    }
                }
            } catch(Exception e) {
                if (mRunFlag) {
                    e.printStackTrace();
                    mLogger.log(Level.SEVERE, "caught exception acception message on channel", e);
                }
            }
        }
        mLogger.info("DTEL service engine in-out thread finished");
    }

    public void cease() {
        try {
            mLogger.info("Ceasing DTEL service engine in-out thread");
            mRunFlag = false;
            mLogger.info("DTEL service engine in-out thread ceased");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
