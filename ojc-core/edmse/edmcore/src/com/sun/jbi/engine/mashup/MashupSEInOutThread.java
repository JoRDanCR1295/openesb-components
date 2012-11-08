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
package com.sun.jbi.engine.mashup;

import com.sun.mashup.engine.QueryContext;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import com.sun.jbi.internationalization.Messages;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.wsdl.Output;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.mashup.mbean.MashupSERuntimeConfiguration;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
//import com.sun.sql.framework.utils.RuntimeAttribute;
import java.io.File;
import java.util.ArrayList;

public class MashupSEInOutThread extends Thread {

    private static final Logger logger = Logger.getLogger(MashupSEInOutThread.class.getName());
    private static final Messages mMessages = Messages.getMessages(MashupSEInOutThread.class);
    private static final String InputPartNode = "inputItem";
    private static final String OutputPartName = "part";
    private static final String OutputPartNode = "outputItem";
    private MashupMapEntryTable mMapEntryTable;
    private MessageExchangeFactory mMessageExchangeFactory;
    private DeliveryChannel mChannel;
    private MashupSERuntimeConfiguration runtimeConfigMBean;
    private boolean mRunFlag;
    private Properties envProps = new Properties();
    private ExecutorService threadpool;
    public static final String FAULTCODE_PROPERTY_NAME = "com.sun.jbi.crl.faultcode";
    public static final String FAULTSTRING_PROPERTY_NAME = "com.sun.jbi.crl.faultstring";
    public static final String FAULTACTOR_PROPERTY_NAME = "com.sun.jbi.crl.faultactor";
    public static final String FAULTDETAIL_PROPERTY_NAME = "com.sun.jbi.crl.faultdetail";
    private final String COMPONENT_NAME = "sun-edm-engine";
    /**
     * Map of invocation requests that are waiting for a response. Key is
     * message exchange id value is a InOut message
     */
    private Map mCorrelationMap;
    private QueryContext queryContext;

    /**
     * Creates a new instance of MashupSEInOutThread
     * 
     * @param runtimeConfigMBean
     */
    public MashupSEInOutThread(MashupSERuntimeConfiguration runtimeConfigMBean,
            DeliveryChannel channel, MashupMapEntryTable mashupMapEntryTable) {
        try {
            mRunFlag = true;
            mChannel = channel;
            mMessageExchangeFactory = mChannel.createExchangeFactory();
            mMapEntryTable = mashupMapEntryTable;
            mCorrelationMap = new HashMap();
            this.runtimeConfigMBean = runtimeConfigMBean;
            this.runtimeConfigMBean.addNotificationListener(listener, null,
                    null);

            threadpool = Executors.newCachedThreadPool();

            // added query context

            queryContext = new QueryContext();
            int pageSize = Integer.parseInt(this.runtimeConfigMBean.getPageSize());
            queryContext.setPageSize(pageSize);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void process(InOut inOut) {
        threadpool.execute(new InOutMessageHandler(inOut));
    }

    /**
     * 
     * This will be called from the executor's thread
     * 
     * @param inOut
     */
    public void handleInOutMessage(InOut inOut) {
        if (inOut == null) {
            return;
        }
        logger.fine(mMessages.getString("EDMSE-F0213.reached_handleInOutMessage"));
        QName operation = inOut.getOperation();

        if (operation != null) {
            logger.fine(mMessages.getString("EDMSE-F0214.operation_Found") + operation.toString());
        } else {
            logger.info(mMessages.getString("EDMSE-I0307.null_Operation"));
        }
        QName service = inOut.getEndpoint().getServiceName();
        if (operation != null) {
            logger.fine(mMessages.getString("EDMSE-F0215.service_Found") + service.toString());
        } else {
            logger.info(mMessages.getString("EDMSE-I0308.null_Service"));
        }

        MashupMapEntry mashupMapEntry = mMapEntryTable.findMashupEntry(operation,
                service);
        logger.fine(mMessages.getString("EDMSE-F0216.mashupMapEntry_Found") + mashupMapEntry.getServiceUnitName());

        try {
            if (mashupMapEntry == null) {
                String errMsg = mMessages.getString("EDMSE-I0309.null_mashupEntry") + operation + ", service=" + service;
                logger.info(errMsg);

                // FIX ME: inOut.setStatus(ExchangeStatus.ERROR)?
                return;
            }

            if (!mashupMapEntry.isStarted()) {
                // The containing service unit is not started
                logger.info(mMessages.getString("EDMSE-I0310.su_NotStarted"));
                return;
            }

            NormalizedMessage request = inOut.getInMessage();

            if (request == null) {
                logger.info(mMessages.getString("EDMSE-I0311.null_Request"));
                return;
            }

            File f = new File(mashupMapEntry.getFile());
            String fileName = f.getName();
            String prefix = fileName.substring(0, fileName.lastIndexOf(".")) + "_";
            //logger.info("PREFIX = " + prefix);

            // Calculate processed content
            Source content = request.getContent();
            //logger.info("request content = " + content);
            Source processedContent = null;

            if (content instanceof DOMSource) {
                DOMSource xmlSource = (DOMSource) content;
                DOMResult xmlResult = new DOMResult(XmlUtil.newDocument());
                Element root;
                Node rn = xmlSource.getNode();
                root = ((Document) rn).getDocumentElement();

                if (rn instanceof Document) {
                    logger.info("InputXML: " + XmlUtil.toXml(((Document) rn).getDocumentElement(), "UTF-8", false));
                } else if (rn instanceof Element) {
                    logger.info("InputXML: " + XmlUtil.toXml(rn, "UTF-8", false));
                }


                String result = startMashupProcess(mashupMapEntry, root);
                //logger.info("result" + result);


                // String outp = "<?xml version=\"1.0\"
                // encoding=\"UTF-8\"?><dtelOperation><output><outElement><Risk>0.0</Risk></outElement></output></dtelOperation>";
                processedContent = new DOMSource(XmlUtil.createDocumentFromXML(
                        true, result));

                mashupMapEntry.getEndpointStatus().incrementReceivedRequests();
            } else {
                String errMsg = mMessages.getString("EDMSE-I0312.unknown_XMLType") + content.getClass().getName();
                logger.info(errMsg);
                mashupMapEntry.getEndpointStatus().incrementReceivedErrors();
                throw new Exception(errMsg);
            }

            // Send processed content;
            if (mashupMapEntry.getType().equals(MashupMapEntry.REQUEST_REPLY_SERVICE)) {
                Element outRoot = ((Document) ((DOMSource) processedContent).getNode()).getDocumentElement();
                NodeList nodeList = outRoot.getElementsByTagName(prefix + OutputPartNode);
                Element partNode = (Element) nodeList.item(0);
                Output output = mashupMapEntry.getOutput();
                WrapperBuilder builder = HelperFactory.createBuilder();
                builder.initialize(XmlUtil.newDocument(), output.getMessage(),
                        output.getName());
                builder.addPart(OutputPartName, partNode);

                Document normalizedDoc = builder.getResult();
                //logger.info("NormOut: "
                //	+ XmlUtil.toXml(normalizedDoc, "UTF-8", false));

                NormalizedMessage response = inOut.createMessage();
                response.setContent(new DOMSource(normalizedDoc)); // processedContent);
                inOut.setOutMessage(response);
                mChannel.send(inOut);
                mashupMapEntry.getEndpointStatus().incrementSentReplies();

                return;
            }

            logger.info(mMessages.getString("EDMSE-I0313.service_Type")+ mashupMapEntry.getType() + mMessages.getString("EDMSE-I0314.unsupport_InOut"));
        } catch (Throwable t) {
            String msg = mMessages.getString("EDMSE-S0400.exception_Occured");
            logger.log(Level.SEVERE, msg, t);
            try {
                inOut.setError(new Exception(null, t));
                inOut.setStatus(ExchangeStatus.ERROR);
                inOut.setProperty(FAULTCODE_PROPERTY_NAME, "dfsa");
                inOut.setProperty(FAULTSTRING_PROPERTY_NAME, "fda");
                inOut.setProperty(FAULTACTOR_PROPERTY_NAME, COMPONENT_NAME);
                inOut.setProperty(FAULTDETAIL_PROPERTY_NAME, "fda");
                mChannel.send(inOut);
            //this.metlMapEntry.getEndpointStatus().incrementSentErrors();
            } catch (MessagingException ex) {
                //mLogger.log(Level.SEVERE, mLoc.loc("ERRO025: unable to set error status on inOut"), ex);
            }

        //try {
        //	inOut.setStatus(ExchangeStatus.ERROR);
        //	mChannel.send(inOut);
        //	mashupMapEntry.getEndpointStatus().incrementSentErrors();
        //} catch (MessagingException ex) {
        //	logger.log(Level.SEVERE, "unable to set error status on inOut",
        //			ex);
        //}
        }
    }

    /**
     * 
     * @param mashupMapEntry
     * @param inputArgs
     * @return status
     */
    private String startMashupProcess(MashupMapEntry mashupMapEntry, Node inputArgs) throws Exception {
        ArrayList inputArgsAsList = XmlUtil.getNodeList((Element) inputArgs, "jbi:part");

        MashupProcessHandler handler = new MashupProcessHandler(mashupMapEntry, inputArgsAsList, this.runtimeConfigMBean, this.queryContext);
        return handler.startProcess();
    }

    public void run() {
        logger.info(mMessages.getString("EDMSE-I0315.seInOutThread_Started"));

        while (mRunFlag) {
            // when receiving an IN-OUT, IN-ONLY, ROBUST-IN message, it means
            // that BC is invoking SE.
            // when receiving an OUT-IN message, it means that the SE has
            // invoked out, and this message exchange
            // is the response.
            // There shouldn't be OUT-ONLY or ROBUST-OUT because only BC are
            // suppose to receive such messages
            if (mChannel == null) {
                continue;
            }

            try {
                MessageExchange msgEx = mChannel.accept();
                URI pattern = msgEx.getPattern();
                logger.info("Status: " + msgEx.getStatus().toString() + ", Pattern for exchange Id " + msgEx.getExchangeId() + " is " + pattern);

                if (msgEx.getStatus().equals(ExchangeStatus.ACTIVE)) {

                    switch (ExchangePattern.valueOf(msgEx)) {
                        case IN_OUT:

                            //logger.info(mMessages.getString("EDMSE-I0316.InOutMessage_Received") + msgEx.getExchangeId());
                            process((InOut) msgEx);
                            break;
                        case IN_ONLY:
                            logger.info(mMessages.getString("EDMSE-I0317.InOnly_NotSupported") + msgEx.getExchangeId());
                            sendError(msgEx);
                            break;
                        case IN_OPTIONAL_OUT:
                            logger.info(mMessages.getString("EDMSE-I0319.robustInOptionalOnly_NotSupported")+ msgEx.getExchangeId());
                            sendError(msgEx);
                            break;
                        case ROBUST_IN_ONLY:
                            logger.info(mMessages.getString("EDMSE-I0318.robustInOnly_NotSupported") + msgEx.getExchangeId());
                            sendError(msgEx);
                            break;
                        case CUSTOM:
                            logger.severe(mMessages.getString("EDMSE-S0401.invalid_Pattern") + msgEx.getExchangeId());
                            sendError(msgEx);
                            break;
                        default:
                            logger.severe(mMessages.getString("EDMSE-S0401.invalid_Pattern")+ msgEx.getExchangeId());
                            sendError(msgEx);
                            break;
                    }

                }
            } catch (Exception e) {
                if (mRunFlag) {
                    e.printStackTrace();
                    logger.log(Level.SEVERE,
                            mMessages.getString("EDMSE-S0402.messageAcception"), e);
                }
            }
        }
        logger.info(mMessages.getString("EDMSE-I0320.seInOutThread_Finished"));
    }

    private void sendError(MessageExchange msgEx) throws MessagingException {
        msgEx.setStatus(ExchangeStatus.ERROR);
        mChannel.send(msgEx);
    }

    public void cease() {
        try {
            mRunFlag = false;
            logger.info(mMessages.getString("EDMSE-I0322.seInOutThread_Ceased"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    private NotificationListener listener = new NotificationListener() {

        public void handleNotification(Notification notification, Object obj) {
            logger.log(Level.INFO, "MashupSEInOutThread.Handling_notification");

            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                logger.log(Level.INFO,
                        "MashupSEInOutThread.Getting_notification_attribute" + attrName);

                envProps.setProperty(attrName, (String) attrNotif.getNewValue());
            // TODO use this envProps for the next instance of ETL engine
            }
        }
    };

    private class InOutMessageHandler implements Runnable {

        InOut inOutMsg;

        public InOutMessageHandler(InOut inout) {
            inOutMsg = inout;
        }

        public void run() {
            handleInOutMessage(inOutMsg);
        }
    }
}
