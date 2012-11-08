/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package com.zaz.jbi.engine.screenscrapingse.process;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

import com.zaz.jbi.engine.screenscrapingse.ScreenScrapingseEndpoint;
import com.zaz.jbi.engine.screenscrapingse.process.ScriptInfoVO.WorkingMode;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.StringWriter;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;

import javax.xml.namespace.QName;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

/**
 * Utility class for Script service engine.
 *
 * @author Prashanth B.R
 */
public class ScreenScrapingSEUtil {

    private static Logger mLogger = Logger.getLogger(ScreenScrapingSEUtil.class.getName());
    private static TransformerFactory mTransFact = TransformerFactory.newInstance();
    private static ErrorListener mErrorListener = new ErrorListener() {

        public void warning(TransformerException exception)
                throws TransformerException {
            System.out.println("warning");
            exception.printStackTrace();
        }

        public void fatalError(TransformerException exception)
                throws TransformerException {
            System.out.println("fatal");
            exception.printStackTrace();
        }

        public void error(TransformerException exception)
                throws TransformerException {
            System.out.println("error");
            exception.printStackTrace();
        }
    };

    /* Not intended to be instantiated. */
    private ScreenScrapingSEUtil() {
    }

    /**
     * DOCUMENT ME!
     *
     * @param src
     * @param endpt
     * @param ctx
     *
     * @return
     */
    public static Source transform(Source src, ScreenScrapingseEndpoint endpt,
            ExchangeContext ctx) {
        DOMSource ret = null;
        DOMSource domSrc = null;

        try {
            domSrc = (src instanceof DOMSource) ? (DOMSource) src
                    : convert(src, ctx);

            //      if (endpt.isTransformJBI())
            //      {
            ////        ret = transformAsIs(domSrc, endpt, ctx);
            //      }
            //      else
            //      {
            // extract payload content of jbi:part element
            Element partContent = extractPartContent(domSrc.getNode());

            if (partContent != null) {
                //                    DOMResult result = new DOMResult(ctx.newDocument());
                Source result = new DOMSource(ctx.newDocument());
                DOMSource payload = new DOMSource(ctx.newDocument());
                payload.setNode(partContent);
                mLogger.log(Level.INFO,
                        "The PAYLOAD CONTENT WRAPPED IN EXCHANGE CTX DOC IS \n\n");

                mLogger.log(Level.INFO, "actual transform - " + print(payload));

                //                    endpt.getTransformer().transform(payload, result);
                //PRASH ADDED --
                if (endpt.getMScrInfo().getMWorkMode().equals(WorkingMode.CMPLX_MODE)) {
                    ScriptExecutorImpl lExecutor = new ScriptExecutorImpl(endpt.getMSUInstallPath());
                    result = lExecutor.processScript(payload);
                    System.out.println("\nResult is" + result);
                    //PRASH ADDED -- TO HARD CODE THE MESSAGE TYPE ETC
                    endpt.setMessageType(new QName(
                            "http://www.zaz-consulting.com/screenscraping/screenscrapingse",
                            "echo"));
                } else if (endpt.getMScrInfo().getMWorkMode().equals(WorkingMode.SIMPLE_MODE)) {
                    /** Added to demonstrate the Sample Use case of String input to Jerry...**/

                    //            SUScriptExecutor lExecutor = new SUScriptExecutor(endpt.getMSUInstallPath(),endpt.getMScrInfo());
                    //             String lInputParam =  payload.getNode().getTextContent();
                    //            //Converting the DOMSource Content in to a String.
                    ////            String lInputParam = ScriptXMLHelper.readFromDOMSource(payload).toString();
                    //            mLogger.log(Level.INFO, "The INPUT PARAMETER IS \n\n"+ lInputParam);
                    //            Object lResult = lExecutor.executeSUScript(lInputParam);
                    //            
                    //            if (lResult == null)
                    //            {
                    //              lResult = new String("<result>Successfully Executed the Script</result>");
                    //            }
                    //                        
                    //            //Wrapping the result object back in to an DOMSource
                    //            result = ScriptXMLHelper.createDOMSource(new StringReader(lResult.toString()));
                    /** Added to demonstrate the Sample Use case of String input to Jerry...ENDS******/
                    SUScriptExecutor lExecutor = new SUScriptExecutor(endpt.getMSUInstallPath(),
                            endpt.getMScrInfo());
                    result = lExecutor.processScript(payload);
                    mLogger.log(Level.INFO, "The Result is \n\n" + result);
                } else {
                    mLogger.log(Level.SEVERE,
                            "NO MODE IS SET FOR THE END POINT.. FAILED TO WORK...\n\n");
                }

                //PRASH ADDED ENDS--
                // wrap transformed payload in WSDL 1.1 jbi:message
                Element plMsg = wrapTransformedPayload(result,
                        endpt.getMessageType());
                ret = new DOMSource(plMsg);

                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine("actual result - " + print(ret));
                }
            } //if (partContent != null) ends.
        //      }//else ends.

        } catch (Exception e) {
            // TODO handle better
            mLogger.log(Level.SEVERE, "Script transform failed", e);
        }

        return ret;
    }

    private static DOMSource convert(Source src, ExchangeContext ctx) {
        try {
            Transformer transformer = null;

            synchronized (mTransFact) {
                transformer = mTransFact.newTransformer();
                transformer.setErrorListener(mErrorListener);
            }

            DOMResult result = new DOMResult(ctx.newDocument());
            transformer.transform(src, result);

            return new DOMSource((Document) result.getNode());
        } catch (Exception e) {
            mLogger.warning("Failed to convert to DOMSource: " +
                    e.getMessage());

            return null;
        }
    }

    /**
     * Formats the specified xml source and returns an xml string.
     *
     * @param src The xml source object.
     *
     * @return Formatted xml or an empty string if formatting fails.
     */
    public static String print(Source src) {
        try {
            Transformer transformer = null;

            synchronized (mTransFact) {
                transformer = mTransFact.newTransformer();
                transformer.setErrorListener(mErrorListener);
            }

            StringWriter writer = new StringWriter();
            StreamResult dest = new StreamResult(writer);
            transformer.transform(src, dest);

            return writer.toString();
        } catch (Exception e) {
            mLogger.info("Failed to print xml: " + e.getMessage());

            return "";
        }
    }

    /**
     * Propogates any transaction information from one message exchange to another.
     *
     * @param from the incoming message
     * @param to the outgoing message
     */
    public static void propogateTransaction(MessageExchange from,
            MessageExchange to) {
        if ((from != null) && (to != null) && from.isTransacted()) {
            Object txProp = from.getProperty(InOnly.JTA_TRANSACTION_PROPERTY_NAME);
            mLogger.info("Propogating transaction: " + String.valueOf(txProp));
            to.setProperty(InOnly.JTA_TRANSACTION_PROPERTY_NAME, txProp);
        }
    }

    private static DOMSource transformAsIs(DOMSource src,
            ScreenScrapingseEndpoint endpt, ExchangeContext ctx) {
        DOMSource ret = null;

        try {
            DOMResult result = new DOMResult(ctx.newDocument());
            endpt.getTransformer().transform(src, result);
            ret = new DOMSource((Document) result.getNode());
        } catch (Exception e) {
            // TODO handle better
            mLogger.log(Level.SEVERE, "Script transform failed", e);
        }

        return ret;
    }

    private static Element extractPartContent(Node node) {
        if (node instanceof Document) {
            return extractPartContent(((Document) node).getDocumentElement());
        }

        if (node instanceof Element) {
            Element msg = (Element) node;

            if (msg.getTagName().equals(WrapperUtil.WRAPPER_MESSAGE)) {
                // we have the jbi:message node, find first part (only 1 supported...)
                NodeList children = msg.getElementsByTagName(WrapperUtil.WRAPPER_PART);

                if (children.getLength() > 0) {
                    Element part = (Element) children.item(0);

                    // payload is expected to be first ELEMENT child
                    NodeList partChildren = part.getElementsByTagName("*");

                    if (partChildren.getLength() > 0) {
                        return (Element) partChildren.item(0);
                    }
                }
            }
        }

        return null;
    }

    private static Element wrapTransformedPayload(Source result,
            QName resultMsgType) throws Exception {
        DOMSource lResultSrc = (DOMSource) result;
        Document doc = (Document) lResultSrc.getNode();
        Element plPart = WrapperUtil.createJBIWrappedPart(doc,
                doc.getDocumentElement());
        Element plMsg = WrapperUtil.createJBIMessageWrapper(doc, resultMsgType,
                null);
        // append part and add type namespace decl
        plMsg.appendChild(plPart);
        doc.appendChild(plMsg);

        return plMsg;
    }
}
