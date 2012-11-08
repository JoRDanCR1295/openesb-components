
package com.sun.jbi.engine.scriptse.jbiadapter;

import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import com.sun.jbi.common.xml.XmlUtil;


import com.sun.jbi.engine.scriptsecore.process.ScriptInfoVO.WorkingMode;
import com.sun.jbi.engine.scriptsecore.process.ScriptExecutorImpl;
import com.sun.jbi.engine.scriptsecore.process.SUScriptExecutor;

import javax.xml.parsers.DocumentBuilder;
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
 *
 * @author rchen
 */
public class ScriptseExecutorFactory {
private static Logger mLogger = Logger.getLogger(ScriptseExecutorFactory.class.getName());
    private static TransformerFactory mTransFact = TransformerFactory.newInstance();
    private static ErrorListener mErrorListener = new ErrorListener() {
    private DocumentBuilder mBuilder = null;
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
    private ScriptseExecutorFactory()
    {
    };
    /**
     * DOCUMENT ME!
     *
     * @param src
     * @param endpt
     * @param ctx
     *
     * @return
     */
    public static Source transform(Source src, ScriptseEndpoint endpt) {
        DOMSource ret = null;
        DOMSource domSrc = null;

        try {
            domSrc = (src instanceof DOMSource) ? (DOMSource) src
                    : convert(src);

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
                Source result = new DOMSource(XmlUtil.newDocument());
                DOMSource payload = new DOMSource(XmlUtil.newDocument());
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
    private static DOMSource convert(Source src) {
        try {
            Transformer transformer = null;

            synchronized (mTransFact) {
                transformer = mTransFact.newTransformer();
                transformer.setErrorListener(mErrorListener);
            }

            DOMResult result = new DOMResult(XmlUtil.newDocument());
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
