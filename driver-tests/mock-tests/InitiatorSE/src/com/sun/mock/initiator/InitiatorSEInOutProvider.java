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
 * @(#)WorkflowSEInOutProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

    package com.sun.mock.initiator;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.Principal;
import java.security.acl.Group;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.security.auth.Subject;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import sun.security.acl.PrincipalImpl;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider;
import com.sun.jbi.internationalization.Messages;

/**
 * @author sgharibi
 * 
 */
public class InitiatorSEInOutProvider extends AbstractInOutProvider {
    private static final Logger LOGGER = Logger.getLogger(InitiatorSEInOutProvider.class.getName());

    private static final Messages MESSAGES = Messages.getMessages(InitiatorSEInOutProvider.class);

    private static final String OutputPartName = "output";

    private static final String OutputPartNode = "ns0:outElement";

    public static final String CUST_REL_MESG_PROP = "javax.jbi.messaging.messageid";


    private InitiatorEngine mEngine;

    private Definition mDef;
	
	private Entry mEntry;
	
    public InitiatorSEInOutProvider(Definition definition, InitiatorEngine engine) {
        mEngine = engine;
        this.mDef = definition;
        QName serviceName = ServiceConstants.PARTICIPANT1_IN_OUT_SERVICE_NAME;
        String endpointName = ServiceConstants.PARTICIPANT1_IN_OUT_ENDPOINT_NAME;
        PortType pt = mDef.getPortType(new QName("http://j2ee.netbeans.org/wsdl/transferAmount", "transferAmountPortType"));
        Operation op = pt.getOperation("transferAmountOperation", "input1", "output1");
        
		this.mEntry = new Entry(serviceName, endpointName, op, pt);
		
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processIn(com.sun.jbi.crl.mep.exchange.CRLInOut,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    @Override
    public void processIn(CRLInOut inOut, ExchangeContext ctx) throws JBIException {
        //mRequests.put(inOut.getExchangeId(), new InOutWrapper(inOut, entry));
        process(inOut);
    }

    private static DocumentBuilder mBuilder = null;

    private static Document newDocument() throws ParserConfigurationException {
        if (mBuilder == null) {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            mBuilder = factory.newDocumentBuilder();
        }
        return mBuilder.newDocument();
    }

    private void process(CRLInOut inOut) throws JBIException {
        if (inOut == null) {
            return;
        }
        QName operation = inOut.getOperation();
        QName service = inOut.getEndpoint().getServiceName();

        try {
            NormalizedMessage request = inOut.getInMessage();
            if (request == null) {
                return;
            }

            // Calculate processed content
            Source content = request.getContent();
            Source processedContent = null;
            DOMSource xmlSource = null;
            if (content != null) {
                if (content instanceof DOMSource) {
                    xmlSource = (DOMSource) content;
                } else {
                    // hackStream(content);

                    Transformer t = TransformerFactory.newInstance().newTransformer();
                    DOMResult result = new DOMResult();
                    t.transform(content, result);
                    if (result.getNode() != null) {
                        xmlSource = new DOMSource(result.getNode());
                    }
                }
            }

            if (xmlSource != null) {
                // DOMResult xmlResult = new DOMResult(newDocument());
                Node rn = xmlSource.getNode();
                Element el = null;
                if (rn instanceof Document) {
                    LOGGER.info("InputXML: "
                            + XmlUtil.toXml(((Document) rn).getDocumentElement(), "UTF-8", false));
                    el = ((Document) rn).getDocumentElement();
                } else if (rn instanceof Element) {
                    LOGGER.info("InputXML: " + XmlUtil.toXml(rn, "UTF-8", false));
                    el = (Element) rn;
                }

                // TODO: WorkflowEngine workflowEngine = workflowMapEntry.getWorkflowEngine();

                //WorkflowRequest workflowReq = makeRequest(el, inOut, workflowMapEntry);
                //mEngine.acceptRequest(workflowReq);

                // TODO: workflowEngine.execute(xmlSource, (DOMSource) processedContent);
                
            }
            
            inOut.setStatus(ExchangeStatus.ERROR);
            inOut.send();

            // Send processed content;
            /*
             * if (workflowMapEntry.getType().equals(WorkflowMapEntry.REQUEST_REPLY_SERVICE)) {
             * 
             * Element outRoot = ((Document)((DOMSource)
             * processedContent).getNode()).getDocumentElement(); NodeList nodeList =
             * outRoot.getElementsByTagName(OutputPartNode); Element partNode = (Element)
             * nodeList.item(0); Output output = workflowMapEntry.getOutput(); WrapperBuilder
             * builder = HelperFactory.createBuilder(); builder.initialize(newDocument(),
             * output.getMessage(), output.getName()); builder.addPart(OutputPartName, partNode);
             * Document normalizedDoc = builder.getResult(); mLogger.info("NormOut: " +
             * XmlUtil.toXml(normalizedDoc, "UTF-8", false));
             * 
             * NormalizedMessage response = inOut.createMessage(); response.setContent(new
             * DOMSource(normalizedDoc)); // processedContent); inOut.setOutMessage(response);
             * 
             * //later we will want to delegate back to CRL to do the send by returning back the
             * populated //CRLInOut object. //Right now it is not determined how to update the
             * EndpointStatus after sending the data. inOut.send();
             * workflowMapEntry.getEndpointStatus().incrementReceivedDones(); return null; }
             */
            // TODO:
            // mLogger.info("Service type: " + workflowMapEntry.getType() + " doesn't support
            // InOut");
        } catch (Throwable t) {
            
            String msg = "caught unexpected exception";
            LOGGER.log(Level.SEVERE, msg, t);
            try {
                inOut.setStatus(ExchangeStatus.ERROR);
                inOut.send();
            } catch (MessagingException ex) {
                LOGGER.log(Level.SEVERE, "unable to set error status on inOut", ex);
            }
        }

    }

    // private Node hackStream(Source content) {
    // Node node = null;
    // try {
    // InputStream in = ((javax.xml.transform.stream.StreamSource)content).getInputStream();
    // byte[] b = new byte[in.available()];
    // in.read(b);
    // in.reset();
    // DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    // factory.setNamespaceAware(true);
    // Document doc = factory.newDocumentBuilder().parse(new ByteArrayInputStream(b));
    //
    // node = doc.getDocumentElement();
    //    		
    // } catch(Exception ex) {
    // LOGGER.log(Level.SEVERE, "unable to get DOM Node", ex);
    // }
    //    	
    // return node;
    // }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processStatus(com.sun.jbi.crl.mep.exchange.CRLInOut,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    @Override
    public void processStatus(CRLInOut msg, ExchangeContext ctx) {
        // TODO Auto-generated method stub

    }

    
    public void onReply(String meId, DOMSource reply) {
    /*
        try {
            NormalizedMessage response = inOut.createMessage();
            response.setContent(reply);
            inOut.setOutMessage(response);
            inOut.send();
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, MESSAGES
                    .getString("WorkflowSEInOutProvider.On_Reply_Exception"), e);
            throw new RuntimeException(e);
        }
*/
    }

    
}
