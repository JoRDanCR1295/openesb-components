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

package com.sun.jbi.engine.workflow.base;

import java.io.InputStream;
import java.io.StringReader;
import java.security.Principal;
import java.security.acl.Group;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.security.auth.Subject;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

import sun.security.acl.PrincipalImpl;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.engine.workflow.BPELWorkflowRequest;
import com.sun.jbi.engine.workflow.ClientWorkflowRequest;
import com.sun.jbi.engine.workflow.WorkflowEngine;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.WorkflowRequest;
import com.sun.jbi.engine.workflow.clientapi.operations.ClientOperationsHelper;
import com.sun.jbi.engine.workflow.process.GroupImpl;
import com.sun.jbi.engine.workflow.process.InOutCallBack;
import com.sun.jbi.engine.workflow.process.TimeoutSystemException;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XmlUtil;

/**
 * @author sgharibi
 * 
 */
public class WLMSEInOutProvider implements InOutCallBack {
    private static final Logger LOGGER = Logger
            .getLogger(WLMSEInOutProvider.class.getName());

    private static final String STATIC_SERVICE_NAME = "TaskCommonService";

    private static final String STATIC_URI = "http://jbi.com.sun/wfse/wsdl/TaskCommon";

    private static final String STATIC_PORTTYPE_NAME = "TaskCommonPortType";

    private static final String OutputPartName = "output";

    private static final String OutputPartNode = "ns0:outElement";

    // public static final String CUST_REL_MESG_PROP =
    // "javax.jbi.messaging.messageid";

    private Hashtable<String, InOutWrapper> mRequests = new Hashtable<String, InOutWrapper>();

    private WorkflowEngine mEngine;

    private WLMSEManagerContext mManagerContext;

    private static class InOutWrapper {
        private InOut mInOut;

        private WorkflowMapEntry mEntry;

        public InOutWrapper(InOut inOut, WorkflowMapEntry entry) {
            mInOut = inOut;
            mEntry = entry;
        }

        public WorkflowMapEntry getEntry() {
            return mEntry;
        }

        public InOut getInOut() {
            return mInOut;
        }

    }

    public WLMSEInOutProvider(WorkflowEngine engine,
            WLMSEManagerContext managerContext) {
        mEngine = engine;
        mEngine.setInOutCallBack(this);
        mManagerContext = managerContext;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processIn(com.sun.jbi.crl.mep.exchange.CRLInOut,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    public void processIn(InOut inOut) throws JBIException {
        WorkflowMapEntry entry = mManagerContext.findEntry(inOut);
        if (entry == null) {
            throw new JBIException(
                    I18n
                            .loc(
                                    "WLM-7009: No operation is available for service: {0}, operation: {1}",
                                    inOut.getService(), inOut.getOperation()));
        }
        mRequests.put(inOut.getExchangeId(), new InOutWrapper(inOut, entry));
        process(inOut, entry);
    }

    private static DocumentBuilder mBuilder = null;

    private static Document newDocument() throws ParserConfigurationException {
        if (mBuilder == null) {
            DocumentBuilderFactory factory = DocumentBuilderFactory
                    .newInstance();
            mBuilder = factory.newDocumentBuilder();
        }
        return mBuilder.newDocument();
    }

    private void process(InOut inOut, WorkflowMapEntry workflowMapEntry)
            throws JBIException {
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
            DOMSource xmlSource = null;
            Element el = null;
            if (content != null) {
                try {
                    xmlSource = com.sun.jbi.common.xml.XmlUtil
                            .toDOMSource(content);
                } catch (Exception e) {
                    // Hack to fix javaee prefix issue, javaee in GF 2.0 misses
                    // to define all ns prefixes in jbi message
                    // Sacrifice the performance by inserting the required the
                    // ns into jbi message wrapper
                    InputStream is = ((StreamSource) content).getInputStream();
                    is.reset();
                    String oldContent = Util.getStringBuffer(is);
                    oldContent = oldContent
                            .replaceFirst("<jbi:message",
                                    "<jbi:message xmlns:ns2=\"http://jbi.com.sun/wfse/wsdl/TaskCommon\"");
                    el = com.sun.jbi.common.xml.XmlUtil.readXml(
                            new InputSource(new StringReader(oldContent)))
                            .getDocumentElement();
                }
            }

            if (xmlSource != null || el != null) {
                // DOMResult xmlResult = new DOMResult(newDocument());
                if (xmlSource != null) {
                    Node rn = xmlSource.getNode();

                    if (rn instanceof Document) {
                        LOGGER.fine("InputXML: "
                                + XmlUtil.toXml(((Document) rn)
                                        .getDocumentElement(), "UTF-8", false));
                        el = ((Document) rn).getDocumentElement();
                    } else if (rn instanceof Element) {
                        LOGGER.fine("InputXML: "
                                + XmlUtil.toXml(rn, "UTF-8", false));
                        el = (Element) rn;
                    }
                }

                // TODO: WorkflowEngine workflowEngine =
                // workflowMapEntry.getWorkflowEngine();

                WorkflowRequest workflowReq = makeRequest(el, inOut,
                        workflowMapEntry);
                mEngine.acceptRequest(workflowReq);

                // TODO: workflowEngine.execute(xmlSource, (DOMSource)
                // processedContent);
                // workflowMapEntry.getEndpointStatus()
                // .incrementReceivedRequests();
            }

            // Send processed content;
            /*
             * if
             * (workflowMapEntry.getType().equals(WorkflowMapEntry.REQUEST_REPLY_SERVICE)) {
             * 
             * Element outRoot = ((Document)((DOMSource)
             * processedContent).getNode()).getDocumentElement(); NodeList
             * nodeList = outRoot.getElementsByTagName(OutputPartNode); Element
             * partNode = (Element) nodeList.item(0); Output output =
             * workflowMapEntry.getOutput(); WrapperBuilder builder =
             * HelperFactory.createBuilder(); builder.initialize(newDocument(),
             * output.getMessage(), output.getName());
             * builder.addPart(OutputPartName, partNode); Document normalizedDoc =
             * builder.getResult(); mLogger.info("NormOut: " +
             * XmlUtil.toXml(normalizedDoc, "UTF-8", false));
             * 
             * NormalizedMessage response = inOut.createMessage();
             * response.setContent(new DOMSource(normalizedDoc)); //
             * processedContent); inOut.setOutMessage(response);
             * 
             * //later we will want to delegate back to CRL to do the send by
             * returning back the populated //CRLInOut object. //Right now it is
             * not determined how to update the EndpointStatus after sending the
             * data. inOut.send();
             * workflowMapEntry.getEndpointStatus().incrementReceivedDones();
             * return null; }
             */
            // TODO:
            // mLogger.info("Service type: " + workflowMapEntry.getType() + "
            // doesn't support
            // InOut");
        } catch (Throwable t) {
            // workflowMapEntry.getEndpointStatus().incrementReceivedErrors();
            String msg = "caught unexpected exception";
            LOGGER.log(Level.SEVERE, msg, t);
            try {
                inOut.setStatus(ExchangeStatus.ERROR);
                mManagerContext.getMessagingChannel().send(inOut);
                // inOut.send();
            } catch (MessagingException ex) {
                LOGGER.log(Level.SEVERE, "unable to set error status on inOut",
                        ex);
            }
        }

    }

    // private Node hackStream(Source content) {
    // Node node = null;
    // try {
    // InputStream in =
    // ((javax.xml.transform.stream.StreamSource)content).getInputStream();
    // byte[] b = new byte[in.available()];
    // in.read(b);
    // in.reset();
    // DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    // factory.setNamespaceAware(true);
    // Document doc = factory.newDocumentBuilder().parse(new
    // ByteArrayInputStream(b));
    //
    // node = doc.getDocumentElement();
    //    		
    // } catch(Exception ex) {
    // LOGGER.log(Level.SEVERE, "unable to get DOM Node", ex);
    // }
    //    	
    // return node;
    // }
    private WorkflowRequest makeRequest(Element el, InOut inOut,
            WorkflowMapEntry workflowMapEntry) throws JBIException {
        // TODO Auto-generated method stub
        WorkflowRequest workflowReq = null;
        if (workflowMapEntry.isTask()) {
            NormalizedMessage request = inOut.getInMessage();
            String reliId = (String) inOut
                    .getProperty(ServiceQuality.MESSAGE_ID);
            if (reliId == null) {
                reliId = inOut.getExchangeId();
            }
            workflowReq = new BPELWorkflowRequest(inOut.getExchangeId(),
                    reliId, el, workflowMapEntry.getTaskModel());
        } else {
            Definition wsdl = workflowMapEntry.getWsdl();
            QName serviceName = inOut.getEndpoint().getServiceName();
            PortType portType = null;
            if (isStaticService(serviceName)) {
                portType = getStaticPorType(wsdl);
            } else {
                String portName = inOut.getEndpoint().getEndpointName();
                Service service = wsdl.getService(serviceName);
                Port port = service.getPort(portName);
                portType = port.getBinding().getPortType();
            }
            Operation operation = portType.getOperation(inOut.getOperation()
                    .getLocalPart(), null, null);
            if (operation == null) {
                String msg = I18n
                        .loc(
                                "WLM-7009: No operation is available for service: {0}, operation: {1}",
                                portType.getQName(), inOut.getOperation());
                LOGGER.log(Level.SEVERE, msg);
                throw new JBIException(msg);
            }
            try {
                workflowReq = new ClientWorkflowRequest(inOut.getExchangeId(),
                        el, operation, getSubject(inOut, el, operation));
            } catch (Exception e) {
                // TODO Auto-generated catch block
                String msg = I18n
                        .loc(
                                "WLM-6047: WorkflowSEInOutProvider can not get Security Subject for service: {0}, operation: {1}",
                                portType.getQName(), inOut.getOperation());
                LOGGER.log(Level.WARNING, msg);
                throw new JBIException(msg, e);
            }
        }
        return workflowReq;
    }

    private boolean isStaticService(QName serviceName) {
        // TODO Auto-generated method stub
        if (serviceName.getNamespaceURI().equals(STATIC_URI)
                && serviceName.getLocalPart().equals(STATIC_SERVICE_NAME)) {
            return true;
        } else {
            return false;
        }
    }

    private PortType getStaticPorType(Definition wsdl) {
        // TODO Auto-generated method stub
        return wsdl.getPortType(new QName(STATIC_URI, STATIC_PORTTYPE_NAME));
    }

    // TODO: get subject from ME
    private Subject getSubject(InOut inOut, Element el, Operation opt)
            throws Exception {
        Subject subject = inOut.getInMessage().getSecuritySubject();
        String userId = ClientOperationsHelper.extractUserID(el, opt);
        // if userId has #, it means the names ("," delimited) are used as
        // groups in place of what passed in subject.
        if (userId == null || userId.length() == 0) {
            userId = "workflowUser";
        } else if (userId.indexOf("#") != -1) {
            Set<Principal> principals = new HashSet<Principal>();
            int start = userId.indexOf("#") + 1;
            String user = userId.substring(0, start - 1);
            Principal userPrincipal = new PrincipalImpl(user);
            principals.add(userPrincipal);
            String groups = userId.substring(start);
            String[] groupNames = groups.split(",");
            if (groupNames != null && groupNames.length > 0) {
                for (int i = 0; i < groupNames.length; i++) {
                    Group group = new GroupImpl(groupNames[i]);
                    principals.add(group);
                }
            }
            subject = new Subject(true, principals, new HashSet(),
                    new HashSet());
            return subject;
        }
        if (subject == null || subject.getPrincipals().size() == 0) {
            Principal principal = new PrincipalImpl(userId);
            Set<Principal> principals = new HashSet<Principal>();
            principals.add(principal);
            subject = new Subject(true, principals, new HashSet(),
                    new HashSet());
        } else {
            Set<Principal> principals = subject.getPrincipals();
            Set<Principal> newPrincipals = new HashSet<Principal>();
            boolean found = false;
            // Look for user Id in th principal list
            for (Principal principal : principals) {
                if (principal.getName().equals(userId)) {
                    newPrincipals.add(principal);
                    found = true;
                } else {
                    Group group = new GroupImpl(principal.getName());
                    newPrincipals.add(group);
                }
            }
            if (!found) {
                // If the useid and the princpal do not match, it means the user
                // by-pass the principal and want to use userId
                Principal userPrincipal = new PrincipalImpl(userId);
                newPrincipals.add(userPrincipal);
            }
            subject = new Subject(true, newPrincipals, new HashSet(),
                    new HashSet());
        }
        return subject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processStatus(com.sun.jbi.crl.mep.exchange.CRLInOut,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    public void processStatus(InOut msg) {
        // TODO Auto-generated method stub

    }

    public void  onFault(String meId, DOMSource faultSource) {
        // TODO Auto-generated method stub
        InOutWrapper wrapper = mRequests.remove(meId);
        if (wrapper != null) {
            InOut inOut = wrapper.getInOut();
            WorkflowMapEntry entry = wrapper.getEntry();
            try {
                Fault fault = inOut.createFault();
                fault.setContent(faultSource);
                inOut.setFault(fault);
                // inOut.send();
                mManagerContext.getMessagingChannel().send(inOut);
                // entry.getEndpointStatus().incrementSentErrors();
            } catch (Exception e) {
                // TODO Auto-generated catch block
                LOGGER
                        .log(
                                Level.WARNING,
                                I18n
                                        .loc("WLM-6045: WorkflowSEInOutProvider on fault exception"),
                                e);
                throw new RuntimeException(e);

            }           
        }

    }

    public void  onReply(String meId, DOMSource reply) {
        InOutWrapper wrapper = mRequests.remove(meId);
        if (wrapper != null) {
            InOut inOut = wrapper.getInOut();
            WorkflowMapEntry entry = wrapper.getEntry();

            try {
                NormalizedMessage response = inOut.createMessage();
                response.setContent(reply);
                inOut.setOutMessage(response);
                // inOut.send();
                mManagerContext.getMessagingChannel().send(inOut);
                // entry.getEndpointStatus().incrementSentReplies();
            } catch (Exception e) {
                LOGGER
                        .log(
                                Level.WARNING,
                                I18n
                                        .loc("WLM-6046: WorkflowSEInOutProvider on reply exception"),
                                e);
                throw new RuntimeException(e);
            }
        } 

    }

    public void  onTimeout(String meId, DOMSource timeout) {
        InOutWrapper wrapper = mRequests.remove(meId);
        if (wrapper != null) {
            InOut inOut = wrapper.getInOut();
            WorkflowMapEntry entry = wrapper.getEntry();

            try {
                TimeoutSystemException systemError = new TimeoutSystemException(
                        timeout);
                inOut.setError(systemError);

                // Fault response = inOut.createFault();
                // response.setContent(timeout);
                // inOut.setFault(response);
                // inOut.send();
                mManagerContext.getMessagingChannel().send(inOut);
                entry.getEndpointStatus().incrementSentReplies();
            } catch (Exception e) {
                LOGGER
                        .log(
                                Level.WARNING,
                                I18n
                                        .loc("WLM-6046: WorkflowSEInOutProvider on reply exception"),
                                e);
                throw new RuntimeException(e);
            }
        } 
    }

 
}
