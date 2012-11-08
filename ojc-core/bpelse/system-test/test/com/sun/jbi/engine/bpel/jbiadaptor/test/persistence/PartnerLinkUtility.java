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
 * @(#)CorrelationUtility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import java.rmi.server.UID;
import java.util.Properties;

import javax.transaction.Transaction;
import javax.wsdl.Message;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import junit.framework.Assert;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;

/**
 * Persistence utility class for correlation unit tests.
 * 
 * @author Kevan Simpson
 */
public class PartnerLinkUtility extends UtilityClass {
    
    
    public void initiateBPInstanceForWaitWithReply(final Properties props, 
            final Engine eng, 
            DeploymentBindings deplBindings) 
    throws Exception {
        initiateBPInstanceForReplyBasedBPELs(props, eng, deplBindings);
        waitAndContinue(props, eng);
    }
    
    public void associate2WayInvokeChannel(final Properties props, 
                                           final Engine eng, 
                                           DeploymentBindings deplBindings) 
            throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, QName operation, boolean oneWay, RBPELProcess process) {
                return process2WayInvokeForPLink(props, msgContainer, model, eng);
            }

            public void reply(MessageContainer msgContainer) {
                acceptMessageForTest(props, msgContainer, model, eng, msgContainer.getId());
            }
        };
        eng.setOutChannel(channel);
    }
    
    private String process2WayInvokeForPLink(Properties props, 
            MessageContainer container, 
            InComingEventModel model, 
            Engine eng) {      
        try {
            
            // send message         
            String ipFileLoc = props.getProperty("INVOKE_2WAY_RESPONSE_FILE");
            Document doc = EngineDriver.getDocument(ipFileLoc);

            String respMesgType = props.getProperty("INVOKE_2WAY_RESPONSE_MESG_TYPE");
            QName respMsgTypeQName = QName.valueOf(respMesgType);
            Message wsdlMessage = model.getBPELProcess().getWSDLMessage(respMsgTypeQName);

            JBIMessageImpl outMsg = new JBIMessageImpl(doc, wsdlMessage);

            String messageExchangeId = new UID().toString();
            Transaction tx = container.getTransaction();
            EngineDriver.registerTransaction(messageExchangeId, tx);
            MessageContainer responseContainer = 
                MessageContainerFactory.createMessage(messageExchangeId, outMsg, container.getCRMPInvokeId(), tx);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.REPLY_FAULT, responseContainer.getId());
            eng.process(event, responseContainer);
            return messageExchangeId;
        } 
        catch (Exception e) {
            String messageExchangeId = new UID().toString();            
            MessageContainer statusContainer = MessageContainerFactory.createErrorStatus(messageExchangeId,
                    e.getMessage(),null,  null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.ERROR, statusContainer.getId());
            eng.process(event, statusContainer);
            e.printStackTrace();
            Assert.fail(e.getMessage());
            return messageExchangeId;
        }
    }


    public void associatePartnerLinkReplyChannel(final Properties props, 
            final Engine eng, DeploymentBindings deplBindings) 
    throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public void reply(MessageContainer msgContainer) {
                acceptMessageForTest(props, msgContainer, model, eng, msgContainer.getId());
            }

            /** @see com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor#getExternalEndPoint(com.sun.bpel.model.PartnerLink)
             */
            @Override
            public DocumentFragment getEndpointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole) {
                try {
                    final String WS_ADDRESSING_NS = "http://schemas.xmlsoap.org/ws/2004/08/addressing";                
                    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                    DocumentBuilder builder = factory.newDocumentBuilder();
                    final Document document  = builder.newDocument();

                    DocumentFragment fragment = document.createDocumentFragment();
                    
                    final String DUMMY_NS = "http://schemas.xmlsoap.org/ws/2004/08/addressing";
                    Element rootElem = document.createElementNS(DUMMY_NS, "EndpointReference");
                    fragment.appendChild(rootElem);
                    
                    // create the wsa:Address element
                    Element address = document.createElementNS(WS_ADDRESSING_NS, "wsa:Address");
                    address.setTextContent("http://localhost:18181/newWSDLService/newWSDLPort");
                    rootElem.appendChild(address);
                    
                    // create the wsa:ServiceName element
                    Element serviceName = document.createElementNS(WS_ADDRESSING_NS, "wsa:ServiceName");
                    String serviceNs = "http://j2ee.netbeans.org/wsdl/newWSDL";
                    String serviceLocalName = "newWSDLService";
                    serviceName.setAttributeNS("http://www.w3.org/2000/xmlns/","xmlns:ns0", serviceNs);
                    serviceName.setTextContent("ns0:" + serviceLocalName);
                    Attr port = document.createAttribute("PortName");
                    port.setValue("newWSDLPort");
                    serviceName.setAttributeNode(port);
                    rootElem.appendChild(serviceName);
                    fragment = (DocumentFragment)com.sun.jbi.engine.bpel.core.bpel.util.Utility.wrapinServiceRef(fragment);
                    return fragment;                    
                } catch (Exception ex) {
                    ex.printStackTrace();
                    throw new RuntimeException(ex);
                } 
            }
            
            
        };

        eng.setOutChannel(channel);
    }
    
    public void associatePartnerLinkReplyChannel_testDocFragWithoutSingleRootElement(final Properties props, 
            final Engine eng, DeploymentBindings deplBindings) 
    throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public void reply(MessageContainer msgContainer) {
                acceptMessageForTest(props, msgContainer, model, eng, msgContainer.getId());
            }

            /** @see com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor#getExternalEndPoint(com.sun.bpel.model.PartnerLink)
             */
            @Override
            public DocumentFragment getEndpointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole) {
                try {
                    final String WS_ADDRESSING_NS = "http://schemas.xmlsoap.org/ws/2004/08/addressing";                
                    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                    DocumentBuilder builder = factory.newDocumentBuilder();
                    final Document document  = builder.newDocument();

                    DocumentFragment fragment = document.createDocumentFragment();
                    
                    // create the wsa:Address element
                    Element address = document.createElementNS(WS_ADDRESSING_NS, "wsa:Address");
                    address.setTextContent("http://localhost:18181/newWSDLService/newWSDLPort");
                    fragment.appendChild(address);
                    
                    // create the wsa:ServiceName element
                    Element serviceName = document.createElementNS(WS_ADDRESSING_NS, "wsa:ServiceName");
                    String serviceNs = "http://j2ee.netbeans.org/wsdl/newWSDL";
                    String serviceLocalName = "newWSDLService";
                    serviceName.setAttributeNS("http://www.w3.org/2000/xmlns/","xmlns:ns0", serviceNs);
                    serviceName.setTextContent("ns0:" + serviceLocalName);
                    Attr port = document.createAttribute("PortName");
                    port.setValue("newWSDLPort");
                    serviceName.setAttributeNode(port);
                    fragment.appendChild(serviceName);
                    fragment = (DocumentFragment)com.sun.jbi.engine.bpel.core.bpel.util.Utility.wrapinSrefWSAEndpointRef(fragment);
                    return fragment;
                } catch (Exception ex) {
                    ex.printStackTrace();
                    throw new RuntimeException(ex);
                } 
            }
            
            
        };

        eng.setOutChannel(channel);
    }
    

}
