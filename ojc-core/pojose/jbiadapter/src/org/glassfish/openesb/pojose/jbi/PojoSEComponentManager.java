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
 * @(#)PojoSEComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi;

import com.sun.jbi.common.descriptor.EndpointInfo;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.core.pool.DocumentBuilderPool;
import org.glassfish.openesb.pojose.core.util.Constants;
import org.glassfish.openesb.pojose.core.util.Util;
import org.glassfish.openesb.pojose.jbi.su.PojoSEServiceUnit;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;


/**
 * POJOservice engine implementation.
 * 
 * @author Girish Patil
 */
public class PojoSEComponentManager implements Component {
    private Logger logger = Logger.getLogger(PojoSEComponentManager.class.getName());
    private POJOComponentContext pojoCompCtx = new POJOComponentContext();    

    public ComponentLifeCycle getLifeCycle() {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("POJOSE-1516: Returning PojoSELifeCycle instance.");//NOI18N
            logger.log(Level.FINEST, msg);
        }
        return pojoCompCtx.getCompLifeCycle();
    }

    public Document getServiceDescription(ServiceEndpoint se) {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("POJOSE-1517: Got request for Service description for endpoint {0}.", se);//NOI18N
            logger.log(Level.FINEST, msg);
        }

        return getSvcDescForSEP(se);
    }

    public ServiceUnitManager getServiceUnitManager() {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("POJOSE-1518: Returning ServiceUnitManager.");//NOI18N
            logger.log(Level.FINEST, msg);
        }
        return this.pojoCompCtx.getServiceUnitManager();
        
    }

    public boolean isExchangeWithConsumerOkay(ServiceEndpoint se, MessageExchange me) {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("POJOSE-1519: isExchangeWithConsumerOkay invoked for ServiceEndpoint {0} with ME {1}.", se, me );//NOI18N
            logger.log(Level.FINEST, msg);
        }
        
        return true;
    }

    public boolean isExchangeWithProviderOkay(ServiceEndpoint se, MessageExchange me) {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("POJOSE-1520: isExchangeWithProviderOkay invoked for ServiceEndpoint {0} with ME {1}.", se, me );//NOI18N
            logger.log(Level.FINEST, msg);
        }
        
        return true;
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment df) {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("POJOSE-1521: resolveEndpointReference invoked.");//NOI18N
            logger.log(Level.FINEST, msg);
        }

        return null;
    }

    private Document getSvcDescForSEP(ServiceEndpoint sept){
        Document ret = null;
        PojoSEServiceUnit su = this.pojoCompCtx.getPojoSU(sept);
        
        if (su != null){
            POJOClassMetadata pojoMeta = su.getPojoClassMetadata(sept);
            EndpointInfo ei = Util.getEndpointInfo(pojoMeta);
            QName outMsgType = null;
            String opName = null;
            
            if (pojoMeta.getOperationMetadata().getOperation().outMessageTypeQN() != null){
                outMsgType = QName.valueOf(pojoMeta.getOperationMetadata().getOperation().outMessageTypeQN());
            } else {
                outMsgType = new QName(ei.getServiceName().getNamespaceURI(), ei.getEndpointName() + "OperationResponse");
            }

            if (pojoMeta.getOperationMetadata().getOperation().name() == null){
                opName = ei.getEndpointName() + "Operation" ;
            }

            ret = getAbstractWSDLDoc(ei.getServiceName(), ei.getInterfaceName(),
                    opName,
                    pojoMeta.getMEPStyle().equals(Util.MEPStyle.InOut), 
                    outMsgType,
                    ei.getEndpointName());
        }

        return ret;
    }

    private Document getAbstractWSDLDoc(QName svc, QName pt, String opName,
            boolean inOut, QName outMsgName, String endpointName){

        Document doc = null;

        DocumentBuilder db = null;
        try {
            db = DocumentBuilderPool.getInstance().acquire();
            doc = db.newDocument();
        } finally {
            if (db != null){
                DocumentBuilderPool.getInstance().release(db);
            }
        }
        
        doc.setXmlVersion("1.0"); //NOI18N
        Element wsdlDef = doc.createElementNS(Constants.WSDL_1_1_NS, "definitions"); //NOI18N
        wsdlDef.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, "xmlns:xsd", Constants.XSD_NS);
        wsdlDef.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, "xmlns:tns", svc.getNamespaceURI());
        wsdlDef.setAttribute("targetNamespace", svc.getNamespaceURI());
        doc.appendChild(wsdlDef);

        Element type = doc.createElementNS(Constants.WSDL_1_1_NS, "types");
        wsdlDef.appendChild(type);

        Element inMsg = doc.createElementNS(Constants.WSDL_1_1_NS, "message");
        inMsg.setAttribute("name", endpointName + "OperationRequest");
        wsdlDef.appendChild(inMsg);

        Element part1 = doc.createElementNS(Constants.WSDL_1_1_NS, "part");
        part1.setAttribute("name", "part1");
        part1.setAttribute("type", "xsd:anyType");
        inMsg.appendChild(part1);

        if (inOut){
            Element outMsg = doc.createElementNS(Constants.WSDL_1_1_NS, "message");
            outMsg.setAttribute("name", outMsgName.getLocalPart());
            wsdlDef.appendChild(outMsg);

            Element opart = doc.createElementNS(Constants.WSDL_1_1_NS, "part");
            opart.setAttribute("name", "part1");
            opart.setAttribute("type", "xsd:anyType");
            outMsg.appendChild(opart);
        }

        Element portType = doc.createElementNS(Constants.WSDL_1_1_NS, "portType");
        portType.setAttribute("name", pt.getLocalPart());
        wsdlDef.appendChild(portType);

        Element operation = doc.createElementNS(Constants.WSDL_1_1_NS, "operation");
        operation.setAttribute("name", opName);
        portType.appendChild(operation);

        Element in1 = doc.createElementNS(Constants.WSDL_1_1_NS, "input");
        in1.setAttribute("name", "input1");
        in1.setAttribute("message", "tns:" + endpointName + "OperationRequest");
        operation.appendChild(in1);

        if (inOut){
            Element out1 = doc.createElementNS(Constants.WSDL_1_1_NS, "output");
            out1.setAttribute("name", "output1");
            out1.setAttribute("message", "tns:" + outMsgName.getLocalPart());
            operation.appendChild(out1);
        }

        return doc;
    }
}

