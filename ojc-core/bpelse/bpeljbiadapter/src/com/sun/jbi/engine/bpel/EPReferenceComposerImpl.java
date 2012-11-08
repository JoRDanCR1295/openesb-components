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
package com.sun.jbi.engine.bpel;

import com.sun.bpel.model.BPELHelper;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink.InternalEPR;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimePartnerLinkImpl;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import com.sun.bpel.model.PartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.engine.EPReferenceComposer;
import com.sun.jbi.engine.bpel.core.bpel.engine.EndpointEntity;
import com.sun.jbi.engine.bpel.core.bpel.engine.ExtEndpointEntity;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.wsdl4j.ext.NamespaceDeclarations;
import java.util.HashMap;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.w3c.dom.DocumentFragment;

/**
 *
 * @author Vitaly Bychkov
 */
public class EPReferenceComposerImpl implements EPReferenceComposer {

    private static final Logger LOGGER = Logger.getLogger(EPReferenceComposerImpl.class.getName());
    private BPELSEHelper mBPELSEHelper;

    /**
     *
     * @param bpelsehelper BPELSEHelper
     */
    public EPReferenceComposerImpl(BPELSEHelper bpelsehelper) {
        mBPELSEHelper = bpelsehelper;
    }

    public ServiceEndpoint resolveEPR(DocumentFragment docFrag) {
        ExtEndpointEntity extEP = getEndpointEntity(docFrag);
        if (extEP == null) {
            return null;
        }

        EndpointEntity inEp = extEP.getInternalEndpoint();
        return resolveEPR(docFrag, extEP, !(inEp != null && inEp.getServiceName() != null));
    }

    private ServiceEndpoint resolveEPR(DocumentFragment docFrag,
            ExtEndpointEntity extEP, boolean isExternal)
    {
        extEP = extEP != null ? extEP : getEndpointEntity(docFrag);
        if (extEP == null) {
            return null;
        }

        QName portName = null;
        QName serviceName = null;
        String epName = null;
        ServiceEndpoint servEP = null;
        if (isExternal) {
            servEP = mBPELSEHelper.getComponentContext().resolveEndpointReference(docFrag);
            if (servEP != null) {
                return servEP;
            }
        }

        portName = extEP.getInterface();

        if (isExternal) {
            serviceName = extEP.getServiceName();
            epName = extEP.getEndpointName();
        } else {
            EndpointEntity inEP = extEP.getInternalEndpoint();
            if (inEP != null) {
                serviceName = inEP.getServiceName();
                epName = inEP.getEndpointName();
            }
        }

        if (serviceName != null && epName != null) {
            servEP = mBPELSEHelper.getComponentContext().getEndpoint(serviceName, epName);
        } else if (serviceName != null && epName == null) {
            ServiceEndpoint[] servEPs = isExternal ?
                mBPELSEHelper.getComponentContext().getExternalEndpointsForService(serviceName)
                    : mBPELSEHelper.getComponentContext().getEndpointsForService(serviceName);

            if (servEPs != null && servEPs.length > 0) {
                servEP = servEPs[0];
            }
        } else if (serviceName == null && portName != null) {
            ServiceEndpoint[] servEPs = isExternal ?
                mBPELSEHelper.getComponentContext().getExternalEndpoints(portName)
                    : mBPELSEHelper.getComponentContext().getEndpoints(portName);

            if (servEPs != null && servEPs.length > 0) {
                servEP = servEPs[0];
            }
        }

        return servEP;
    }

    public ServiceEndpoint resolveEPR(DocumentFragment docFrag, boolean isExternal) {
        return resolveEPR(docFrag, null, isExternal);
    }

    public ServiceEndpoint resolveEPR(PartnerLink pl, InternalEPR iepr) {

        if (pl == null || iepr == null) {
            return null;
        }

        QName serviceName = iepr.getService();
        String endpointName = iepr.getEndPoint();
        ServiceEndpoint endpoint = mBPELSEHelper.getComponentContext().getEndpoint(serviceName, endpointName);

        if (iepr.isMyRole()) {
            return endpoint;
        }

        //partner part
        //TODOvb change after cc.getServiceDescription becomes supported
        return endpoint;
    }

    public DocumentFragment getEndpointReference(PartnerLinkScope plScope,
            PartnerLink partnerLink, boolean isMyRole)
    {

        DocumentFragment extDocFrag = null;
        RuntimePartnerLink.InternalEPR inEPR = null;

        //TODOvb c | r
        if (!isMyRole) {
            RuntimePartnerLink rPl = plScope.getRuntimePartnerLink(partnerLink);
            Object servRef = null;
            if (rPl != null) {
                servRef = rPl.getServiceRef();
            }

            if (servRef instanceof DocumentFragment) {
                inEPR = RuntimePartnerLinkImpl.getEPR(partnerLink, isMyRole);
                extDocFrag = normalize((DocumentFragment) servRef);
            } else if (servRef instanceof RuntimePartnerLink.InternalEPR) {
                inEPR = (RuntimePartnerLink.InternalEPR) servRef;
            }
        }

        if (inEPR == null) {
            inEPR = RuntimePartnerLinkImpl.getEPR(partnerLink, isMyRole);
        }
        assert inEPR != null;

        // search for the external endpoint reference...
        if (extDocFrag == null) {
            extDocFrag = normalize(getExternalEndpointReference(partnerLink, isMyRole));
        }

        DocumentFragment epRef = getEndpointReference(extDocFrag, inEPR);

        return epRef;
    }

    //todovb i
    public DocumentFragment getEndpointReference(ExtEndpointEntity extEP) {
        throw new UnsupportedOperationException();
    }

    public ExtEndpointEntity getEndpointEntity(DocumentFragment docFrag) {
        if (docFrag == null) {
            return null;
        }
        docFrag = normalize(docFrag);
        QName portType = null;

        QName extServiceName = null;
        String extEPName = null;
        String address = null;

        QName inServiceName = null;
        String inEPName = null;
        EndpointEntity inEP = null;


        Node eprNode = getEPRElement(docFrag);
        NodeList eprChildren = eprNode == null ? null : eprNode.getChildNodes();

        if (eprChildren == null) {
            return null;
        }

        for (int i = 0; i < eprChildren.getLength(); i++) {
            Node child = eprChildren.item(i);
            if (SERVICE_NAME.equalsIgnoreCase(child.getLocalName())  &&
                WS_ADDRESSING_NS.equals(child.getNamespaceURI()))
            {
                extEPName = DOMHelper.getAttributeValue(child, PORT_NAME);
                extServiceName = DOMHelper.parseQNameContent(docFrag, child, SERVICE_NAME);
            }
            else if (SERVICE_NAME.equalsIgnoreCase(child.getLocalName())  &&
                INTERNAL_WS_ADDRESSING_NS.equals(child.getNamespaceURI()))
            {
                inEPName = DOMHelper.getAttributeValue(child, PORT_NAME);
                inServiceName = DOMHelper.parseQNameContent(docFrag, child, SERVICE_NAME);
            }
            else if (PORT_TYPE.equalsIgnoreCase(child.getLocalName())  &&
                WS_ADDRESSING_NS.equals(child.getNamespaceURI()))
            {
                portType = DOMHelper.parseQNameContent(docFrag, child, PORT_TYPE);
            }
            else if (ADDRESS.equalsIgnoreCase(child.getLocalName())  &&
                WS_ADDRESSING_NS.equals(child.getNamespaceURI()))
            {
                address = child.getTextContent().trim();
            }

        }

        if (inServiceName != null) {
            inEP = new EndpointEntityImpl(inServiceName, inEPName);
        }

        return extServiceName == null && inEP == null && portType == null
                ? null : new ExternalEndpointEntityImpl(extServiceName, extEPName, address, inEP, portType);
    }

    public DocumentFragment getExternalEndpointReference(PartnerLink partnerLink) {
        return getExternalEndpointReference(partnerLink, true);
    }

    //TODOvb correct endpoint resolution by partner link
    private DocumentFragment getExternalEndpointReference(PartnerLink partnerLink, boolean isMyRole) {

        //Find out external service endpoint that.
        PortType portType = isMyRole
                ? partnerLink.getMyRoleWSDLPortType() : partnerLink.getPartnerRoleWSDLPortType();

        // we have to use Service Name and PortName(EndpointName) where possible
        QName interfaceName = portType.getQName();

        ServiceEndpoint[] endPoints =
                mBPELSEHelper.getComponentContext().getExternalEndpoints(interfaceName);


        if (endPoints == null || endPoints.length == 0) {
            return null;
        }
        DocumentFragment docFrag = endPoints[0].getAsReference(null);
        return docFrag;
    }

    private DocumentFragment normalize(DocumentFragment docFrag) {
        if (docFrag == null) {
            return null;
        }

        if (Utility.containsSingleRootElement(docFrag)) {
            Node rootEl = docFrag.getFirstChild();
            //TODOvb add type checking...
            if (!(SERVICE_REF.equalsIgnoreCase(rootEl.getLocalName())
                    && SERVICE_REF_NS.equalsIgnoreCase(rootEl.getNamespaceURI())))
            {
                docFrag = (DocumentFragment)Utility.wrapinServiceRef(docFrag);
            }
        } else {
            docFrag = (DocumentFragment)Utility.wrapinSrefWSAEndpointRef(docFrag);
        }

        return docFrag;
    }

    //TODOvb check performance vs direct DOM parsing
    private ServiceEndpoint extractInternalEP(DocumentFragment frag) {
        Map<String, String> prefixMap = new HashMap<String, String>();
        prefixMap.put("wsa", WS_ADDRESSING_NS);
        prefixMap.put("iepr", INTERNAL_WS_ADDRESSING_NS);

        //extract serviceName
        JXPathContext xc = Utility.newJXPathContext(frag, getNamespaceDeclarations(prefixMap));
        Pointer pointer = xc.getPointer("wsa:EndpointReference/iepr:ServiceName");
        Object value = pointer.getNode();
        if (value == null) {
            return null;
        }
        Node iep = (Node) value;
        String iepName = iep.getTextContent();
        int beginIndex = iepName.indexOf(':');
        String snprefix = iepName.substring(0, beginIndex);
        String localPart = iepName.substring(beginIndex + 1);
        QName serviceName = new QName(iep.lookupNamespaceURI(snprefix), localPart);

        //extract endpointName
        xc = Utility.newJXPathContext(value, getNamespaceDeclarations(prefixMap));
        pointer = xc.getPointer("@PortName");
        String endpointName = (String) pointer.getValue();
        ServiceEndpoint endpointReference = (mBPELSEHelper.getComponentContext()).getEndpoint(serviceName, endpointName);
        return endpointReference;
    }

    //TODOvb check performance vs direct DOM parsing
    private NamespaceDeclarations getNamespaceDeclarations(final Map<String, String> prefixMap) {
        NamespaceDeclarations nsDeclarations = new NamespaceDeclarations() {

            public Map<String, String> getAll() {
                return prefixMap;
            }

            public String lookUpNamespaceURI(String prefix) {
                return prefixMap.get(prefix);
            }

            public String lookUpNamespacePrefix(String uri) {
                if (prefixMap == null || uri == null) {
                    return null;
                }

                Set<String> keys = prefixMap.keySet();
                for (String key : keys) {
                    if (uri.equals(prefixMap.get(key))) {
                        return key;
                    }
                }
                return null;
            }

            public boolean isEmpty() {
                return prefixMap == null || prefixMap.isEmpty();
            }
        };
        return nsDeclarations;
    }

    // extDocFrag have to be normalized
    private DocumentFragment getEndpointReference(Node extDocFrag,
            RuntimePartnerLink.InternalEPR inEPR)
    {
        // there is no external EP
        if (extDocFrag == null) {
            return inEPR.getAsReference();
        }


        XmlResourceProviderPool resourcePool = (XmlResourceProviderPool) BPELSERegistry.getInstance().lookup(
                XmlResourceProviderPool.class.getName());
        XmlResourceProvider xmlResourceProvider = resourcePool.acquireXmlResourceProvider();
        Document document = xmlResourceProvider.getDocumentBuilder().newDocument();
        resourcePool.releaseXmlResourceProvider(xmlResourceProvider);
        xmlResourceProvider = null;

        DocumentFragment df = document.createDocumentFragment();
        Node importedElement = document.importNode(extDocFrag, true);
        df.appendChild(importedElement);

        //since extDocFrag comes in normalized mode i.e. ServiceReference is the first child
        Node eprNode = getEPRElement(df);
        NodeList children = eprNode != null ? eprNode.getChildNodes() : null;;

        if (children == null) {
            return df;
        }

//        for (int i = 0; i < children.getLength(); i++) {
//            Node child = children.item(i);
//
//            if (SERVICE_NAME.equalsIgnoreCase(child.getLocalName())
//                    && WS_ADDRESSING_NS.equalsIgnoreCase(child.getNamespaceURI()))
//            {

        Element serviceNameEl = document.createElementNS(INTERNAL_WS_ADDRESSING_NS, SERVICE_NAME);
        serviceNameEl = (Element) eprNode.appendChild(serviceNameEl);

        serviceNameEl.setAttribute(PORT_NAME, inEPR.getEndPoint());
        //
        QName serviceName = inEPR.getService();
        String nsPrefix = addXMLNS(serviceNameEl, serviceName.getNamespaceURI());

        Text serviceNameVal = document.createTextNode(nsPrefix + DOMHelper.COLUMN + serviceName.getLocalPart());
        serviceNameEl.appendChild(serviceNameVal);
//            }
//        }

        return df;
    }

    //TODOvb m
    private Node getEPRElement(DocumentFragment docFrag) {
        if (docFrag == null) {
            return null;
        }
        // docFrag have to be normalized i.e. rootEl is ServiceRef
        Node sref = docFrag.getFirstChild();
        assert sref != null;

        Node eprNode = null;
        NodeList children  = sref.getChildNodes();
        Node tmpNode = null;
        for (int i = 0; i < children.getLength(); i++) {
            tmpNode = children.item(i);
            //because of service-ref schema just one child node is allowed
            if (tmpNode instanceof Element) {
                //TODO check element type it have to be or EndpoiontReferenceType or one of extended
                eprNode = tmpNode;
                break;
            }
        }

        return eprNode;
    }

    private String addXMLNS(Element el, String nsURI) {
        assert el != null && nsURI != null;
        String nsPrefix = getFreePrefix(el, DOMHelper.BASE_PREFIX, 0);

        el.setAttributeNS(DOMHelper.XMLNS_URI, DOMHelper.XMLNS_PREFIX + DOMHelper.COLUMN + nsPrefix, nsURI);
        return nsPrefix;
    }

    private String getFreePrefix(Node el, String prefix, int startPoint) {
        assert startPoint > -1 && el != null && prefix != null;
        String nsPrefix = prefix + startPoint;
        return el.lookupNamespaceURI(nsPrefix) == null ? nsPrefix : getFreePrefix(el, prefix, startPoint++);
    }
}
