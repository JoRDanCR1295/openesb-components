/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.io.Reader;

import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.PartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.EPReferenceComposer;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * @author Sun Inc
 * Apr 30, 2007
 */
public class RuntimePartnerLinkImpl implements RuntimePartnerLink {

//    private static final String BPELSE_INTERNAL_SCHEME_URI = "BPELSE-internal-scheme-URI"; //no i8n
        private static final String BPELSE_INTERNAL_SCHEME_URI = "http://java.sun.com/jbi/end-point-reference"; //no i8n
    private static final String REFERENCE_SCHEME = "reference-scheme";
    
    private boolean mIsInserted;
    private PartnerLink mPLink;
    /** Either an internal representation of the EPR or a document fragment. 
     * Currently we only support the WS-Addressing schema and we don't understand 
     * anything else. Even though it is a document fragment we treat this as Document 
     * (Fragment has only one root Element, and that is the first Element too) 
     * till we attempt to support other formats of WS where may really need a Document
     * Fragment. This assumption is important for the value we store and pass it to
     * a BC or when we persist and recover the value. */
    private Object mEPR;
    private String mScopeGuid;
    
    public RuntimePartnerLinkImpl(PartnerLink pLink, String scopeGuid) {
        mPLink = pLink;
        mEPR = new InternalEPRImpl(pLink, false); 
        mScopeGuid = scopeGuid;
    }
          
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink#getServiceRef()
     */
    public Object getServiceRef() {
        return mEPR;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink#setServiceRef(java.lang.Object)
     */
    public void setServiceRef(Object epr) {
        if (epr instanceof InternalEPR) {
            mEPR = epr; 
        } else {
            // TODO do a deep copy of the document fragment
            mEPR = epr;
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink#getScopeGuid()
     */
    public String getScopeGuid() {
        return mScopeGuid;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink#setSerializedValue(java.lang.Object)
     */
    public void setSerializedValue(Object value) {
        Document doc = DOMHelper.readDocument((Reader) value);
        // if the element represents an internal type, construct RuntimePartnerLink.InternalEPR
        // else construct a document fragment.
        DocumentFragment docFrag = doc.createDocumentFragment();
        Element docElement = doc.getDocumentElement();
        docFrag.appendChild(docElement);
        NamedNodeMap attrs = docElement.getAttributes();
        boolean isInternalEPR = false;
        if (attrs != null) {
            Node attrNode = attrs.getNamedItem(REFERENCE_SCHEME);
            if (attrNode != null) {
                String referenceSchemeValue = attrNode.getNodeValue();
                if (Utility.areEqual(referenceSchemeValue, BPELSE_INTERNAL_SCHEME_URI)) {
                    isInternalEPR = true;
                }
            }
        }
        if (isInternalEPR) {
            mEPR = new InternalEPRImpl(docFrag);
        } else {
            mEPR = docFrag;
        }
        markInserted();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink#getStaticModel()
     */
    public PartnerLink getStaticModel() {
        return mPLink;
    }
    
    public static InternalEPR getEPR(PartnerLink pLink, boolean myRole) {
        return new InternalEPRImpl(pLink, myRole);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink#isInserted()
     */
    public boolean isInserted() {
        return mIsInserted;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink#markInserted()
     */
    public void markInserted() {
        mIsInserted = true;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink#getSerializedValue()
     */
    public String getSerializedValue() {
        if (mEPR instanceof InternalEPR) {
            return mEPR.toString();
        } else {
            // TODO Assumption as of now is that the document fragment of dynamic EPR
            // will only have one child node. 
            String strDocument = DOMHelper.createXmlString(((DocumentFragment) mEPR).getFirstChild());
            return strDocument;
        }
    }
    
    public static String constructInternalEndpointName(PartnerLink pLink, boolean isMyRole) {
        assert pLink != null;
        return isMyRole
                ? pLink.getMyRole()+InternalEPR.MY_ROLE_SUFFIX
                : pLink.getPartnerRole() + InternalEPR.PARTNER_ROLE_SUFFIX;
    }

    public static String constructInternalEndpointName(String roleName, boolean isMyRole) {
        assert roleName != null;
        return roleName + (isMyRole
                ? InternalEPR.MY_ROLE_SUFFIX : InternalEPR.PARTNER_ROLE_SUFFIX);
    }

    /** InternalEPR is immutable object
     * @author Sun Inc
     * Nov 1, 2007
     */
    private static final class InternalEPRImpl implements InternalEPR {

        private QName mServiceName;
        private String mEndPointName;
        private String mToStringVal;
        private DocumentFragment mDocFrag;
        private boolean mIsMyRole;

        private InternalEPRImpl(PartnerLink pLink, boolean myRole) {
            BPELProcess process = (BPELProcess) ((BPELDocument) pLink.getOwnerDocument()).getDocumentProcess();
            mServiceName = new QName(process.getTargetNamespace(), pLink.getName());
            mIsMyRole = myRole;
            mEndPointName = RuntimePartnerLinkImpl.constructInternalEndpointName(pLink, mIsMyRole);
            initEPR();
        }

        //todovb c
        private InternalEPRImpl(DocumentFragment docFrag) {
            //service-ref
            Element rootElement = (Element) docFrag.getFirstChild();
            // refer the initEPR method.

            Node serviceNameNode = rootElement.getFirstChild().getFirstChild().getNextSibling().getNextSibling().getNextSibling();
            Node eprReferenceChildNode = rootElement.getFirstChild().getFirstChild().getNextSibling().getFirstChild();
            mServiceName = DOMHelper.parseQNameContent(docFrag, serviceNameNode, EPReferenceComposer.SERVICE_NAME);
            mEndPointName = serviceNameNode.getAttributes().getNamedItem("PortName").getNodeValue();

            initEPR();
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink.InternalEPR#getEndPoint()
         */
        public String getEndPoint() {
            return mEndPointName;
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink.InternalEPR#getService()
         */
        public QName getService() {
            return mServiceName;
        }

        /** @see java.lang.Object#toString()
         */
        public String toString() {
            return mToStringVal;
        }

        public boolean isMyRole() {
            return mIsMyRole;
        }

        public DocumentFragment getAsReference() {
            return mDocFrag;
        }

        private void initEPR() {
            if (mServiceName == null || mEndPointName == null) {
                throw new IllegalStateException("BPCOR-7141: Service name and Endpoint Name should be non null.");
            }

            mToStringVal = "<sref:service-ref xmlns:sref='http://docs.oasis-open.org/wsbpel/2.0/serviceref' " +
                    REFERENCE_SCHEME + "='" + BPELSE_INTERNAL_SCHEME_URI + "' >" +
                    "<wsa:EndpointReference  xmlns:wsa='http://schemas.xmlsoap.org/ws/2004/08/addressing' " +
                    "xmlns:iepr='http://java.sun.com/jbi/end-point-reference' xmlns:sn='" +
                    mServiceName.getNamespaceURI() + "' >" +
                    " <wsa:Address/> " +
                    "<iepr:ServiceName PortName='" + mEndPointName + "'>sn:" + mServiceName.getLocalPart() +
                    "</iepr:ServiceName>" + "</wsa:EndpointReference>" + "</sref:service-ref>";


            XmlResourceProviderPool resourcePool = (XmlResourceProviderPool) BPELSERegistry.getInstance().lookup(
                    XmlResourceProviderPool.class.getName());
            XmlResourceProvider xmlResourceProvider = resourcePool.acquireXmlResourceProvider();
            Document document = xmlResourceProvider.getDocumentBuilder().newDocument();
            resourcePool.releaseXmlResourceProvider(xmlResourceProvider);
            xmlResourceProvider = null;

            mDocFrag = document.createDocumentFragment();
            Node importedElement = document.importNode(DOMHelper.createDOM(mToStringVal), true);
            mDocFrag.appendChild(importedElement);

        }
    }
}
