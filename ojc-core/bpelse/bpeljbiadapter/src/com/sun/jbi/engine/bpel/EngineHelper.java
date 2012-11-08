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
 * @(#)EngineHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import net.sf.hulp.measure.Measurement;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.EntityResolver;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.DefaultWSDLResolverFactory;
import com.sun.bpel.model.DefaultXSDResolverFactory;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.impl.RInvokeImpl;
import com.sun.bpel.model.parser.impl.ParseContextImpl;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.model.visitor.IXSDResolver;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.descriptor.ServicesDescriptor;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModelFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.impl.BPELSEException;
import com.sun.jbi.engine.bpel.util.I18n;
import com.sun.jbi.engine.bpel.util.SUArtifacts;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * EngineHelper class
 * 
 * @author Sun Microsystems
 */
public class EngineHelper {

    private static Logger MLOGGER = Logger.getLogger(EngineHelper.class.getName());

    private static HashSet loadDefinitions(SUArtifacts suArtifacts, File deploydir) throws BPELSEException {
        HashSet ret = new HashSet();
        URL url = null;
        InputStream is = null;
        InputStreamReader reader = null;

        List bpelfilepaths = findBPELFiles(deploydir);

        String bpelfilepath = null;

        try {
            ParsingCaches caches = new ParsingCaches();
            DeferredActionRegistry registry = new DeferredActionRegistry();
            EntityResolver entityResolver = null;
            for (int i = 0; i < bpelfilepaths.size(); i++) {
                bpelfilepath = (String) bpelfilepaths.get(i);
                File bpelFile = new File(bpelfilepath);
                String bpelFileURI = bpelFile.toURI().toString();
                // try {
                url = new URL("file", null, bpelfilepath); // NO I18N at present
                is = url.openStream();
                reader = new InputStreamReader(is, "UTF-8");

                BPELParseContext parseContext = new ParseContextImpl();

                parseContext.setCatalog(deploydir, bpelFile);
                if (parseContext.hasCatalog()) {
                    entityResolver = parseContext.getBaseURIResolver();
                }
                IWSDLResolver wsdlResolver = DefaultWSDLResolverFactory.getInstance().newWSDLResolver(
                        bpelFileURI, parseContext);
                parseContext.setWSDLResolver(wsdlResolver);

                // set the xsd resolver
                IXSDResolver xsdLoader = DefaultXSDResolverFactory.getInstance().newXSDResolver(
                        bpelFileURI, parseContext);
                parseContext.setXSDResolver(xsdLoader);

                caches.setCurrentBaseURI(bpelfilepath);
                parseContext.setCaches(caches);
                parseContext.setDeferredActionRegistry(registry);
                BPELDocument bpelDoc = BPELDocumentParseFactory.getInstance().load(reader,
                        parseContext);
                bpelDoc.setBaseURI(bpelfilepath);

                RBPELProcess bProc = (RBPELProcess) bpelDoc.getDocumentProcess();

                ret.add(bProc);
                reader.close();
            }
            WSDL4JExt.applySingleSchemaTypeLoader(registry, entityResolver);

            // put xsl stylesheet locations into SUArtifacts
            for (String xsl : caches.getXslCache()) {
                suArtifacts.getXslCache().addStylesheet(xsl, null);
            }
        } catch (MalformedURLException ex) {
            throw new BPELSEException(I18n.loc("BPJBI-6022: Bpel file url {0} is malformed", bpelfilepath), ex);
        } catch (IOException ex) {
            throw new BPELSEException(I18n.loc("BPJBI-6023: Unable to open bpel file stream for {0}", bpelfilepath), ex);
        } catch (Exception ex) {
            throw new BPELSEException(I18n.loc("BPJBI-6024: Error while loading BPEL file {0}, Exception details are {1}",
                    bpelfilepath, ex.getMessage()), ex);
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ex) {
                    throw new BPELSEException(ex);
                }
            }

            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException ex) {
                    throw new BPELSEException(ex);
                }
            }
        }

        return ret;
    }

    private static List findBPELFiles(File dir) {
        List bpelFilesList = new ArrayList();
        File[] files = dir.listFiles();

        for (int i = 0; i < files.length; i++) {
            File file = files[i];

            if (file.isFile() && file.getName().endsWith(".bpel")) { // NO I18N
                bpelFilesList.add(file.getAbsolutePath());
            } else if (file.isDirectory()) {
                bpelFilesList.addAll(findBPELFiles(file));
            }
        }

        return bpelFilesList;
    }

    /**
     * deploy bpelse service units
     * 
     * @param suName The name of the service unit
     * @param deploydir file object of JBI deploy directory
     * @param deployLookup handle/helper to parse deployment information
     * @param epsHelper EndpointStatisticsHelper
     * @throws BPELSEException BPEL service engine exception
     * @throws DeploymentException 
     */
    public static SUArtifacts deploy(String suName, File deploydir,
            DeploymentLookup deployLookup)
            throws BPELSEException, DeploymentException {
        Measurement m = Measurement.begin(BPELSEDeployer.CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.processArtifacts.loadAssembly");
        SUArtifacts suArtifacts = new SUArtifacts(deployLookup.getQoSAssembly(suName), suName);
        m.end();

        m = Measurement.begin(BPELSEDeployer.CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.processArtifacts.loadArtifacts");
        suArtifacts.setBPs(loadDefinitions(suArtifacts, deploydir));
        m.end();

        MLOGGER.log(Level.INFO, I18n.loc("BPJBI-4010: Loaded BPEL, WSDL and XSD documents for {0}.", suName));

        ServiceUnit unit = null;
        Measurement m1 = Measurement.begin(BPELSEDeployer.CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.processArtifacts.providesEPs");
        Measurement m2 = Measurement.begin(BPELSEDeployer.CLASSIDENTIFIER_FORMEASUREMENT,
                "SU.init.processArtifacts.providesEPs.readSUDescriptor");
        try {
            // Get the Consumes and Provides endpoint information from the SU descriptor
            unit = ServicesDescriptor.parse(suName, deploydir.getPath());
        } catch (Exception ex) {
            throw new BPELSEException(I18n.loc("BPJBI-6025: Exception occured while processing the descriptor file " +
                    "for the service unit {0}.", suName), ex);
        } finally {
            m2.end();
        }

        EndpointInfo[] providesEndpoints = unit.getServices().getProvides();
        if (providesEndpoints == null) {
            throw new BPELSEException(
                    I18n.loc("BPJBI-6026: BPEL SE can't be deployed without any provisioning artifacts."));
        }

        m2 = Measurement.begin(BPELSEDeployer.CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.processArtifacts.providesEPs.identifyIMA");

        for (int i = 0; i < providesEndpoints.length; i++) {
            EndpointInfo providesEntry = providesEndpoints[i];
            QName serviceName = providesEntry.getServiceName();
            String endPointName = providesEntry.getEndpointName();

            if ((serviceName == null) || (endPointName == null) || (providesEntry.getInterfaceName() == null)) {
                throw new BPELSEException(I18n.loc("BPJBI-6027: One of the values in the provides element in the " +
                        "SU descriptor is not specified. Service name: {0}, endpoint name {1}, interface name: {2}",
                        serviceName, endPointName,
                        providesEntry.getInterfaceName()));
            }

            HashMap matchingStartElementsMap = getMatchingStartElements(providesEntry, suArtifacts.getBPs());

            if (matchingStartElementsMap.isEmpty()) {
                // Mostly this is an un-used partnerLink, ignore it, since
                // we do not have support for them.
                continue;
            }


            Set startElementSet = matchingStartElementsMap.entrySet();
            Iterator startElementIter = startElementSet.iterator();

            while (startElementIter.hasNext()) {
                Map.Entry startElementMapEntry = (Map.Entry) startElementIter.next();
                RStartElement startElement = (RStartElement) startElementMapEntry.getKey();
                RBPELProcess bpModel = (RBPELProcess) startElementMapEntry.getValue();
                // the throttling parameter needs to be set on the process manager only
                // for startElements that have createInstance = 'yes'.
                boolean isCreate = startElement.getRCreateInstance();
                if (isCreate) {
                    Measurement m3 = Measurement.begin(BPELSEDeployer.CLASSIDENTIFIER_FORMEASUREMENT,
                            "SU.init.processArtifacts.providesEPs.identifyIMA.ThrottlingCount");
                    int throttlingCount = getThrottlingCount(suArtifacts, providesEntry);
                    m3.end();
                    suArtifacts.setMaxInstanceCountByBP(bpModel.getBPELId(), throttlingCount);
                }

                Operation op = startElement.getWSDLOperation();
                String pattern = Engine.IN_ONLY;
                if (op.getOutput() != null) {
                    pattern = Engine.IN_OUT;
                }

                InComingEventModel model = InComingEventModelFactory.createModel(bpModel,
                        startElement, pattern);
                InComingKey key = DeploymentBindings.createInComingBindingsKey(serviceName, endPointName,
                        op.getName());
                suArtifacts.addIncomingEventModel(key, model);
            }
            suArtifacts.addProviderEndpoint(new ActivationEntry(serviceName, endPointName));
        }
        m2.end();
        m1.end();

        m1 = Measurement.begin(BPELSEDeployer.CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.processArtifacts.consumeEPs");
        EndpointInfo[] consumesEndpoints = unit.getServices().getConsumes();
        if (consumesEndpoints != null) {
            for (int i = 0; i < consumesEndpoints.length; i++) {
                EndpointInfo consumesEntry = consumesEndpoints[i];
                QName serviceName = consumesEntry.getServiceName();
                String endPointName = consumesEntry.getEndpointName();
                QName interfaceName = consumesEntry.getInterfaceName();

                if ((serviceName == null) || (endPointName == null) || (consumesEntry.getInterfaceName() == null)) {
                    throw new BPELSEException(I18n.loc("BPJBI-6028: One of the values in the consumes element in " +
                            "the SU descriptor is not specified. Service name: {0}, endpoint name {1}, " +
                            "interface name: {2}", serviceName, endPointName, interfaceName));
                }

                //register QoS on Invoke. 
                m2 = Measurement.begin(BPELSEDeployer.CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.processArtifacts.consumeEPs.registerQoS");
                EndpointInfo info = new EndpointInfo(false, endPointName, interfaceName, serviceName, null);
                List<ServiceQuality> qosList =
                        suArtifacts.getQoSAssembly().getServiceQualities().get(info);
                if (qosList != null && !qosList.isEmpty()) {
                    setQosParamsOnInvoke(info, qosList, suArtifacts.getBPs());
                }
                m2.end();

                if (MLOGGER.isLoggable(Level.FINE)) {
                    MLOGGER.log(Level.FINE,
                            I18n.loc("BPJBI-3021: Consumes Endpoint, service name {0}, endpoint name {1}",
                            serviceName, endPointName));
                }
            }
        }
        m1.end();
        return suArtifacts;
    }

    private static int getThrottlingCount(SUArtifacts artifacts,
            EndpointInfo provider) {
        int throttleVal = 0;
        // lookup all the consumers that are connected to this provisioning endpoint.
        EndpointInfo[] consumes =
                artifacts.getQoSAssembly().getConsumersByProvider(provider);
        Map<EndpointInfo, List<ServiceQuality>> sMap =
                artifacts.getQoSAssembly().getServiceQualities();
        if (consumes != null && consumes.length > 0 && sMap != null && !sMap.isEmpty()) {
            for (int i = 0, I = consumes.length; i < I; i++) {
                List<ServiceQuality> qosList = sMap.get(consumes[i]);
                if (qosList != null) {
                    for (Iterator<ServiceQuality> iter = qosList.iterator(); iter.hasNext();) {
                        ServiceQuality qos = iter.next();
                        if (qos instanceof ThrottlingConfig) {
                            int tCount = ((ThrottlingConfig) qos).getMaxConcurrencyLimit();
                            if (throttleVal == 0) {
                                throttleVal = tCount;
                            } else if (tCount < throttleVal) {
                                throttleVal = tCount;
                            }
                        }
                    }
                }
            }
        }

        return throttleVal;
    }

    private static void setQosParamsOnInvoke(EndpointInfo endpointInfo,
            List<ServiceQuality> qosList,
            HashSet loadedBPs) {
        Iterator bpItr = loadedBPs.iterator();
        while (bpItr.hasNext()) {
            RBPELProcess bpelProcess = (RBPELProcess) bpItr.next();
            Set<Invoke> invokeActivities = bpelProcess.getInvokeElements();
            QName interfaceName = endpointInfo.getInterfaceName();
            QName serviceName = endpointInfo.getServiceName();
            for (Invoke act : invokeActivities) {
                // Check whether the invoke element matches the endpoint.
                RInvokeImpl rInvoke = (RInvokeImpl) act;
                PartnerLink partnerLink = rInvoke.getRPartner();

                if (!partnerLink.getName().equals(serviceName.getLocalPart())) {
                    continue;
                }

                QName startElementPortType = rInvoke.getRPortType();

                if (!interfaceName.equals(startElementPortType)) {
                    continue;
                }
                // this rInvoke matches the consumer endpoint.
                rInvoke.setServiceQualityParams(endpointInfo, qosList);
            }

        }

    }

    private static EndpointInfo getQosEndpointInfo(EndpointIdentifier identifier,
            Set<EndpointInfo> endpointInfoSet) {
        for (EndpointInfo info : endpointInfoSet) {
            if (info.getInterfaceName().equals(identifier.getInterfaceName()) && info.getServiceName().equals(identifier.getServiceName()) && info.getEndpointName().equals(identifier.getEndpointName())) {
                return info;
            }
        }
        return null;
    }

    private static HashMap getMatchingStartElements(EndpointInfo providesElement, HashSet loadedBPs) {
        HashMap startElementMap = new HashMap();
        javax.xml.namespace.QName interfaceName = providesElement.getInterfaceName();
        javax.xml.namespace.QName serviceName = providesElement.getServiceName();

        Iterator bpsit = loadedBPs.iterator();

        while (bpsit.hasNext()) {
            RBPELProcess bpModel = (RBPELProcess) bpsit.next();

            if (!serviceName.getNamespaceURI().equals(bpModel.getTargetNamespace())) {
                continue;
            }

            // Currently we iterate through all the BPs which have a matching namespace
            Iterator startElementsIter = bpModel.getStartElements().iterator();

            while (startElementsIter.hasNext()) {
                RStartElement startElement = (RStartElement) startElementsIter.next();

                // Check whether the start element matches the providesElement
                PartnerLink partnerLink = startElement.getRPartner();

                if (!partnerLink.getName().equals(serviceName.getLocalPart())) {
                    continue;
                }

                javax.xml.namespace.QName startElementPortType = startElement.getRPortType();

                if (!interfaceName.equals(startElementPortType)) {
                    continue;
                }

                startElementMap.put(startElement, bpModel);
            }
        }

        return startElementMap;
    }

    /**
     * get status message
     * 
     * @param mep InOut message exchange
     * @param transformer XML transforemer
     * @return WSMessage webservice message
     */
    public static WSMessage getStatusMessage(InOut mep) {
        return getStatusMessage(mep.getOutMessage());
    }

    /**
     * For attribute values which denote a QName, i.e. include a namespace prefix, resolve the value
     * into a QName. If a namespace can not be resolved, it is set to empty - it does not result in
     * an exception
     * 
     * @param qnameValue the string value of the attribute
     * @param element the element the attribute belongs to
     * @return DOCUMENT ME!
     */
    private static QName resolveQName(String qnameValue, Element element) {
        String localName = null;
        String prefix = null;
        String namespaceUri = null;

        int colonLoc = qnameValue.indexOf(":");

        if (colonLoc > -1) {
            prefix = qnameValue.substring(0, colonLoc);
            localName = qnameValue.substring(colonLoc + 1);

            // Traverse up the hierarchy until a namespace definition is found
            // or the top of the document is reached.
            Node currNode = element;

            while ((namespaceUri == null) && (currNode != null)) {
                if (currNode.getNodeType() == Node.ELEMENT_NODE) {
                    namespaceUri = ((Element) currNode).getAttribute("xmlns:" + prefix);
                }

                currNode = currNode.getParentNode();
            }
        } else {
            localName = qnameValue;
        }
        if (prefix != null) {
            return new QName(namespaceUri, localName, prefix);
        } else {
            return new QName(namespaceUri, localName);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param normalizedMsg DOCUMENT ME!
     * @param transformer DOCUMENT ME!
     * @param process DOCUMENT ME!
     * @return DOCUMENT ME!
     * @throws RuntimeException DOCUMENT ME!
     */
    public static WSMessage createWSMessage(NormalizedMessage normalizedMsg,
            RBPELProcess process,
            boolean isFaultMessage) {
        if (normalizedMsg == null) {
            throw new RuntimeException(I18n.loc("BPJBI-3022: No Message found"));
        }
        WSMessage wsMessage = null;
        Message wsdlMessage = null;

        //if there is no content
        Source source = normalizedMsg.getContent();
        if (source == null) {
            throw new RuntimeException(I18n.loc("BPJBI-6029: The content of the NormalizedMessage is not a JBI " +
                    "WSDL 1.1 wrapped message {0}", normalizedMsg));
        }

        Document jbiDocument = null;
        Element jbiMessage = null;

        if (!(source instanceof DOMSource)) {

            DOMResult result = new DOMResult();

            XmlResourceProviderPool xmlResProviderpool = (XmlResourceProviderPool) BPELSERegistry.getInstance().lookup(
                    XmlResourceProviderPool.class.getName());
            XmlResourceProvider xmlResourceProvider = xmlResProviderpool.acquireXmlResourceProvider();
            Transformer transformer = xmlResourceProvider.getTransformer();

            try {
                transformer.transform(source, result);
            } catch (TransformerException ex) {
                throw new RuntimeException(I18n.loc("BPJBI-6030: Error occured while transforming the " +
                        "normalized message {0}", normalizedMsg), ex);
            } finally {
                transformer = null;
                xmlResProviderpool.releaseXmlResourceProvider(xmlResourceProvider);
                xmlResourceProvider = null;
            }

            jbiDocument = (Document) result.getNode();
            jbiMessage = jbiDocument.getDocumentElement();

        } else {
            Node sourceNode = ((DOMSource) source).getNode();
            if (sourceNode instanceof Document) {
                jbiDocument = (Document) sourceNode;
                jbiMessage = jbiDocument.getDocumentElement();
            } else {
                jbiDocument = wrapInDocument((Element) sourceNode);
                jbiMessage = jbiDocument.getDocumentElement();
            }
        }

        // Check whether the message passed is a valid
        // WSDL 1.1 wrapper
        if (jbiMessage != null && "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper".equals(jbiMessage.getNamespaceURI()) && "message".equals(jbiMessage.getLocalName())) {

            String typeValue = jbiMessage.getAttribute("type");

            if ((typeValue == null) || typeValue.equals("")) {
                throw new RuntimeException(I18n.loc("BPJBI-6031: The value of attribute \"type\" is null or empty " +
                        "in the jbi message element for the normalized message {0}", normalizedMsg));
            }
            QName wsdlMessageType = resolveQName(typeValue, jbiMessage);
            wsdlMessage = process.getWSDLMessage(wsdlMessageType);

            if (wsdlMessage == null) {
                throw new RuntimeException(I18n.loc("BPJBI-6032: Could not find the messageType definition {0}, " +
                        "specified by the type attribute in jbi:message element.", wsdlMessageType.toString()));
            }

            wsMessage = new JBIMessageImpl(jbiDocument, wsdlMessage);
            Set<String> propNames = normalizedMsg.getPropertyNames();
            for (String propName : propNames) {
                Object val = normalizedMsg.getProperty(propName);
                wsMessage.setNMProperty(propName, val);
            }

            Set<String> attachmentNames = normalizedMsg.getAttachmentNames();
            for (String atchName : attachmentNames) {
                DataHandler attch = normalizedMsg.getAttachment(atchName);
                if (attch == null) {
                    throw new RuntimeException(I18n.loc("BPJBI-6034: Attachment for name {0} is null", atchName));
                }
                wsMessage.setAttachment(atchName, attch);
            }

            return wsMessage;
        }

        /*
         * Presently we only support messages that are sent as a WSDL 1.1 wrapped In case, they are
         * not, then these messages are mostly server faults, and the data that was received cannot
         * be mapped to any message defined in the WSDL Since we have no mechanism to provide this
         * data along with the fault, we return null. For a request or a response message, we throw
         * an exception, since they cannot be null or not WSDL 1.1 wrapped messages.
         */

        if (isFaultMessage) {
            return null;
        } else {
            throw new RuntimeException(I18n.loc("BPJBI-6029: The content of the NormalizedMessage is not a " +
                    "JBI WSDL 1.1 wrapped message {0}", normalizedMsg));
        }
    }

    private static Document wrapInDocument(Element element) {
        XmlResourceProviderPool resourcePool = (XmlResourceProviderPool) BPELSERegistry.getInstance().lookup(
                XmlResourceProviderPool.class.getName());
        XmlResourceProvider xmlResourceProvider = resourcePool.acquireXmlResourceProvider();
        Document document = xmlResourceProvider.getDocumentBuilder().newDocument();
        resourcePool.releaseXmlResourceProvider(xmlResourceProvider);
        xmlResourceProvider = null;

        Node importedElement = document.importNode(element, true);
        document.appendChild(importedElement);

        return document;
    }

    /**
     * retrieve status message
     * 
     * @param msg normalized message
     * @return WSMessage webservice message
     * @param transformer XML transforemer
     * @throws RuntimeException DOCUMENT ME!
     */
    public static WSMessage getStatusMessage(NormalizedMessage msg) {
        WSMessage domMsg = null;
        DOMResult result = new DOMResult();

        XmlResourceProviderPool xmlResProviderpool = (XmlResourceProviderPool) BPELSERegistry.getInstance().lookup(
                XmlResourceProviderPool.class.getName());
        XmlResourceProvider xmlResourceProvider = xmlResProviderpool.acquireXmlResourceProvider();
        Transformer transformer = xmlResourceProvider.getTransformer();

        try {
            transformer.transform(msg.getContent(), result);
        } catch (TransformerException ex) {
            throw new RuntimeException(I18n.loc("BPJBI-6030: Error occured while transforming the " +
                    "normalized message {0}", msg.getContent()), ex);
        } finally {
            transformer = null;
            xmlResProviderpool.releaseXmlResourceProvider(xmlResourceProvider);
            xmlResourceProvider = null;
        }

        try {
            Document doc = (Document) result.getNode();
            WrapperBuilder builder = HelperFactory.createBuilder();
            domMsg = new JBIMessageImpl(doc, Utility.getCommonMessage(builder.getStatusMessage()));
        } catch (WrapperProcessingException ex) {
            throw new RuntimeException(I18n.loc("BPJBI-6036: failed to get WSDL Factory"), ex);
        }

        return domMsg;
    }

    /**
     * ActivationMap class
     */
    public static class ActivationEntry {

        private QName serviceName;
        private String endpointName;
        private int hash;

        /**
         * constructor
         * 
         * @param service Service QName
         * @param endpoint Endpoint local name
         */
        public ActivationEntry(QName service, String endpoint) {
            serviceName = service;
            endpointName = endpoint;
            hash = serviceName.hashCode() + endpointName.hashCode();
        }

        /**
         * get service name
         * 
         * @return QName service name
         */
        public QName getServiceName() {
            return serviceName;
        }

        /**
         * get endpoint name
         * 
         * @return String endpoint name
         */
        public String getEndpointName() {
            return endpointName;
        }

        public int hashCode() {
            return hash;
        }

        public boolean equals(Object obj) {
            if (obj == null || !(obj instanceof ActivationEntry)) {
                return false;
            }
            if (obj == this) {
                return true;
            }

            boolean retVal = false;
            ActivationEntry entry = (ActivationEntry) obj;
            if (entry.getServiceName().equals(serviceName) && entry.getEndpointName().equals(endpointName)) {
                retVal = true;
            }

            return retVal;
        }
    }
}
