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
 * @(#)EmailWSDLConfigurations.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.net.URL;

import javax.jbi.management.DeploymentException;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.wsdl.Input;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.xml.sax.EntityResolver;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.encoder.tools.xml.XsdLocator;
import com.sun.jbi.binding.email.EmailBCEndpoint;
import com.sun.jbi.binding.email.I18n;
import com.sun.jbi.binding.email.protocol.wsdl.EmailBCExtSerializer;
import com.sun.jbi.binding.email.protocol.wsdl.EmailBCExtensionRegistry;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPAddress;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPBinding;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPOperation;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPOperationInput;
import com.sun.jbi.binding.email.protocol.wsdl.POP3Address;
import com.sun.jbi.binding.email.protocol.wsdl.POP3Binding;
import com.sun.jbi.binding.email.protocol.wsdl.POP3Operation;
import com.sun.jbi.binding.email.protocol.wsdl.POP3OperationInput;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPAddress;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPBinding;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPOperation;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPOperationInput;
import com.sun.jbi.binding.email.protocol.send.smtp.EmailDenormalizer;
import com.sun.jbi.binding.email.protocol.send.smtp.SMTPEndpoint;
import com.sun.jbi.binding.email.protocol.receive.pop3.POP3Endpoint;
import com.sun.jbi.binding.email.protocol.receive.imap.IMAPEndpoint;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.config.AppConfig;
import com.sun.jbi.common.qos.descriptor.QosServices;
import com.sun.jbi.common.qos.descriptor.QosServicesDescriptor;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 * @author Shivanand Kini (shivanand.kini@sun.com)
 */
public class EmailWSDLConfigurations {

    private ManagerContext mMgrCtx;
    private EndpointInfo mEptInfo;
    private AppConfig appConfig;
    ArrayList<Definition> mDefs = new ArrayList<Definition>();
    private List mXsds = new ArrayList();
    private Map mEncoderMap = new HashMap();
    private static final Logger mLogger = Logger.getLogger(EmailWSDLConfigurations.class.getName());

    public EmailWSDLConfigurations(ManagerContext ctx, EndpointInfo info, String suRootPath) throws WSDLException, DeploymentException {
        this(suRootPath, ctx);
        this.mEptInfo = info;

    }

    public EmailWSDLConfigurations(String svcUnitRootPath, ManagerContext ctx) throws WSDLException, DeploymentException {
        mMgrCtx = ctx;
        appConfig = getAppConfig(svcUnitRootPath);
        File dir = new File(svcUnitRootPath);
        this.parse(dir);
    }

    private AppConfig getAppConfig(String svcUnitRootPath) throws DeploymentException {
        QosServices qosSvcs = QosServicesDescriptor.parse(svcUnitRootPath);
        if (qosSvcs != null) {
            String appConfigName = qosSvcs.getApplicationConfiguration(mEptInfo);
            if (null != appConfigName) {
                return mMgrCtx.getConfiguration().getAppConfig(appConfigName);
            }
        }
        return null;
    }

    private void parse(File dir) throws WSDLException {
        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() + File.separator + "meta-inf" + File.separator + "catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);

        mXsds = listResourceFiles(dir, ".xsd");

        Iterator<File> wsdls = listResourceFiles(dir, ".wsdl").iterator();
        while (wsdls.hasNext()) {
            Definition def = readWSDL(wsdls.next(), resolver);
            mDefs.add(def);
        }

    }

    /**
     * List all wsdl files in the currentDir and below
     */
    private List<File> listResourceFiles(File currentDir,
            String extension) {
        List<File> cumulativeResults = new ArrayList<File>();
        File[] filesInCurrentDir = currentDir.listFiles();
        for (File element : filesInCurrentDir) {

            if (element.isFile()) {
                if (element.getName().toLowerCase().endsWith(extension)) {
                    cumulativeResults.add(element);
                }
            } else if (element.isDirectory()) {
                List<File> wsdlsInSubDirectories = listResourceFiles(element,
                        extension);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }

    private Definition readWSDL(File f, EntityResolver resolver) throws WSDLException {
        EmailBCExtSerializer serializer = new EmailBCExtSerializer(mMgrCtx.getConfiguration());
        /*
        // work with maven open-jbi-components/wsdl4j (not maven wsdl4j/wsdl4j)
        WSDLReaderImpl reader = (WSDLReaderImpl) WSDLFactory.newInstance().newWSDLReader();
        reader.setEntityResolver(resolver);
         */
        // work with maven open-jbi-components/wsdl4jext
        WSDLReader reader = WSDL4JExt.newWSDLReader(resolver);

        reader.setExtensionRegistry(new EmailBCExtensionRegistry(serializer));
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

    public EmailBCEndpoint createEmailEndpoint() throws EmailBindingComponentConfigurationException {

        EmailBCEndpoint endpoint = null;

        QName serviceName = mEptInfo.getServiceName();
        String endpointName = mEptInfo.getEndpointName();

        Iterator<Definition> it = mDefs.iterator();

        while (it.hasNext()) {
            Definition def = it.next();

            Service service = def.getService(serviceName);
            if (service == null) {
                continue;
            }

            Port port = service.getPort(endpointName);
            if (port == null) {
                continue;
            }

            IMAPBinding imapBinding = getIMAPBinding(def, serviceName,
                    endpointName);
            POP3Binding pop3Binding = getPOP3Binding(def, serviceName, endpointName);

            SMTPBinding smtpBinding = getSMTPBinding(def, serviceName, endpointName);

            if (imapBinding == null && pop3Binding == null && smtpBinding == null) {
                continue;
            }

            if (imapBinding != null) {
                IMAPEndpoint imapEndpoint = new IMAPEndpoint(this.mMgrCtx, this.mEptInfo, appConfig);
                imapEndpoint.setIMAPBinding(imapBinding);

                constructIMAPEndpoint(serviceName, endpointName, def, imapEndpoint);
                endpoint = imapEndpoint;
            } else if (pop3Binding != null) {
                POP3Endpoint pop3Endpoint = new POP3Endpoint(this.mMgrCtx, this.mEptInfo, appConfig);
                pop3Endpoint.setPOP3Binding(pop3Binding);

                constructPOP3Endpoint(serviceName, endpointName, def, pop3Endpoint);
                endpoint = pop3Endpoint;
            } else if (smtpBinding != null) {
                SMTPEndpoint smtpEndpoint = new SMTPEndpoint(this.mMgrCtx, this.mEptInfo, appConfig);
                smtpEndpoint.setSMTPBinding(smtpBinding);

                constructSMTPEndpoint(serviceName, endpointName, def, smtpEndpoint);
                endpoint = smtpEndpoint;
            }
            break;
        }

        return endpoint;

    }

    private void constructIMAPEndpoint(QName serviceName, String endpointName, Definition def, IMAPEndpoint imapEndpoint) throws EmailBindingComponentConfigurationException {
        // If we have an IMAPBinding, we must have an IMAPAddress.
        IMAPAddress address = getIMAPAddress(def, serviceName,
                endpointName);
        if (address == null) {
            String msg = I18n.loc("EMAILBC-7022: IMAP address is missing, please specify the address in the wsdl. Service name: '{0}', Endpoint name : '{1}'", serviceName, endpointName);
            throw new EmailBindingComponentConfigurationException(msg);
        }

        // If we have an IMAPBinding, we must have operations
        Map<QName, ExtensibilityElement> imapOperations = getIMAPOperations(def, serviceName,
                endpointName);

        if ((imapOperations == null) || (imapOperations.size() == 0)) {
            String msg = I18n.loc("EMAILBC-7023: IMAP Operation extensibility element is missing, please specify the imapoperation in the wsdl. Service name: '{0}', Endpoint name : '{1}'", serviceName, endpointName);
            throw new EmailBindingComponentConfigurationException(msg);
        }

        // Store the Definition
        imapEndpoint.setDefinition(def);

        // Store our extensibility elements
        imapEndpoint.setIMAPAddress(address);
        imapEndpoint.setEmailOperations(imapOperations);
        setIMAPInputsOutputs(def, serviceName, endpointName, imapOperations.values());
    }

    private void constructPOP3Endpoint(QName serviceName, String endpointName, Definition def, POP3Endpoint pop3Endpoint) throws EmailBindingComponentConfigurationException {
        // If we have an POP3Binding, we must have an POP3Address.
        POP3Address address = getPOP3Address(def, serviceName,
                endpointName);
        if (address == null) {
            String msg = I18n.loc("EMAILBC-7024: POP3 address is missing, please specify the address in the wsdl. Service name: '{0}', Endpoint name : '{1}'", serviceName, endpointName);
            throw new EmailBindingComponentConfigurationException(msg);
        }

        // If we have an POP3Binding, we must have operations
        Map<QName, ExtensibilityElement> pop3Operations = getPOP3Operations(def, serviceName,
                endpointName);

        if ((pop3Operations == null) || (pop3Operations.size() == 0)) {
            String msg = I18n.loc("EMAILBC-7025: POP3 Operation extensibility element is missing, please specify the pop3operation in the wsdl. Service name: '{0}', Endpoint name : '{1}'", serviceName, endpointName);
            throw new EmailBindingComponentConfigurationException(msg);
        }

        // Store the Definition
        pop3Endpoint.setDefinition(def);

        // Store our extensibility elements
        pop3Endpoint.setPOP3Address(address);
        pop3Endpoint.setEmailOperations(pop3Operations);
        setPOP3InputsOutputs(def, serviceName, endpointName, pop3Operations.values());
    }

    private void constructSMTPEndpoint(QName serviceName, String endpointName, Definition def, SMTPEndpoint smtpEndpoint) throws EmailBindingComponentConfigurationException {
        // If we have an SMTPBinding, we must have an SMTPAddress.
        SMTPAddress address = getSMTPAddress(def, serviceName,
                endpointName);
        if (address == null) {
            String msg = I18n.loc("EMAILBC-7026: SMTP address is missing, please specify the address in the wsdl. Service name: '{0}', Endpoint name : '{1}'", serviceName, endpointName);
            throw new EmailBindingComponentConfigurationException(msg);
        }

        // If we have an SMTPBinding, we must have operations
        Map<QName, ExtensibilityElement> smtpOperations = getSMTPOperations(def, serviceName,
                endpointName);

        if ((smtpOperations == null) || (smtpOperations.size() == 0)) {
            String msg = I18n.loc("EMAILBC-7027: SMTP Operation extensibility element is missing, please specify the smtpoperation in the wsdl. Service name: '{0}', Endpoint name : '{1}'", serviceName, endpointName);
            throw new EmailBindingComponentConfigurationException(msg);
        }

        // Store the Definition
        smtpEndpoint.setDefinition(def);

        // Store our extensibility elements
        smtpEndpoint.setSMTPAddress(address);
        smtpEndpoint.setEmailOperations(smtpOperations);
        setSMTPInputsOutputs(def, serviceName, endpointName, smtpOperations.values());
    }

    private IMAPBinding getIMAPBinding(Definition def,
            QName serviceName, String endpointName) {
        IMAPBinding imapBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            // Look for imap:binding

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while ((extIter != null) && extIter.hasNext() && (imapBinding == null)) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (IMAPBinding.class.isInstance(ee)) {
                    imapBinding = (IMAPBinding) ee;
                }
            }
        }
        return imapBinding;
    }

    private POP3Binding getPOP3Binding(Definition def,
            QName serviceName, String endpointName) {
        POP3Binding pop3Binding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            // Look for pop3:binding

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while ((extIter != null) && extIter.hasNext() && (pop3Binding == null)) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (POP3Binding.class.isInstance(ee)) {
                    pop3Binding = (POP3Binding) ee;
                }
            }
        }
        return pop3Binding;
    }

    private SMTPBinding getSMTPBinding(Definition def,
            QName serviceName, String endpointName) {
        SMTPBinding SMTPBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            // Look for smtp:binding

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while ((extIter != null) && extIter.hasNext() && (SMTPBinding == null)) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SMTPBinding.class.isInstance(ee)) {
                    SMTPBinding = (SMTPBinding) ee;
                }
            }
        }
        return SMTPBinding;
    }

    private Binding getBinding(Definition def, QName serviceName,
            String endpointName) {
        Service svc = def.getService(serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(endpointName);
        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }

    private IMAPAddress getIMAPAddress(Definition def,
            QName serviceName, String endpointName) {
        IMAPAddress address = null;
        Service svc = def.getService(serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(endpointName);
        if (port != null) {
            List extElems = port.getExtensibilityElements();

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while ((extIter != null) && extIter.hasNext() && (address == null)) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (IMAPAddress.class.isInstance(ee)) {
                    address = (IMAPAddress) ee;
                }
            }
        }
        return address;
    }

    private POP3Address getPOP3Address(Definition def,
            QName serviceName, String endpointName) {
        POP3Address address = null;
        Service svc = def.getService(serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(endpointName);
        if (port != null) {
            List extElems = port.getExtensibilityElements();

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while ((extIter != null) && extIter.hasNext() && (address == null)) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (POP3Address.class.isInstance(ee)) {
                    address = (POP3Address) ee;
                }
            }
        }
        return address;
    }

    private SMTPAddress getSMTPAddress(Definition def,
            QName serviceName, String endpointName) {
        SMTPAddress address = null;
        Service svc = def.getService(serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(endpointName);
        if (port != null) {
            List extElems = port.getExtensibilityElements();

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while ((extIter != null) && extIter.hasNext() && (address == null)) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SMTPAddress.class.isInstance(ee)) {
                    address = (SMTPAddress) ee;
                }
            }
        }
        return address;
    }

    private Map<QName, ExtensibilityElement> getIMAPOperations(Definition def,
            QName serviceName, String endpointName) {
        Map<QName, ExtensibilityElement> imapOperations = new HashMap<QName, ExtensibilityElement>();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null
                    : bindingOperations.iterator();
            while ((operIter != null) && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for imap:operation entries

                Iterator extIter = extElems == null ? null : extElems.iterator();
                while ((extIter != null) && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (IMAPOperation.class.isInstance(ee)) {
                        IMAPOperation imapOperation = (IMAPOperation) ee;
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement) inputIter.next();
                                if (inputExt instanceof IMAPOperationInput) {
                                    IMAPOperationInput imapOperationInput = (IMAPOperationInput) inputExt;
                                    imapOperation.setInput(imapOperationInput);
                                }
                            }
                        }
                        imapOperations.put(QName.valueOf(oper.getName()), ee);
                    }
                }
            }
        }
        return imapOperations;
    }

    private Map<QName, ExtensibilityElement> getPOP3Operations(Definition def,
            QName serviceName, String endpointName) {
        Map<QName, ExtensibilityElement> pop3Operations = new HashMap<QName, ExtensibilityElement>();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null
                    : bindingOperations.iterator();
            while ((operIter != null) && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for imap:operation entries

                Iterator extIter = extElems == null ? null : extElems.iterator();
                while ((extIter != null) && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (POP3Operation.class.isInstance(ee)) {
                        POP3Operation pop3Operation = (POP3Operation) ee;
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement) inputIter.next();
                                if (inputExt instanceof POP3OperationInput) {
                                    POP3OperationInput pop3OperationInput = (POP3OperationInput) inputExt;
                                    pop3Operation.setInput(pop3OperationInput);
                                }
                            }
                        }
                        pop3Operations.put(QName.valueOf(oper.getName()), ee);
                    }
                }
            }
        }
        return pop3Operations;
    }

    private Map<QName, ExtensibilityElement> getSMTPOperations(Definition def,
            QName serviceName, String endpointName) {
        Map<QName, ExtensibilityElement> smtpOperations = new HashMap<QName, ExtensibilityElement>();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null
                    : bindingOperations.iterator();
            while ((operIter != null) && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for imap:operation entries

                Iterator extIter = extElems == null ? null : extElems.iterator();
                while ((extIter != null) && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (SMTPOperation.class.isInstance(ee)) {
                        SMTPOperation smtpOperation = (SMTPOperation) ee;
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement) inputIter.next();
                                if (inputExt instanceof SMTPOperationInput) {
                                    SMTPOperationInput smtpOperationInput = (SMTPOperationInput) inputExt;
                                    smtpOperation.setInput(smtpOperationInput);
                                }
                            }
                        }
                        smtpOperations.put(QName.valueOf(oper.getName()), ee);
                    }
                }
            }
        }
        return smtpOperations;
    }

    private void setIMAPInputsOutputs(Definition def, QName serviceName, String endpointName,
            Collection<ExtensibilityElement> operations) {

        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while ((operIter != null) && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();

                IMAPOperation imapOperation = null;
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while ((extIter != null) && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (operations.contains(ee)) {
                        imapOperation = (IMAPOperation) ee;
                    }
                }

                if (imapOperation != null) {
                    BindingInput bindingInput = oper.getBindingInput();
                    if (bindingInput != null) {
                        Iterator it = bindingInput.getExtensibilityElements().iterator();
                        while (it.hasNext()) {
                            ExtensibilityElement ee = (ExtensibilityElement) it.next();
                            if (ee instanceof IMAPOperationInput) {
                                imapOperation.setInput((IMAPOperationInput) ee);
                            }
                        }
                    }
                }
            }
        }
    }

    private void setSMTPInputsOutputs(Definition def, QName serviceName, String endpointName,
            Collection<ExtensibilityElement> operations) {

        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while ((operIter != null) && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();

                SMTPOperation smtpOperation = null;
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while ((extIter != null) && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (operations.contains(ee)) {
                        smtpOperation = (SMTPOperation) ee;
                    }
                }

                if (smtpOperation != null) {
                    BindingInput bindingInput = oper.getBindingInput();
                    if (bindingInput != null) {
                        Iterator it = bindingInput.getExtensibilityElements().iterator();
                        while (it.hasNext()) {
                            ExtensibilityElement ee = (ExtensibilityElement) it.next();
                            if (ee instanceof SMTPOperationInput) {
                                smtpOperation.setInput((SMTPOperationInput) ee);
                            }
                        }
                    }
                }
            }
        }
    }

    private void setPOP3InputsOutputs(Definition def, QName serviceName, String endpointName,
            Collection<ExtensibilityElement> operations) {

        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while ((operIter != null) && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();

                POP3Operation pop3Operation = null;
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while ((extIter != null) && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (operations.contains(ee)) {
                        pop3Operation = (POP3Operation) ee;
                    }
                }

                if (pop3Operation != null) {
                    BindingInput bindingInput = oper.getBindingInput();
                    if (bindingInput != null) {
                        Iterator it = bindingInput.getExtensibilityElements().iterator();
                        while (it.hasNext()) {
                            ExtensibilityElement ee = (ExtensibilityElement) it.next();
                            if (ee instanceof POP3OperationInput) {
                                pop3Operation.setInput((POP3OperationInput) ee);
                            }
                        }
                    }
                }
            }
        }
    }
    public Map getPartEncoderMapping(String serviceName, String endpointName,
             Map emailOperations) throws Exception {
        Map partMapping = new HashMap();
        
        // Don't support in-line encoder schemas
        if (mXsds.size() <= 0) {
            return partMapping;
        }
        Port port = null;
        Service service = null;
        Iterator<Definition> it = mDefs.iterator();
        while (it.hasNext()) {
            Definition def = it.next();

            service = def.getService(QName.valueOf(serviceName));
            if (service == null) {
                continue;
            }
            port = service.getPort(endpointName);
            if (port == null) {
                continue;
            } else {
            	break;
            }
        }
        
        PortType portType = port.getBinding().getPortType();
        /**
         * Locate the operation we are interested in.
         * There may be multiple operations by the same name (operation overloading)
         * and the WSDL spec does allow it. The uniqueness
         * should be guaranteed by the examination of input and/or output names.
         * For the time being, we will assume that we don't have operation overloading.
         */
        javax.wsdl.Message wsdlMessage = null;
        Map parts = new HashMap();
        for (Iterator opnameIter = emailOperations.keySet().iterator(); opnameIter.hasNext(); ) {
            String encodingStyle = null;
            
            QName operationName = (QName)opnameIter.next();
            SMTPOperation smtpOperation = (SMTPOperation)emailOperations.get(operationName);
            
            for (Iterator operIter = portType.getOperations().iterator(); operIter.hasNext();) {
                Operation op = (Operation)operIter.next();
                if (op.getName().equals(operationName.toString()) ||
                        op.getName().equals(operationName.getLocalPart())) {
                    /**
                     * There is nothing in the WSDL specification that says
                     * that the part name has to be unique within the
                     * WSDL document, so we need to prefix the part name
                     * with the message name.
                     */
                    
                    Input input = op.getInput();
                    if (input != null) {
                    	SMTPOperationInput smtpInput = (SMTPOperationInput)smtpOperation.getInput();
                        wsdlMessage = input.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName elem = (aPart.getElementName() != null) ? aPart.getElementName() : null;
                            
                            // locate the XSD file based on the part type namespace
                            String xsdFileLoc = getXsdFileLocation(elem);
                            if (xsdFileLoc != null) {
                            	   // Provider
                                    if (smtpInput == null) {
                                    	throw new Exception(I18n.loc("EMAILBC-7033: Missing required SMTP Input attributes for SMTP binding operation", operationName.toString()));
                                    }
                                    if (smtpInput.getSmtpUseType().equals(EmailBCConstants.SMTP_USE_TYPE_ENCODED)) {
                                        encodingStyle = smtpInput.getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                    	throw new Exception(I18n.loc("EMAILBC-7034: Missing required SMTP Input 'encodingStyle' attribute(value) for SMTP binding operation ", operationName.toString()));
                                    }
                                
                                Encoder encoder = null;
                                MetaRef metaRef = new MyMetaRef(xsdFileLoc, elem);
                                if (mEncoderMap.get(metaRef) != null) {
                                    encoder = (Encoder)mEncoderMap.get(metaRef);
                                } else {
                                    EncoderFactory encoderFactory = EncoderFactory.newInstance();
                                    encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle),
                                            metaRef);
                                    mEncoderMap.put(metaRef, encoder);
                                }
                                partMapping.put(wsdlMessage.getQName() + partName, encoder);
                            }
                        }
                    }
                }
            }
        }
        return partMapping;
        
    }
    protected String getXsdFileLocation(QName elemName) {
        if (elemName == null) {
            return null;
        }
        
        File aXsdFile = null;
        try {
            aXsdFile = XsdLocator.findXsdByElement(mXsds, elemName);
        } catch (Exception e) {
            mLogger.log(Level.SEVERE,
            			I18n.loc("EMAILBC-7035: Error getting XSD file {0} , exception {1}", aXsdFile.getName(), e.getLocalizedMessage()));                    
        }
        return aXsdFile != null ? aXsdFile.getAbsolutePath() : null;
    }    
    
    /**
     * An implementation of the MetaRef interface
     */
    private class MyMetaRef implements MetaRef {
        private final String mXsdPath;
        private final QName mRootElemName;
        private final String mToString;
        
        /**
         * Constructor
         */
        protected MyMetaRef(String xsdLoc) {
            this(xsdLoc, null);
        }
        
        /**
         * Alternative constructor that constructs a MetaRef object
         * with the file path location of the main XSD and
         * qualified name of the root element
         */
        protected MyMetaRef(String xsdLoc, QName rootElemName) {
            mXsdPath = xsdLoc;
            mRootElemName = rootElemName;
            mToString = toString();
        }
        
        /**
         * Return the file path location of the main XSD
         * @return    the path of the main meta file
         */
        public String getPath() {
            return mXsdPath;
        }
        
        /**
         * Return the QName of the root element.
         * @return the QName of the root element
         */
        public QName getRootElemName() {
            return mRootElemName;
        }
        
        /**
         * Gets the URL of the main metadata file.  This URL should point to an
         * XSD file somewhere.  If this method returns a value other than
         * <code>null</code>, the return value of <code>getPath()</code> will
         * be ignored.  To load encoder metadata from a jar file, a URL in form
         * "jar:&lt;url&gt;!/{entry}" can be used.
         *
         * @return the URL of the main meta file
         */
        public URL getURL() {
            return null;
        }
        
        public String toString() {
            return mXsdPath + mRootElemName.toString();
        }
        
        public boolean equals(Object obj) {
            if (!(obj instanceof MyMetaRef)) {
                return false;
            }
            return mToString.equals(((MyMetaRef) obj).mToString);
        }
        
        public int hashCode() {
            return mToString.hashCode();
        }
    }
}
