/*
 * @(#)RuleExecutionTask.java        $Revision: 1.9 $ $Date: 2009/01/27 21:42:45 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.component;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Callable;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.rules.RuleRuntime;
import javax.rules.RuleServiceProvider;
import javax.rules.RuleServiceProviderManager;
import javax.rules.StatelessRuleSession;
import javax.rules.admin.LocalRuleExecutionSetProvider;
import javax.rules.admin.RuleAdministrator;
import javax.rules.admin.RuleExecutionSet;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import nu.xom.Document;
import nu.xom.Element;

import com.google.inject.Inject;

import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import org.openesb.components.rules4jbi.shared.wsdl.WSDLConstants;

import org.openesb.components.rules4jbi.engine.guice.annotations.ExecutionTask;
import org.openesb.components.rules4jbi.engine.wsdl.JBIWrapper;

/**
 * A task that processes a single message exchange.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.9 $ $Date: 2009/01/27 21:42:45 $
 * 
 * @since 0.1
 */
public final class RuleExecutionTask implements Callable<InOut> {

    @Inject @ExecutionTask
    private Logger logger = null;
    
    private String ruleServiceProvider = null;
    
    private String ruleServiceProviderClass = null;
    
    private File rulesetFile = null;
    
    private Class<?>[] classes = null;

    private ClassLoader classLoader = null;

    private String targetNamespace = null;
    
    private List<QName> outputElements = null;
    
    private InOut messageExchange = null;
    
    /* Instances of this class should be created only by Guice */
    private RuleExecutionTask() {}

    void setRuleServiceProvider(String ruleServiceProvider) {
        this.ruleServiceProvider = ruleServiceProvider;
    }

    void setRuleServiceProviderClass(String ruleServiceProviderClass) {
        this.ruleServiceProviderClass = ruleServiceProviderClass;
    }

    void setRulesetFile(File rulesetFile) {
        this.rulesetFile = rulesetFile;
    }

    void setClasses(Class<?>[] classes) {
        this.classes = classes;
    }

    void setClassLoader(ClassLoader classLoader) {
        this.classLoader = classLoader;
    }

    void setTargetNamespace(String targetNamespace) {
        this.targetNamespace = targetNamespace;
    }

    void setOutputElements(List<QName> outputElements) {
        this.outputElements = outputElements;
    }
    
    void setMessageExchange(InOut messageExchange) {
        this.messageExchange = messageExchange;
    }
    
    public InOut call() throws MessagingException {
        if (logger == null || ruleServiceProvider == null || ruleServiceProviderClass == null
                || rulesetFile == null || classes == null || classLoader == null
                || targetNamespace == null || outputElements == null || messageExchange == null)
        {
            throw new IllegalStateException("This rule execution task was not properly initialized");
        }
        
        NormalizedMessage inMessage = messageExchange.getInMessage();
        Source inputSource = inMessage.getContent();
        
        logger.fine("Converting the Source to nu.xom.Document");
        Document document = XOMUtils.toDocument(inputSource);
        logger.fine("Document element: %s", document.toXML());

        Element message = document.getRootElement();
        logger.fine("Message element: %s", message.toXML());

        Element part = message.getFirstChildElement("part", "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper");
        logger.fine("Part element: %s", part.toXML());

        Element data = part.getFirstChildElement(WSDLConstants.INPUT_ELEMENT_NAME, WSDLConstants.TYPES_NAMESPACE_URI);
        logger.fine("Data element: %s", data.toXML());

        List<nu.xom.Element> businessObjectElements = XOMUtils.asList(data.getChildElements());
        List<Object> businessObjects = new ArrayList<Object>();
        
        Serializer serializer = new Serializer(classes);

        for (Element element : businessObjectElements) {
            logger.fine("Business object xml: %s", element.toXML());
            
            Object obj = serializer.deserialize(element);
            logger.fine("Business object: %s", obj);
            
            businessObjects.add(obj);
        }
        
        Thread.currentThread().setContextClassLoader(classLoader);
        
        List<?> result = null;
        try {
             result = executeRules(businessObjects);
            
        } catch (RuleExecutionFailedException e) {
            
            messageExchange.setError(e.getCause());
            
            return messageExchange;
        }
        
        Collections.sort(result, new QNameComparator(outputElements));
        
        JBIWrapper outputDataWrapper = new JBIWrapper(JBIWrapper.Type.OUTPUT, targetNamespace);
        for (Object obj : result) {
            logger.fine("Resulting business object: %s", obj);
            outputDataWrapper.addBusinessObject(serializer.serialize(obj));
        }

        logger.fine("Sending result %s", outputDataWrapper.getWrapper().toXML());

        Source outputSource = outputDataWrapper.toDOMSource();
        
        NormalizedMessage outMessage = messageExchange.createMessage();
        outMessage.setContent(outputSource);
        messageExchange.setOutMessage(outMessage);
        
        return messageExchange;
    }
    
    public List<?> executeRules(List<?> businessObjects) throws RuleExecutionFailedException {
        logger.fine("Executing rules with provider: '%s', '%s'", ruleServiceProvider, ruleServiceProviderClass);
        
        InputStream inputStream = null;
        
        StatelessRuleSession session = null;
        
        try {
            Class<?> providerClass = Class.forName(ruleServiceProviderClass , true, classLoader);
            
//            RuleServiceProviderManager.registerRuleServiceProvider(ruleServiceProvider, providerClass, classLoader);

            RuleServiceProvider provider =
                    RuleServiceProviderManager.getRuleServiceProvider(ruleServiceProvider);

            RuleAdministrator administrator = provider.getRuleAdministrator();

            LocalRuleExecutionSetProvider ruleExecutionSetProvider =
                    administrator.getLocalRuleExecutionSetProvider(null);

            logger.fine("Loading rules from file '%s'", rulesetFile.getAbsolutePath());
            inputStream = new FileInputStream(rulesetFile);
            
            RuleExecutionSet ruleExecutionSet =
                    ruleExecutionSetProvider.createRuleExecutionSet(inputStream, null);

            inputStream.close();
            inputStream = null;

            logger.fine("Loaded rule execution set: '%s'", ruleExecutionSet);

            String uri = ruleExecutionSet.getName();

            administrator.registerRuleExecutionSet(uri, ruleExecutionSet, null);

            logger.fine("Bound RuleExecutionSet to URI: %s", uri);

            RuleRuntime runtime = provider.getRuleRuntime();

            session = (StatelessRuleSession) runtime.createRuleSession(uri, new HashMap(),
                    RuleRuntime.STATELESS_SESSION_TYPE);

            logger.fine("Stateless rule session opened: " + session);
            
            logger.fine("Executing rules with input: %s", businessObjects);

            List<?> result = session.executeRules(businessObjects, new QNameObjectFilter(outputElements));

            logger.fine("Rule execution returned: %s", result);

            session.release();
            session = null;
            
            return result;
            
        } catch (Exception e) {
            logger.severe("Failed to execute the business rules", e);
            
            throw new RuleExecutionFailedException(e);

        } finally {
            if (session != null) {
                try {
                    session.release();

                    logger.fine("Session released successfully");

                } catch (Exception e) {
                    logger.fine("Failed to release the session", e);
                }
            }

            if (inputStream != null) {
                try {
                    inputStream.close();
                    
                } catch (IOException e) {
                    logger.fine("Failed to properly close the inputstream", e);
                }
            }
        }
    }
}
