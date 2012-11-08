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
 * @(#)JMSExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.Serializable;
import java.io.PrintWriter;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;

import org.w3c.dom.Attr;
import org.w3c.dom.CDATASection;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

// for stand alone test
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;
import org.xml.sax.EntityResolver;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfigurationMBean;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.org.apache.xml.internal.serializer.ElemDesc;

/**
 * JMSExtSerializer
 */
public class JMSExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {

    private static final URL[] EMPTY_ARRAY = new URL[0];
	private static final Messages mMessages =
        Messages.getMessages(JMSExtSerializer.class);
    private static final Logger mLogger =
        Messages.getLogger(JMSExtSerializer.class);
    private static final ClassLoader GLOBALCLASSLOADER;
    
    private static final long serialVersionUID = 1L;
    private Map mEnvVariableMap;
    private boolean mResolveTokens = false;
    private JMSBCRuntimeConfigurationMBean mRuntimeConfig;
        
    static{
    	URL[] urls = parseClasspath(System.getProperty("com.sun.jbi.jmsbc.classpath"));
        if(urls != null && urls.length > 0){
        	GLOBALCLASSLOADER = new URLClassLoader(urls, JMSExtSerializer.class.getClassLoader());
        }else{
        	GLOBALCLASSLOADER = null;
        }
    	
    }

    /** 
     * JMSExtSerializer default constructor 
     * private for internal test purposes only
     */
    private JMSExtSerializer() {
    }    

    /** Creates a new instance of JMSExtSerializer 
     *
     *  @param envVariableMap The runtime environment variable map for the bc 
     *  @param resolveTokens Determines whether tokens in wsdl should be resolved
     */
    public JMSExtSerializer(Map envVariableMap, boolean resolveTokens, JMSBCRuntimeConfigurationMBean runtimeConfig) {
        mEnvVariableMap = envVariableMap;
        mResolveTokens = resolveTokens;
        mRuntimeConfig = runtimeConfig;
    }
    
    /**
     * Registers the serializers / deserializers
     */
    public void registerSerializer(ExtensionRegistry registry) {
        // Register and map JMS Binding
        registry.registerSerializer(Binding.class, JMSConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, JMSConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, JMSConstants.QNAME_BINDING, JMSBinding.class);

        // Register and map JMS Operation
        registry.registerSerializer(BindingOperation.class, JMSConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, JMSConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, JMSConstants.QNAME_OPERATION, JMSOperation.class);

        // Register and map JMS Input
        registry.registerSerializer(BindingInput.class, JMSConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingInput.class, JMSConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, JMSConstants.QNAME_MESSAGE, JMSMessage.class);

        // Register and map JMS Output
        registry.registerSerializer(BindingOutput.class, JMSConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, JMSConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, JMSConstants.QNAME_MESSAGE, JMSMessage.class);

        // Register and map JMS Address
        registry.registerSerializer(Port.class, JMSConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, JMSConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, JMSConstants.QNAME_ADDRESS, JMSAddress.class);
    }


    public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
            PrintWriter pw, javax.wsdl.Definition def, ExtensionRegistry extReg) throws WSDLException {
        if (extension == null) {
            return;
        }

        if (extension instanceof JMSBinding) {
            JMSBinding jmsBinding = (JMSBinding) extension;
            pw.print("      <jms:binding");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof JMSOperation) {
            JMSOperation jmsOperation = (JMSOperation) extension;
            pw.print("      <jms:operation>");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            // Todo: marshal operation
            pw.println("</jms:operation>");
            
        } else if (extension instanceof JMSAddress) {
            JMSAddress jmsAddress = (JMSAddress) extension;
            pw.print("      <jms:address");
            
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            // Todo: marsh address
            /*
            if (jmsAddress.getConnectionURL() != null) {
                DOMUtils.printAttribute(JMSAddress.ATTR_CONNECTION_URL, jmsAddress.getConnectionURL(), pw);
            }

            if (jmsAddress.getUsername() != null) {
                DOMUtils.printAttribute(JMSAddress.ATTR_USERNAME, jmsAddress.getUsername(), pw);
            }
            
            if (jmsAddress.getPassword() != null) {
                DOMUtils.printAttribute(JMSAddress.ATTR_PASSWORD, jmsAddress.getPassword(), pw);
            }
            */
            pw.println("/>");
        } else if (extension instanceof JMSInput) {
            JMSInput jmsInput = (JMSInput) extension;
            pw.print("      <jms:input>");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            // Todo: marshal message
            /*
            if (jmsInput.getJMSMessage() != null) {
            }            
             */
            pw.println("</jms:input>");
        } else if (extension instanceof JMSOutput) {
            JMSOutput jmsOutput = (JMSOutput) extension;
            pw.print("      <jms:output>");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            // Todo: marshal message
            /*
            if (jmsOutput.getJMSMessage() != null) {
                // Todo: marshal message element
            }            
             */
            pw.println("</jms:output>");
        }        
    }


    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg) throws javax.wsdl.WSDLException {

        ExtensibilityElement returnValue = null;

        if (JMSConstants.QNAME_BINDING.equals(elementType)) {
            JMSBinding jmsBinding = new JMSBinding();
            returnValue = jmsBinding;
        } else if (JMSConstants.QNAME_OPERATION.equals(elementType)) {
            JMSOperation jmsOperation = new JMSOperation();
            
            //
            // unmarshal element(s)
            //
            JMSOptions options = new JMSOptions();
            unmarshalOptions (options, el, def);
            jmsOperation.setOptions(options);
            
            //
            // unmarshal attributes
            //
            String destination = DOMUtils.getAttribute(el, JMSOperation.ATTR_DESTINATION);
            if (nonEmptyString(destination)) {
            	try {
                    if (isAToken(destination, def)) {
                    	String token = destination;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                throw new WSDLException("INVALID_WSDL", 
                                mMessages.getString("JMSBC-E0003.EnvironmentVariableHasNoValue", new Object[] {token, JMSOperation.ATTR_DESTINATION, def.getQName()}));
                            }
                            jmsOperation.setDestination(metadata[0]);
                        }
                    } else {
                        jmsOperation.setDestination(destination);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                } 
            }
            
            String destinationType = DOMUtils.getAttribute(el, JMSOperation.ATTR_DESTINATION_TYPE);
            if (nonEmptyString(destinationType)) {
                jmsOperation.setDestinationType(destinationType);
            }
            
            String transaction = DOMUtils.getAttribute(el, JMSOperation.ATTR_TRANSACTION);
            if (nonEmptyString(transaction)) {
                jmsOperation.setTransaction(transaction);
            }
                        
            String deliveryMode = DOMUtils.getAttribute(el, JMSOperation.ATTR_DELIVERY_MODE);
            if (nonEmptyString(deliveryMode)) {
                jmsOperation.setDeliveryMode(deliveryMode);
            }
            
            String timeToLive = DOMUtils.getAttribute(el, JMSOperation.ATTR_TIME_TO_LIVE);
            if (nonEmptyString(timeToLive)) {
                jmsOperation.setTimeToLive(Long.valueOf(timeToLive));
            }

            String priority = DOMUtils.getAttribute(el, JMSOperation.ATTR_PRIORITY);
            if (nonEmptyString(priority)) {
                jmsOperation.setPriority(Integer.valueOf(priority));
            }

            String disableMessageID = DOMUtils.getAttribute(el, JMSOperation.ATTR_DISABLE_MESSAGE_ID);
            if (nonEmptyString(disableMessageID)) {
                jmsOperation.setDisableMessageID(Boolean.valueOf(disableMessageID));
            }

            String disableMessageTimeStamp = DOMUtils.getAttribute(el, JMSOperation.ATTR_DISABLE_MESSAGE_TIMESTAMP);
            if (nonEmptyString(disableMessageTimeStamp)) {
                jmsOperation.setDisableMessageTimeStamp(Boolean.valueOf(disableMessageTimeStamp));
            }
            
            String timeout = DOMUtils.getAttribute(el, JMSOperation.ATTR_TIMEOUT);
            if (nonEmptyString(timeout)) {
                jmsOperation.setTimeout(Long.valueOf(timeout));
            }
            
            //Get verb
            String verb = DOMUtils.getAttribute(el, JMSOperation.ATTR_VERB);
            if(nonEmptyString(verb)){
            	jmsOperation.setVerb(verb);
            }
            
            String clientID = DOMUtils.getAttribute(el, JMSOperation.ATTR_CLIENT_ID);
            if (nonEmptyString(clientID)) {
            	try {
                    if (isAToken(clientID, def)) {
                    	String token = clientID;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                mLogger.log(Level.WARNING, "JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSOperation.ATTR_CLIENT_ID, def.getQName()});
                                AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSOperation.ATTR_CLIENT_ID, def.getQName()}), 
                                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                    null, 
                                    AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                    NotificationEvent.EVENT_TYPE_ALERT,
                                    "JMSBC-W0001");    
                            } else {
                                jmsOperation.setClientID(metadata[0]);
                            }
                        }
                    } else {
                        jmsOperation.setClientID(clientID);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                } 
            }

            String messageSelector = DOMUtils.getAttribute(el, JMSOperation.ATTR_MESSAGE_SELECTOR);
            if (nonEmptyString(messageSelector)) {
            	try {
                    if (isAToken(messageSelector, def)) {
                    	String token = messageSelector;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                mLogger.log(Level.WARNING, "JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSOperation.ATTR_MESSAGE_SELECTOR, def.getQName()});
                                AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSOperation.ATTR_MESSAGE_SELECTOR, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0001");
                            } else {
                                jmsOperation.setMessageSelector(metadata[0]);
                            }
                        }
                    } else {
                        jmsOperation.setMessageSelector(messageSelector);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }

            String validateMessageSelector = DOMUtils.getAttribute(el, JMSOperation.ATTR_VALIDATE_MESSAGE_SELECTOR);
            if (nonEmptyString(validateMessageSelector)) {
                jmsOperation.setValidateMessageSelector(Boolean.valueOf(validateMessageSelector));
            }
            
            String subscriptionDurability = DOMUtils.getAttribute(el, JMSOperation.ATTR_SUBSCRIPTION_DURABILITY);
            if (nonEmptyString(subscriptionDurability)) {
                jmsOperation.setSubscriptionDurability(subscriptionDurability);
            }
            
            String subscriptionName = DOMUtils.getAttribute(el, JMSOperation.ATTR_SUBSCRIPTION_NAME);
            if (nonEmptyString(subscriptionName)) {
            	try {
                    if (isAToken(subscriptionName, def)) {
                    	String token = subscriptionName;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                mLogger.log(Level.WARNING, "JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSOperation.ATTR_SUBSCRIPTION_NAME, def.getQName()});
                                AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSOperation.ATTR_SUBSCRIPTION_NAME, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0001");
                            } else {
                                jmsOperation.setSubscriptionName(metadata[0]);
                            }
                        }
                    } else {
                        jmsOperation.setSubscriptionName(subscriptionName);
                    }                    
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }
           
            String maxConcurrentConsumers = DOMUtils.getAttribute(el, JMSOperation.ATTR_MAX_CONCURRENT_CONSUMERS);
            if (mRuntimeConfig.getForceMaxConcurrentConsumers() > 0) {
                maxConcurrentConsumers = mRuntimeConfig.getForceMaxConcurrentConsumers().toString();
                
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine("Using forced value for max concurrent consumers of \"" + maxConcurrentConsumers + "\"");
                }
                
            }
            if (nonEmptyString(maxConcurrentConsumers)) {
            	try {
                if (isAToken(maxConcurrentConsumers, def)) {
                	String token = maxConcurrentConsumers;
                    String envVariableName = getEnvVariableName(token, def);
                    
                    if (!mResolveTokens) {
                        if (!mEnvVariableMap.containsKey(envVariableName)) {
                            String[] metadata = new String[] {null, "STRING"};
                            mEnvVariableMap.put(envVariableName, metadata);                                
                        }
                    } else {
                        String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                        if (metadata == null || metadata[0] == null) {
                            throw new WSDLException("INVALID_WSDL", 
                            mMessages.getString("JMSBC-E0003.EnvironmentVariableHasNoValue", new Object[] {token, JMSOperation.ATTR_DESTINATION, def.getQName()}));
                        }
                        jmsOperation.setMaxConcurrentConsumers(Integer.valueOf((metadata[0])));
                    }
                } else {
                	jmsOperation.setMaxConcurrentConsumers(Integer.valueOf(maxConcurrentConsumers));
                }
            	} catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }
            }            
            
            String concurrencyMode = DOMUtils.getAttribute(el, JMSOperation.ATTR_CONCURRENCY_MODE);
            if (nonEmptyString(mRuntimeConfig.getForceConcurrencyMode())) {
                concurrencyMode = mRuntimeConfig.getForceConcurrencyMode();
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine("Using forced concurrency mode of \"" + concurrencyMode + "\"");
                }
            }
            if (nonEmptyString(concurrencyMode)) {
                jmsOperation.setConcurrencyMode(concurrencyMode);
            } 
            
            String batchSize = DOMUtils.getAttribute(el, JMSOperation.ATTR_BATCH_SZIE);
            if (nonEmptyString(batchSize)) {
                jmsOperation.setBatchSize(Integer.valueOf(batchSize));
            }            

            String redeliveryHandling = DOMUtils.getAttribute(el, JMSOperation.ATTR_REDELIVERY_HANDLING);
            if (!nonEmptyString(redeliveryHandling)) {
                redeliveryHandling = mRuntimeConfig.getDefaultRedeliveryHandling();
                
                if (mLogger.isLoggable(Level.FINE) && nonEmptyString(redeliveryHandling)) {
                    mLogger.fine("Using default redelivery handling of \"" + redeliveryHandling + "\"");
                }
                
            }
            
            if (nonEmptyString(redeliveryHandling)) {
            	try {
                    if (isAToken(redeliveryHandling, def)) {
                    	String token = redeliveryHandling;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                mLogger.log(Level.WARNING, "JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSOperation.ATTR_REDELIVERY_HANDLING, def.getQName()});
                                AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSOperation.ATTR_REDELIVERY_HANDLING, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0001");
                            } else {
                                jmsOperation.setRedeliveryHandling(metadata[0]);
                            }
                        }
                    } else {
                        jmsOperation.setRedeliveryHandling(redeliveryHandling);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }
            
            returnValue = jmsOperation;
        } else if (JMSConstants.QNAME_ADDRESS.equals(elementType)) {
            JMSAddress jmsAddress = new JMSAddress();

            //
            // unmarshal element(s)
            //
            JMSJNDIEnv jndiEnv = new JMSJNDIEnv();
            JMSJCAOptions jmsjcaOptions = new JMSJCAOptions();
            unmarshalJNDIEnvAndJMSJCAOptions (jmsAddress, jndiEnv, jmsjcaOptions, el, def);
            jmsAddress.setJndiEnv(jndiEnv);
            jmsAddress.setJmsjcaOptions(jmsjcaOptions);

            //
            // unmarshal attribute(s)
            //
            String connectionURL = DOMUtils.getAttribute(el, JMSAddress.ATTR_CONNECTION_URL);
            if (nonEmptyString(connectionURL)) {
            	try {
                    if (isAToken(connectionURL, def)) {
                    	String token = connectionURL;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                throw new WSDLException("INVALID_WSDL", 
                                mMessages.getString("JMSBC-E0003.EnvironmentVariableHasNoValue", new Object[] {token, JMSAddress.ATTR_CONNECTION_URL, def.getQName()}));
                            }
                            jmsAddress.setConnectionURL(metadata[0]);
                        }
                    } else {
                        jmsAddress.setConnectionURL(connectionURL);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }

            String username = DOMUtils.getAttribute(el, JMSAddress.ATTR_USERNAME);
            if (nonEmptyString(username)) {
            	try {
                    if (isAToken(username, def)) {
                    	String token = username;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                mLogger.log(Level.WARNING, "JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSAddress.ATTR_USERNAME, def.getQName()});
                                AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSAddress.ATTR_USERNAME, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0001");
                            } else {
                                jmsAddress.setUsername(metadata[0]);
                            }
                        }
                    } else {
                        jmsAddress.setUsername(username);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }
            
            String password = DOMUtils.getAttribute(el, JMSAddress.ATTR_PASSWORD);
            if (nonEmptyString(password)) {
            	try {
                    if (isAToken(password, def)) {
                    	String token = password;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "PASSWORD"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                mLogger.log(Level.WARNING, "JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSAddress.ATTR_PASSWORD, def.getQName()});
                                AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSAddress.ATTR_JNDI_SECURITY_CRDENTIALS, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0001");
                            } else {
                                jmsAddress.setPassword(metadata[0]);
                            }
                        }
                    } else {
                        jmsAddress.setPassword(password);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }
            }
            
            String connectionFactoryName = DOMUtils.getAttribute(el, JMSAddress.ATTR_JNDI_CONNECTION_FACTORY_NAME);
            if (nonEmptyString(connectionFactoryName)) {
            	try {
                    if (isAToken(connectionFactoryName, def)) {
                    	String token = connectionFactoryName;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                throw new WSDLException("INVALID_WSDL", 
                                mMessages.getString("JMSBC-E0003.EnvironmentVariableHasNoValue", new Object[] {token, JMSAddress.ATTR_JNDI_CONNECTION_FACTORY_NAME, def.getQName()}));
                            }
                            jmsAddress.setConnectionFactoryName(metadata[0]);
                        }
                    } else {
                        jmsAddress.setConnectionFactoryName(connectionFactoryName);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }
                        
            String initialContextFactory = DOMUtils.getAttribute(el, JMSAddress.ATTR_JNDI_INITIAL_CONTEXT_FACTORY);
            if (nonEmptyString(initialContextFactory)) {
                jmsAddress.setInitialContextFactory(initialContextFactory);
            }

            String providerURL = DOMUtils.getAttribute(el, JMSAddress.ATTR_JNDI_PROVIDER_URL);
            if (nonEmptyString(providerURL)) {
            	try {
                    if (isAToken(providerURL, def)) {
                    	String token = providerURL;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                throw new WSDLException("INVALID_WSDL", 
                                mMessages.getString("JMSBC-E0003.EnvironmentVariableHasNoValue", new Object[] {token, JMSAddress.ATTR_JNDI_PROVIDER_URL, def.getQName()}));
                            }
                            jmsAddress.setProviderURL(metadata[0]);
                        }
                    } else {
                        jmsAddress.setProviderURL(providerURL);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }

            String securityPrincipal = DOMUtils.getAttribute(el, JMSAddress.ATTR_JNDI_SECURITY_PRINCIPAL);
            if (nonEmptyString(securityPrincipal)) {
            	try {
                    if (isAToken(securityPrincipal, def)) {
                    	String token = securityPrincipal;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "STRING"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                mLogger.log(Level.WARNING, "JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSAddress.ATTR_JNDI_SECURITY_PRINCIPAL, def.getQName()});
                               AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSAddress.ATTR_JNDI_SECURITY_PRINCIPAL, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0001");
                            } else {
                                jmsAddress.setSecurityPrincipal(metadata[0]);
                            }
                        }
                    } else {
                        jmsAddress.setSecurityPrincipal(securityPrincipal);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }

            String securityCredentials = DOMUtils.getAttribute(el, JMSAddress.ATTR_JNDI_SECURITY_CRDENTIALS);
            if (nonEmptyString(securityCredentials)) {
            	try {
                    if (isAToken(securityCredentials, def)) {
                    	String token = securityCredentials;
                        String envVariableName = getEnvVariableName(token, def);
                        
                        if (!mResolveTokens) {
                            if (!mEnvVariableMap.containsKey(envVariableName)) {
                                String[] metadata = new String[] {null, "PASSWORD"};
                                mEnvVariableMap.put(envVariableName, metadata);                                
                            }
                        } else {
                            String[] metadata = (String[]) mEnvVariableMap.get(envVariableName);
                            if (metadata == null || metadata[0] == null) {
                                mLogger.log(Level.WARNING, "JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSAddress.ATTR_JNDI_SECURITY_CRDENTIALS, def.getQName()});
                                AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0001.EnvironmentVariableHasNoValue",
                                                new Object[] {token, JMSAddress.ATTR_JNDI_SECURITY_CRDENTIALS, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0001");
                            } else {
                                jmsAddress.setSecurityCredentials(metadata[0]);
                            }
                        }
                    } else {
                        jmsAddress.setSecurityCredentials(securityCredentials);
                    }
                } catch (WSDLException e) {
                    throw e;
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }                 
            }
            
            returnValue = jmsAddress;
        } else if (JMSConstants.QNAME_MESSAGE.equals(elementType)) {                        
            JMSMessage message = new JMSMessage();
            
            //
            // unmarshal element(s)
            //
            JMSProperties properties = new JMSProperties();
            unmarshalProperties (properties, el, def);
            message.setProperties(properties);
            
            JMSMapMessage mapMessage = new JMSMapMessage();
            unmarshalMapMessage(mapMessage, el, def);
            message.setMapMessage(mapMessage);
            
            //
            // unmarshal attribute(s)
            //
            String messageType = DOMUtils.getAttribute(el, JMSMessage.ATTR_MESSAGE_TYPE);
            if (nonEmptyString(messageType)) {
                message.setMessageType(messageType);
            }
            
            String use = DOMUtils.getAttribute(el, JMSMessage.ATTR_USE);
            if (nonEmptyString(use)) {
                message.setUse(use);
            }

            String encodingStyle = DOMUtils.getAttribute(el, JMSMessage.ATTR_ENCODING_STYLE);
            if (encodingStyle != null) {
                message.setJMSEncodingStyle(encodingStyle);
            }            
            
            String forwardAsAttachment = DOMUtils.getAttribute(el, JMSMessage.ATTR_FORWARD_AS_ATTACHMENT);
            if (forwardAsAttachment != null) {
            	forwardAsAttachment  = forwardAsAttachment.trim();
            	if(forwardAsAttachment.equalsIgnoreCase("true")){
                    message.setForwardAsAttachment(true);
            	}else if(forwardAsAttachment.equalsIgnoreCase("false")){
                    message.setForwardAsAttachment(false);
            	}else{
                    throw new WSDLException("INVALID_WSDL", 
                            mMessages.getString("JMSBC-E0007.InvalidAttributeValue", new Object[] {forwardAsAttachment, JMSMessage.ATTR_FORWARD_AS_ATTACHMENT, "true/false"}));
            	}
            }

            String bytesPart = DOMUtils.getAttribute(el, JMSMessage.ATTR_BYTES_PART);
            if (bytesPart != null) {
                message.setBytesPart(bytesPart);
            }            
            
            String textPart = DOMUtils.getAttribute(el, JMSMessage.ATTR_TEXTPART);
            if (nonEmptyString(textPart)) {
                message.setTextPart(textPart);
            }
                        
            String correlationIdPart = DOMUtils.getAttribute(el, JMSMessage.ATTR_CORRELATION_ID_PART);
            if (nonEmptyString(correlationIdPart)) {
                message.setCorrelationIdPart(correlationIdPart);
            }
 
            String deliveryModePart = DOMUtils.getAttribute(el, JMSMessage.ATTR_DELIVERY_MODE_PART);
            if (nonEmptyString(deliveryModePart)) {
                message.setDeliveryModePart(deliveryModePart);
            }
            
            String messageIDPart = DOMUtils.getAttribute(el, JMSMessage.ATTR_MESSAGE_ID_PART);
            if (nonEmptyString(messageIDPart)) {
                message.setMessageIDPart(messageIDPart);
            }

            String priorityPart = DOMUtils.getAttribute(el, JMSMessage.ATTR_PRIORITY_PART);
            if (nonEmptyString(priorityPart)) {
                message.setPriorityPart(priorityPart);
            }
            
            String typePart = DOMUtils.getAttribute(el, JMSMessage.ATTR_TYPE_PART);
            if (nonEmptyString(typePart)) {
                message.setTypePart(typePart);
            }         

            String redeliveredPart = DOMUtils.getAttribute(el, JMSMessage.ATTR_REDELIVERED_PART);
            if (nonEmptyString(redeliveredPart)) {
                message.setRedeliveredPart(redeliveredPart);
            }         
            
            String timestampPart = DOMUtils.getAttribute(el, JMSMessage.ATTR_TIMESTAMP_PART);
            if (nonEmptyString(timestampPart)) {
                message.setTimestampPart(timestampPart);
            }            
            
            returnValue = message;
        } 
        
        return returnValue;
    }
                        
    protected void unmarshalOptions (JMSOptions options, Element el, Definition def) throws WSDLException {
        // Find properties element
        NodeList childNodes = el.getChildNodes();
        Element optionsElem = null;
        for (int i=0; i < childNodes.getLength(); i++) {
            Node node = childNodes.item(i);
            if (node instanceof Element) {
                Element elem = (Element)node;
                String localName = elem.getLocalName();
                if (localName.equals(JMSConstants.ELEM_OPTIONS)) {
                    optionsElem = elem;
                    break;
                }
            }
        }
        
        // If options element is found, get the children property elements
        // and populate JMSOptions with them
        if (optionsElem != null) {
            NodeList optionList = optionsElem.getChildNodes();
            if (optionList != null && optionList.getLength() > 0) {
                HashMap optionColl = new HashMap();
                for (int i=0; i < optionList.getLength(); i++) {
                    Node node = optionList.item(i);
                    if (node instanceof Element) {
                        Element option = (Element)optionList.item(i);
                        String localName = option.getLocalName();
                        if (localName.equals(JMSConstants.ELEM_OPTION)) {
                            JMSOption jmsOption = new JMSOption();
                            // Get the option attributes (name, value)
                            List listOfNodes = DOMUtils.getAttributes(option);
                            if (listOfNodes != null && listOfNodes.size() > 0 ) {
                                Iterator nodeIter = listOfNodes.iterator();
                                while (nodeIter.hasNext()) {
                                    Attr attribute = (Attr)nodeIter.next();
                                    String name = attribute.getNodeName();
                                    if (name.equals(JMSOption.ATTR_NAME)) {
                                        jmsOption.setName(attribute.getValue());
                                    } else if (name.equals(JMSOption.ATTR_VALUE)) {
                                        jmsOption.setValue(attribute.getValue());
                                    } else {
                                        mLogger.log(Level.WARNING, "JMSBC-W0002.UnexpectedAttribute",
                                                    new Object[]{name, JMSConstants.ELEM_OPTION, def.getQName()});
                                        AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0002.UnexpectedAttribute",
                                                    new Object[]{name, JMSConstants.ELEM_OPTION, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0002");
                                    }
                                }
                            } else { // elem without attributes
                                String errMsg = mMessages.getString("JMSBC-E0001.ElementHasNoAttributes",
                                            new Object[]{JMSConstants.ELEM_OPTION, def.getQName()});            
                                
                                throw new WSDLException (WSDLException.INVALID_WSDL,
                                                         errMsg);
                            }
                            // Add option to collection
                            optionColl.put(jmsOption.getName(), jmsOption);
                        } else { // unexpected elem
                            String errMsg = mMessages.getString("JMSBC-E0002.UnexpectedElement",
                                        new Object[]{node.getLocalName(), def.getQName()});
                            
                            throw new WSDLException (WSDLException.INVALID_WSDL,
                                                     errMsg);                        
                        }
                    }  
                } // for each jms:option
                
                // Set the array of jms option(s)
                if (optionColl.size() > 0) {
                    options.setOptions(optionColl);
                }
            }
        }        
    }

    protected void unmarshalJNDIEnvAndJMSJCAOptions (JMSAddress jmsAddress, JMSJNDIEnv jndiEnv, JMSJCAOptions jmsjcaOptions, Element el, Definition def) throws WSDLException {
        // Find properties element
        NodeList childNodes = el.getChildNodes();
        Element jndienvElem = null;
        Element jmsjcaElem = null;
        Element classpathElem = null;
        for (int i=0; i < childNodes.getLength(); i++) {
            Node node = childNodes.item(i);
            if (node instanceof Element) {
                Element elem = (Element)node;
                String localName = elem.getLocalName();
                if (localName.equals(JMSConstants.ELEM_JNDIENV)) {
                    jndienvElem = elem;
                }else if(localName.equals(JMSConstants.ELEM_JMSJCAOPTIONS)){
                	jmsjcaElem = elem;
                }else if(localName.equals(JMSConstants.ELEM_CLASSPATH) && elem.getNamespaceURI().equals(JMSConstants.NS_URI_JMS)){
                	classpathElem = elem;
                }
            }
        }
        
        // If jndienv element is found, get the children property elements
        // and populate JMSJNDIEnvEntry with them
        if (jndienvElem != null) {
            NodeList enventryList = jndienvElem.getChildNodes();
            if (enventryList != null && enventryList.getLength() > 0) {
                HashMap enventryColl = new HashMap();
                for (int i=0; i < enventryList.getLength(); i++) {
                    Node node = enventryList.item(i);
                    if (node instanceof Element) {
                        Element jndienventryElem = (Element)enventryList.item(i);
                        String localName = jndienventryElem.getLocalName();
                        if (localName.equals(JMSConstants.ELEM_JNDIENVENTRY)) {
                            JMSJNDIEnvEntry jndienventry = new JMSJNDIEnvEntry();
                            // Get the jndienventry attributes (name, value)
                            List listOfNodes = DOMUtils.getAttributes(jndienventryElem);
                            if (listOfNodes != null && listOfNodes.size() > 0 ) {
                                Iterator nodeIter = listOfNodes.iterator();
                                while (nodeIter.hasNext()) {
                                    Attr attribute = (Attr)nodeIter.next();
                                    String name = attribute.getNodeName();
                                    if (name.equals(JMSJNDIEnvEntry.ATTR_NAME)) {
                                        jndienventry.setName(attribute.getValue());
                                    } else if (name.equals(JMSJNDIEnvEntry.ATTR_VALUE)) {
                                        jndienventry.setValue(attribute.getValue());
                                    } else {
                                        mLogger.log(Level.WARNING, "JMSBC-W0002.UnexpectedAttribute",
                                                    new Object[]{name, JMSConstants.ELEM_JNDIENVENTRY, def.getQName()});
                                        AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0002.UnexpectedAttribute",
                                                    new Object[]{name, JMSConstants.ELEM_JNDIENVENTRY, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0002"); 
                                    }
                                }
                            } else {  // elem without attributes                  
                                String errMsg = mMessages.getString("JMSBC-E0001.ElementHasNoAttributes",
                                            new Object[]{JMSConstants.ELEM_JNDIENVENTRY, def.getQName()});            
                                
                                throw new WSDLException (WSDLException.INVALID_WSDL,
                                                         errMsg);
                            }
                            // Add jndienventry to collection
                            enventryColl.put(jndienventry.getName(), jndienventry);
                        } else { // unexpected elem
                            String errMsg = mMessages.getString("JMSBC-E0002.UnexpectedElement",
                                        new Object[]{node.getLocalName(), def.getQName()});
                            
                            throw new WSDLException (WSDLException.INVALID_WSDL,
                                                     errMsg);                        
                        }
                    }  
                } // for each jms:jndienventry
                
                // Set the collection of jndienventry
                if (enventryColl.size() > 0) {
                    jndiEnv.setJNDIEnvEntries(enventryColl);
                }
            }
        }        
        
        if(jmsjcaElem != null){
        	NodeList nl = jmsjcaElem.getChildNodes();
        	for(int i=0; i<nl.getLength(); ++i){
        		Node n = nl.item(i);
        		if(n instanceof CDATASection){
        			try {
        			jmsjcaOptions.setOptions(n.getNodeValue());
					} catch (Exception e) {
                        String errMsg = mMessages.getString("JMSBC-E0006.InvalidJMSJCAOptions",
                                new Object[]{n.getNodeValue()});            
						throw new WSDLException(WSDLException.INVALID_WSDL, errMsg);
					}
        			break;
        		}
        	}
        }
        
		//We can globally set this system property to make JMSBC use this classpath
		jmsAddress.setClassLoader(GLOBALCLASSLOADER);
        if(classpathElem != null){
        	NodeList nl = classpathElem.getChildNodes();
        	for(int i=0; i<nl.getLength(); ++i){
        		Node n = nl.item(i);
        		if(n instanceof CDATASection){
        			String classpath = n.getNodeValue();
        			URL[] urls = parseClasspath(classpath);
        	        if(urls != null && urls.length > 0){
        	        	jmsAddress.setClassLoader(new URLClassLoader(urls, JMSExtSerializer.class.getClassLoader()));
        			}
        			break;
        		}
        	}
        }
    }

	private static URL[] parseClasspath(String classpath){
		if(classpath == null){
			return EMPTY_ARRAY;
		}
		classpath = classpath.trim();
		if(classpath.length() == 0){
			return EMPTY_ARRAY;
		}
		
		ArrayList<URL> urlList = new ArrayList<URL>();
		for(String str : classpath.split(",")){
			str = str.trim();
			if(str.length() > 0){
				try {
					urlList.add(new URL(str));
				} catch (MalformedURLException e) {
		            String errMsg = mMessages.getString("JMSBC-E0008.InvalidClasspath",
		                    new Object[]{str});
		            mLogger.log(Level.WARNING, errMsg, e);
				}
			}
		}
		return urlList.toArray(EMPTY_ARRAY);
	}
    
    /*
    private void unmarshalTextMessageParts (JMSSendMessageDefinition sendMsgDef,
                                            String spaceSepararedList) {
        StringTokenizer strTokenizer = 
                new StringTokenizer(spaceSepararedList, " ");
        if (strTokenizer.countTokens() > 0) {
            String [] parts = new String [strTokenizer.countTokens()];
            int i=0;
            while (strTokenizer.hasMoreTokens()) {
                String part = (String)strTokenizer.nextToken();
                parts[i++] = part;
            }
            sendMsgDef.setParts(parts);
        }        
    }
    */
    
    protected void unmarshalProperties (JMSProperties properties, Element el, Definition def) 
        throws WSDLException {
        // Find properties element
        NodeList childNodes = el.getChildNodes();
        Element propertiesElem = null;
        for (int i=0; i < childNodes.getLength(); i++) {
            Node node = childNodes.item(i);
            if (node instanceof Element) {
                Element elem = (Element)node;
                String localName = elem.getLocalName();
                if (localName.equals(JMSConstants.ELEM_PROPERTIES)) {
                    propertiesElem = elem;
                    break;
                }
            }
        }
        
        // If properties element is found, get the children property elements
        // and populate JMSProperties with them
        if (propertiesElem != null) {
            NodeList propertyList = propertiesElem.getChildNodes();
            if (propertyList != null && propertyList.getLength() > 0) {
                HashMap propertyColl = new HashMap();
                for (int i=0; i < propertyList.getLength(); i++) {
                    Node node = propertyList.item(i);
                    if (node instanceof Element) {
                        Element property = (Element)propertyList.item(i);
                        String localName = property.getLocalName();
                        if (localName.equals(JMSConstants.ELEM_PROPERTY)) {
                            JMSProperty jmsprop = new JMSProperty();
                            // Get the property attributes (name, part, etc..)
                            List listOfNodes = DOMUtils.getAttributes(property);
                            if (listOfNodes != null && listOfNodes.size() > 0 ) {
                                Iterator nodeIter = listOfNodes.iterator();
                                while (nodeIter.hasNext()) {
                                    Attr attribute = (Attr)nodeIter.next();
                                    String name = attribute.getNodeName();
                                    if (name.equals(JMSProperty.ATTR_NAME)) {
                                        jmsprop.setName(attribute.getValue());
                                    } else if (name.equals(JMSProperty.ATTR_PART)) {
                                        jmsprop.setPart(attribute.getValue());
                                    } else if (name.equals(JMSProperty.ATTR_TYPE)) {
                                        jmsprop.setType(attribute.getValue());
                                    } else {
                                        mLogger.log(Level.WARNING, "JMSBC-W0002.UnexpectedAttribute",
                                                    new Object[]{name, JMSConstants.ELEM_PROPERTY, def.getQName()});
                                        AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0002.UnexpectedAttribute",
                                                    new Object[]{name, JMSConstants.ELEM_PROPERTY, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0002"); 
                                    }
                                }                            
                            } else {  // elem without attributes
                                String errMsg = mMessages.getString("JMSBC-E0001.ElementHasNoAttributes",
                                            new Object[]{JMSConstants.ELEM_PROPERTY, def.getQName()});            
                                
                                throw new WSDLException (WSDLException.INVALID_WSDL,
                                                         errMsg);
                            }
                            // Add property to map
                            propertyColl.put(jmsprop.getName(), jmsprop);
                        } else { // unexpected elem
                            String errMsg = mMessages.getString("JMSBC-E0002.UnexpectedElement",
                                        new Object[]{node.getLocalName(), def.getQName()});
                            
                            throw new WSDLException (WSDLException.INVALID_WSDL,
                                                     errMsg);                        
                        }
                    }  
                } // for each jms:property
                
                // Set the array of jms property
                if (propertyColl.size() > 0) {
                    properties.setProperties(propertyColl);
                }
            }
        }
    }
        
    protected void unmarshalMapMessage (JMSMapMessage mapmessage, Element el, Definition def) 
        throws WSDLException {
        // Find parts element
        NodeList childNodes = el.getChildNodes();
        Element parts = null;
        for (int i=0; i < childNodes.getLength(); i++) {
            Node node = childNodes.item(i);
            if (node instanceof Element) {
                Element elem = (Element)node;
                String localName = elem.getLocalName();
                if (localName.equals(JMSConstants.ELEM_MAPMESSAGE)) {
                    parts = elem;
                    break;
                }
            }
        }
        
        // If mapmessageparts element is found, get the children mappart elements
        // and populate JMSMapMessageParts with them
        if (parts != null) {
            NodeList partList = parts.getChildNodes();
            if (partList != null && partList.getLength() > 0) {
                HashMap partColl = new HashMap();
                for (int i=0; i < partList.getLength(); i++) {
                    Node node = partList.item(i);
                    if (node instanceof Element) {
                        Element part = (Element)partList.item(i);
                        String localName = part.getLocalName();
                        if (localName.equals(JMSConstants.ELEM_MAPPART)) {
                            JMSMapMessagePart jmspart = new JMSMapMessagePart();
                            // Get the mapmessage part attributes (name, part, type)
                            List listOfNodes = DOMUtils.getAttributes(part);
                            if (listOfNodes != null && listOfNodes.size() > 0 ) {
                                Iterator nodeIter = listOfNodes.iterator();
                                while (nodeIter.hasNext()) {
                                    Attr attribute = (Attr)nodeIter.next();
                                    String name = attribute.getNodeName();
                                    if (name.equals(JMSMapMessagePart.ATTR_NAME)) {
                                        jmspart.setName(attribute.getValue());
                                    } else if (name.equals(JMSMapMessagePart.ATTR_PART)) {
                                        jmspart.setPart(attribute.getValue());
                                    } else if (name.equals(JMSMapMessagePart.ATTR_TYPE)) {
                                        jmspart.setType(attribute.getValue());
                                    } else {
                                        mLogger.log(Level.WARNING, "JMSBC-W0002.UnexpectedAttribute",
                                                    new Object[]{name, JMSConstants.ELEM_MAPPART, def.getQName()});
                                        AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0002.UnexpectedAttribute",
                                                    new Object[]{name, JMSConstants.ELEM_MAPPART, def.getQName()}), 
                                              JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                              null, 
                                              AlertsUtil.getServerType(),
                                              AlertsUtil.COMPONENT_TYPE_BINDING,
                                              NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                              NotificationEvent.EVENT_TYPE_ALERT,
                                              "JMSBC-W0002"); 
                                    }
                                }
                            } else {  // elem without attributes
                                String errMsg = mMessages.getString("JMSBC-E0001.ElementHasNoAttributes",
                                            new Object[]{JMSConstants.ELEM_MAPPART, def.getQName()});            
                                
                                throw new WSDLException (WSDLException.INVALID_WSDL,
                                                         errMsg);
                            }
                            // Add map part to map
                            partColl.put(jmspart.getName(), jmspart);
                        } else { // unexpected elem
                            String errMsg = mMessages.getString("JMSBC-E0002.UnexpectedElement",
                                        new Object[]{node.getLocalName(), def.getQName()});
                            
                            throw new WSDLException (WSDLException.INVALID_WSDL,
                                                     errMsg);
                        }
                    }  
                } // for each jms:mappart
                
                // Set the array of jms part
                if (partColl.size() > 0) {
                    mapmessage.setMapMessageParts(partColl);
                }
            }
        }
    }

    public Map getEnvVariableMap() {
        return mEnvVariableMap;
    }

    protected boolean isAToken(String name, Definition def) throws Exception {
    	boolean isToken = false;
    	
        if (name != null && !name.equals("")) {
            if (name.startsWith("${")) {
                if (name.endsWith("}")) {
                    isToken = true;
                } else {
                    throw new Exception(mMessages.getString("JMSBC-E0004.InvalidEnvironmentVariableToken", 
                                        new Object [] {name, def.getQName()}));
                }
            }
        }
        
        return isToken;
    }
    
    protected String getEnvVariableName(String aToken, Definition def) throws Exception {
        String tokenName = aToken.substring(2, aToken.length() - 1);
        if ("".equals(tokenName)) {
            throw new Exception(mMessages.getString("JMSBC-E0005.EmptyEnvironmentVariable", def.getQName()));
        } 
        
        return tokenName;
        
    }    
    
    protected boolean onlyOneHasValue (String s1, String s2) {
        return !(s1 != null && s2 != null) || (s1 == null && s2 == null);
    }
    
    protected boolean onlyOneHasValue (int i1, String s1) {
        return !(i1 != -1 && s1 != null) || (i1 == -1 && s1 == null);
    }
    
    protected boolean nonEmptyString (String strToTest) {
        boolean nonEmpty = false;
        if (strToTest != null && strToTest.length() > 0) {
            nonEmpty = true;
        }
        return nonEmpty;
    }

    protected void tearDown() throws Exception {
    }

    //
    // private methods for testing purposes
    //
    
    protected Definition readWSDL(File f)
        throws javax.wsdl.WSDLException {
        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(f.getParent() +
                                       File.separator +
                                       "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);
        
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader =
            ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new JMSExtensionRegistry(this));
        Definition def = reader.readWSDL(f.getAbsolutePath());
        
        return def;
    }
    
    protected Service getService(Definition def,
                               String serviceName) {
        Map services = def.getServices();
        Service svc = (Service)services.get(QName.valueOf(serviceName));
        return svc;
    }
    
    protected Binding getBinding(Definition def, 
                               String serviceName,
                               String endpointName) {
        
        Service svc = getService(def, serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }
    
    protected void traverseInputsOutputs (File wsdlFile,
                                        String serviceName,
                                        String endpointName) throws Exception {
        Definition def = readWSDL (wsdlFile);
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter =  bindingOperations == null ? 
                                 null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                System.out.println ("=========================");
                System.out.println ("Operation ===> '" + oper.getName() + "'");
                System.out.println ("=========================");
                List extElems = oper.getExtensibilityElements();
                // Look for jms:operation entries
                    
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = 
                            (ExtensibilityElement) extIter.next();
                    if (JMSOperation.class.isInstance(ee)) {
                        JMSOperation jmsOp = (JMSOperation)ee;
                        System.out.println ("jms:operation is\n" + 
                                            jmsOp.toString());
                        
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            System.out.println ("Input ===> '" + bindingInput.getName() + "'");
                            System.out.println ("------------------------");
                            Iterator it = 
                                bindingInput.getExtensibilityElements().iterator();
                            while (it.hasNext()) {
                                ExtensibilityElement ee2 = 
                                        (ExtensibilityElement)it.next();
                                if (ee2 instanceof JMSMessage) {
                                    JMSMessage message = (JMSMessage)ee2;
                                    System.out.println ("jms:message is\n" + 
                                                        message.toString());
                                } 
                            }
                        }

                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            System.out.println ("Output ===> '" + bindingOutput.getName() + "'");
                            System.out.println ("------------------------");
                            Iterator it2 = 
                                bindingOutput.getExtensibilityElements().iterator();
                            while (it2.hasNext()) {
                                ExtensibilityElement ee2 = 
                                        (ExtensibilityElement)it2.next();
                                if (ee2 instanceof JMSMessage) {
                                    JMSMessage message = (JMSMessage)ee2;
                                    System.out.println ("jms:message is\n" + 
                                                        message.toString());
                                } 
                            }
                        }
                    }
                }
                System.out.println("\n\n");
            }
        }
    }
    
    protected void traversePort(File wsdlFile,
                              String serviceName,
                              String endpointName) throws Exception {
        Definition def = readWSDL (wsdlFile);        
        Service svc = getService(def, serviceName);
        if (svc == null) {
            System.out.println("Error - null Service");
            return;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());       
        if (port == null) {
            System.out.println("Error - null Port");
            return;
        } else {
            System.out.println ("=========================");
            System.out.println ("Port ===> '" + port.getName() + "'");
            System.out.println ("=========================");
            List extElems = port.getExtensibilityElements();
                
            //Look for jms:address
            JMSAddress address = null;    
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (JMSAddress.class.isInstance(ee)) {
                    address = (JMSAddress) ee;
                }
            }
            if (address != null) {
                System.out.println ("jms:address is :\n" + address.toString());
            }
        }
    }    
    
    public void sampleParse() throws Throwable {
        File wsdlFile = new File("/temp" + File.separator + 
                                 "JMSBCSonicMQTestOneway.wsdl");
        String serviceName = "{http://j2ee.netbeans.org/wsdl/JMSBCSonicMQTestOneway}JMSBCSonicMQTestService";
        String endpointName = "JMSBCSonicMQTestPortOneWayIn";
        traverseInputsOutputs(wsdlFile, serviceName, endpointName);
        traversePort(wsdlFile, serviceName, endpointName);
    }
    
    public static void main(String [] args) {
        try {
            JMSExtSerializer jmsExt = new JMSExtSerializer();
            jmsExt.sampleParse();
        } catch (Throwable t) {
            t.printStackTrace();;
        }
    }
}
