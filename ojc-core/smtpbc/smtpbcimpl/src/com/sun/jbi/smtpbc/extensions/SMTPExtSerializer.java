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
 * @(#)SMTPExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import java.io.PrintWriter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;

/**
 *
 * @author aegloff
 */
public class SMTPExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {
    
    private static final long serialVersionUID = 1L;
    
    private static final Messages mMessages = Messages.getMessages(SMTPExtSerializer.class);
    protected Map envVarMap = null;
    
    /**
     * Creates a new instance of SMTPExtSerializer 
     */
    public SMTPExtSerializer() {
    }
    
    public SMTPExtSerializer(Map envVarMap){
    	this.envVarMap = envVarMap;
    }

    /** 
     * Registers the serializers / deserializers
     */
    public void registerSerializer(final ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, SMTPBinding.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, SMTPBinding.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, SMTPBinding.QNAME_BINDING, SMTPBinding.class);

        registry.registerSerializer(BindingOperation.class, SMTPOperation.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, SMTPOperation.QNAME_OPERATION, this);        
        registry.mapExtensionTypes(BindingOperation.class, SMTPOperation.QNAME_OPERATION, SMTPOperation.class);

        registry.registerSerializer(Port.class, SMTPAddress.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, SMTPAddress.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, SMTPAddress.QNAME_ADDRESS, SMTPAddress.class);

        registry.registerSerializer(BindingInput.class, SMTPOperationInput.QNAME_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class, SMTPOperationInput.QNAME_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class, SMTPOperationInput.QNAME_OPERATION_INPUT, SMTPOperationInput.class);

        registry.registerSerializer(BindingOutput.class, SMTPOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.registerDeserializer(BindingOutput.class, SMTPOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.mapExtensionTypes(BindingOutput.class, SMTPOperationOutput.QNAME_OPERATION_OUTPUT, SMTPOperationOutput.class);
    }
    
    
    public void marshall(final Class parentType, final QName elementType, final ExtensibilityElement extension,
            final PrintWriter pw, final javax.wsdl.Definition def, final ExtensionRegistry extReg) throws WSDLException {
        if (extension == null) {
            return;
        }

        if (extension instanceof SMTPBinding) {
            pw.print("      <smtp:binding");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof SMTPOperation) {
            pw.print("      <smtp:operation");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof SMTPOperationInput) {

        } else if (extension instanceof SMTPOperationOutput) {

        } else if (extension instanceof SMTPAddress) {
            final SMTPAddress smtpAddress = (SMTPAddress) extension;
            pw.print("      <smtp:address");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            try {
                final MailTo location = smtpAddress.getLocation();
                if (location != null) {
                    DOMUtils.printAttribute(SMTPAddress.ATTR_LOCATION, location.marshal(), pw);
                }
                final String smtpServer = smtpAddress.getSMTPServer();
                if (smtpServer != null)
                {
                    DOMUtils.printAttribute(SMTPAddress.ATTR_SMTPSERVER, smtpServer, pw);
                }
            } catch (final Exception ex) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                                        "Failed to marshal location attribute",
                                        ex);
            }
            pw.println("/>");
        }
    }
    
    
    public javax.wsdl.extensions.ExtensibilityElement unmarshall(final Class parentType, final QName elementType,
            final Element el, final Definition def, final ExtensionRegistry extReg) throws WSDLException {

        ExtensibilityElement returnValue = null;

        if (SMTPBinding.QNAME_BINDING.equals(elementType)) {
            final SMTPBinding smtpBinding = new SMTPBinding();

            unmarshalSMTPProxy(smtpBinding, el);

            returnValue = smtpBinding;
        } else if (SMTPOperation.QNAME_OPERATION.equals(elementType)) {
            final SMTPOperation smtpOperation = new SMTPOperation();
            returnValue = smtpOperation;
        } else if (SMTPOperationInput.QNAME_OPERATION_INPUT.equals(elementType)) {
            final SMTPOperationInput smtpOperationInput = new SMTPOperationInput();

            final String message = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_MESSAGE);
            if (message != null) {
                smtpOperationInput.setMessage(message);
            }

            final String subject = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_SUBJECT);
            if (subject != null) {
                smtpOperationInput.setSubject(subject);
            }

            final String from = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_FROM);
            if (from != null) {
                smtpOperationInput.setFrom(from);
            }

            final String to = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_TO);
            if (to != null) {
                smtpOperationInput.setTo(to);
            }

            final String cc = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_CC);
            if (cc != null) {
                smtpOperationInput.setCc(cc);
            }
            final String bcc = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_BCC);
            if (bcc != null) {
                smtpOperationInput.setBcc(bcc);
            }            

            final String charset = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_CHARSET);
            if (charset != null) {
                smtpOperationInput.setCharset(charset);
            }
            
            final String useType = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_USE_TYPE);
            if (useType != null) {
                smtpOperationInput.setSmtpUseType(useType);
            }    
            
            final String encodingStyle = DOMUtils.getAttribute(el, SMTPOperationInput.ATTR_ENCODING_STYLE);
            if (encodingStyle != null) {
                smtpOperationInput.setEncodingStyle(encodingStyle);
            }            

            unmarshalSMTPAttachments(smtpOperationInput, el);

            returnValue = smtpOperationInput;
        } else if (SMTPOperationOutput.QNAME_OPERATION_OUTPUT.equals(elementType)) {
            final SMTPOperationOutput smtpOperationOutput = new SMTPOperationOutput();
            returnValue = smtpOperationOutput;
        } else if (SMTPAddress.QNAME_ADDRESS.equals(elementType)) {
            final SMTPAddress smtpAddress = new SMTPAddress();
            
            try {
                String location = DOMUtils.getAttribute(el, SMTPAddress.ATTR_LOCATION);
                if (location != null) {
                	try {
						if(isAToken(location)){
							location = (String)envVarMap.get(getEnvVariableName(location));
						    if (location == null || "".equals(location)) {
						        throw new WSDLException("INVALID_WSDL", 
						        mMessages.getString("FES_Invalid_token_no_value", new Object[] {location, SMTPAddress.ATTR_LOCATION}));
						    }                		
						} else if(containsToken(location)){
							List emailIdTokens = getEnvVariableNames(location);
							Iterator it = emailIdTokens.iterator();
							//Validating if all the tokens have values
							while(it.hasNext()){
								String emailIdToken = (String)it.next();
								String emailId = (String)envVarMap.get(emailIdToken);
								if(emailId == null || "".equals(emailId)){
							        throw new WSDLException("INVALID_WSDL", 
									        mMessages.getString("FES_Invalid_token_no_value", new Object[] {emailIdToken, SMTPAddress.ATTR_LOCATION}));
								}
								location = replaceTokenWithValue(location,emailIdToken,emailId);
							}
						}
						final MailTo mailTo = new MailTo(location);
						smtpAddress.setLocation(mailTo);
					} catch (WSDLException e) {
						throw e;	
					} catch (Exception e) {
						throw new WSDLException("INVALID_WSDL", e.getMessage());
					}
                }
                String smtpserver = DOMUtils.getAttribute(el, SMTPAddress.ATTR_SMTPSERVER);
                if (smtpserver != null)
                {
                	try {
						if(isAToken(smtpserver)){
							smtpserver = (String)envVarMap.get(getEnvVariableName(smtpserver));
							if(smtpserver == null || "".equals(smtpserver)){
						        throw new WSDLException("INVALID_WSDL", 
								        mMessages.getString("FES_Invalid_token_no_value", new Object[] {location, SMTPAddress.ATTR_SMTPSERVER}));
							}
						}
						smtpAddress.setSMTPServer(smtpserver);
					} catch(WSDLException e){
						throw e;
					} catch (Exception e) {
						throw new WSDLException("INVALID_WSDL", e.getMessage());
					} 
                }
                String smtpport = DOMUtils.getAttribute(el,SMTPAddress.ATTR_SMTPPORT);
                if(smtpport != null){
                	try {
						if(isAToken(smtpport)){
							smtpport = (String)envVarMap.get(getEnvVariableName(smtpport));
							if(smtpport == null || "".equals(smtpport)){
						        throw new WSDLException("INVALID_WSDL", 
								        mMessages.getString("FES_Invalid_token_no_value", new Object[] {location, SMTPAddress.ATTR_SMTPPORT}));
							}
						}
						smtpAddress.setSMTPPort(Integer.parseInt(smtpport));
					} catch (WSDLException e) {
						throw e;
					} catch (Exception e){
						throw new WSDLException("INVALID_WSDL", e.getMessage());						
					}
                }
                String username = DOMUtils.getAttribute(el ,SMTPAddress.ATTR_USERNAME);
                if(username != null){
                	try {
						if(isAToken(username)){
							username = (String)envVarMap.get(getEnvVariableName(username));
							if(username == null || "".equals(username)){
						        throw new WSDLException("INVALID_WSDL", 
								        mMessages.getString("FES_Invalid_token_no_value", new Object[] {location, SMTPAddress.ATTR_USERNAME}));
							}
						}
						smtpAddress.setUserName(username);
					} catch (WSDLException e) {
						throw e;
					}catch (Exception e) {
						throw new WSDLException("INVALID_WSDL", e.getMessage());
					}
                }
                String password = DOMUtils.getAttribute(el, smtpAddress.ATTR_PASSWORD);
                if(password != null){
                	try {
						if(isAToken(password)){
							password = (String)envVarMap.get(getEnvVariableName(password));
							if(password == null || "".equals(password)){
						        throw new WSDLException("INVALID_WSDL", 
								        mMessages.getString("FES_Invalid_token_no_value", new Object[] {location, SMTPAddress.ATTR_PASSWORD}));
							}
						}
						smtpAddress.setPassWord(password);
					} catch (WSDLException e) {
						throw e;
					} catch (Exception e) {
						throw new WSDLException("INVALID_WSDL", e.getMessage());
					}
                }
                String useSSL = DOMUtils.getAttribute(el ,smtpAddress.ATTR_USESSL);
                if(useSSL!=null){
                	if(isAToken(useSSL)){
                		try {
							useSSL = (String)envVarMap.get(getEnvVariableName(useSSL));
							if(useSSL == null || "".equals(useSSL)){
							    throw new WSDLException("INVALID_WSDL", 
								        mMessages.getString("FES_Invalid_token_no_value", new Object[] {location, SMTPAddress.ATTR_USESSL}));
							}
						} catch (WSDLException e) {
							throw e;	
						} catch (Exception e){
							throw new WSDLException("INVALID_WSDL", e.getMessage());							
						}
                	}
                    smtpAddress.setUseSSL(Boolean.parseBoolean(useSSL));
                }
            } catch (final Exception ex) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                                        "Failed to unmarshal location attribute",
                                        ex);
            }
            
            returnValue = smtpAddress;
        }
        
        return returnValue;
    }

    private void unmarshalSMTPProxy(final SMTPBinding smtpBinding, final Element el) {
        final NodeList nl = el.getElementsByTagNameNS(SMTPConstants.NS_URI_SMTP, SMTPProxy.ELEM_PROXY);
        if((nl != null) && (nl.getLength() > 0)) {
            //get the proxy elemnet
            final Element proxyElem = (Element) nl.item(0);
			
            final SMTPProxy proxy = new SMTPProxy();
            final String host = DOMUtils.getAttribute(proxyElem, SMTPProxy.ATTR_HOST);
            if(host != null) {
                proxy.setHost(host);
            }

            final String port = DOMUtils.getAttribute(proxyElem, SMTPProxy.ATTR_PORT);
            if (port != null) {
                proxy.setPort(Integer.valueOf(port));
            }

            final String userName = DOMUtils.getAttribute(proxyElem, SMTPProxy.ATTR_USERNAME);
            if (userName != null) {
                proxy.setUserName(userName);
            }

            final String password = DOMUtils.getAttribute(proxyElem, SMTPProxy.ATTR_PORT);
            if (password != null) {
                proxy.setPassword(password);
            }

            final String sessionAuth = DOMUtils.getAttribute(proxyElem, SMTPProxy.ATTR_SESSION_AUTHENTICATION);
            if (sessionAuth != null) {
                proxy.setSessionAuthentication(Boolean.valueOf(sessionAuth));
            }

            smtpBinding.setSMTPProxy(proxy);
        }
        
    }
    
    private void unmarshalSMTPAttachments(final SMTPOperationInput smtpOperationInput, final Element el) {
        
    }
    
    public Map getEnvVariableMap() {
        return envVarMap;
    }
    
    protected boolean isAToken(String name) throws Exception {
    	boolean isToken = false;
    	
        if (name.startsWith("${")) {
            if (name.endsWith("}")) {
                isToken = true;
            } else {
                throw new Exception(mMessages.getString("FES_Invalid_token_name", name));
            }
        }
        
        return isToken;
    }
    
    protected String getEnvVariableName(String aToken) throws Exception {
        String tokenName = null;
        
        if (aToken == null || "".equals(aToken)) {
            throw new Exception(mMessages.getString("FES_Invalid_token_name", aToken));
        }
            
        tokenName = aToken.substring(2, aToken.length() - 1);
        if ("".equals(tokenName)) {
            throw new Exception(mMessages.getString("FES_Invalid_empty_token_name", aToken));
        } 
        
        return tokenName;
        
    }
    
    protected boolean containsToken(String name) throws Exception {
    	boolean containsToken = false;
    	int index = name.indexOf("${");
    	if(index != -1){
    		int index2 = name.indexOf("}");
    		if(index2 != -1){
    			containsToken = true;
    		}
    	}
    	return containsToken;
    }
    
    protected List getEnvVariableNames(String str) throws Exception {
    	ArrayList envVarNames = new ArrayList();
    	String [] tokens = str.split("\\$\\{");
    	for(int i=0;i<tokens.length;i++){
    		int index = tokens[i].indexOf("}");
                if(index == -1){
                    continue; //skipping a non token
                }
    		String token = tokens[i].substring(0,index);
    		envVarNames.add(token);
    	}
    	return envVarNames;
    }
    
    protected String replaceTokenWithValue(String strToReplace,String token,String value){
    	strToReplace = strToReplace.replaceAll("\\$\\{"+token+"\\}",value);
    	return strToReplace;
    }
}
