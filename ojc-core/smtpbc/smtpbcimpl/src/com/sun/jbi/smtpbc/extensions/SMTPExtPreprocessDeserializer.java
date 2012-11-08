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
 * @(#)SMTPExtPreprocessDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;

public class SMTPExtPreprocessDeserializer extends SMTPExtSerializer {

	
    private static final Messages mMessages = Messages.getMessages(SMTPExtPreprocessDeserializer.class);
	
	public SMTPExtPreprocessDeserializer() {
		super();
	}
	
	public SMTPExtPreprocessDeserializer(Map envVarMap){
		super(envVarMap);
		
	}

	public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el, Definition def, ExtensionRegistry extReg) 
		throws WSDLException {
		
        ExtensibilityElement returnValue = null;
        
        if (SMTPAddress.QNAME_ADDRESS.equals(elementType)) {

            final SMTPAddress smtpAddress = new SMTPAddress();
            

                String location = DOMUtils.getAttribute(el, SMTPAddress.ATTR_LOCATION);
                if (location != null) {
                	try {
                                if(isAToken(location)){
                                    String envVarName = getEnvVariableName(location);
                                    if (!envVarMap.containsKey(envVarName)) {
	                    		envVarMap.put(envVarName, null);
                                    }
                                } else if(containsToken(location)){
                                    List emailIdTokens = getEnvVariableNames(location);
                                    Iterator it = emailIdTokens.iterator();
                                    while(it.hasNext()){
                                            String emailIdToken = (String)it.next();
                                            if(!envVarMap.containsKey(emailIdToken)){
                                                    envVarMap.put(emailIdToken,null);
                                            }
                                    }
                            }
                    } catch (Exception e) {
                        e.printStackTrace();
                            throw new WSDLException("INVALID_WSDL", e.getMessage());
                    }
                }
                String smtpserver = DOMUtils.getAttribute(el, SMTPAddress.ATTR_SMTPSERVER);
                if (smtpserver != null)
                {
            	try {
						if(isAToken(smtpserver)){
							String envVarName = getEnvVariableName(smtpserver);
	                    	if (!envVarMap.containsKey(envVarName)) {
	                    		envVarMap.put(envVarName, null);
	                    	}
						}
					} catch (Exception e) {
						throw new WSDLException("INVALID_WSDL", e.getMessage());
					} 
                }
                String smtpport = DOMUtils.getAttribute(el,SMTPAddress.ATTR_SMTPPORT);
                if(smtpport != null){
                	try {
						if(isAToken(smtpport)){
							String envVarName = getEnvVariableName(smtpport);
	                    	if (!envVarMap.containsKey(envVarName)) {
	                    		envVarMap.put(envVarName, null);
	                    	}
						}
                	} catch (Exception e){
						throw new WSDLException("INVALID_WSDL", e.getMessage());						
					}
                }
                String username = DOMUtils.getAttribute(el ,SMTPAddress.ATTR_USERNAME);
                if(username != null){
                	try {
						if(isAToken(username)){
							String envVarName = getEnvVariableName(username);
	                    	if (!envVarMap.containsKey(envVarName)) {
	                    		envVarMap.put(envVarName, null);
	                    	}
						}
					}catch (Exception e) {
						throw new WSDLException("INVALID_WSDL", e.getMessage());
					}
                }
                String password = DOMUtils.getAttribute(el, smtpAddress.ATTR_PASSWORD);
                if(password != null){
                	try {
						if(isAToken(password)){
							String envVarName = getEnvVariableName(password);
	                    	if (!envVarMap.containsKey(envVarName)) {
	                    		envVarMap.put(envVarName, null);
	                    	}
						}
					} catch (Exception e) {
						throw new WSDLException("INVALID_WSDL", e.getMessage());
					}
                }
                String useSSL = DOMUtils.getAttribute(el ,smtpAddress.ATTR_USESSL);
                if(useSSL!=null){
            		try {                	
            			if(isAToken(useSSL)){
                			String envVarName = getEnvVariableName(useSSL);
	                    	if (!envVarMap.containsKey(envVarName)) {
	                    		envVarMap.put(envVarName, null);
	                    	}
						}
            		} catch (Exception e){
							throw new WSDLException("INVALID_WSDL", e.getMessage());							
					}
            	}
                returnValue = smtpAddress;                
            }
            return returnValue;
	}

}
