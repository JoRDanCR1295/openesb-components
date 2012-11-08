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
 * @(#)SOAPExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.extensions;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.httpsoapbc.extensions.BasicAuthenticationDetail.CredentialValidationType;
import com.sun.jbi.internationalization.Messages;

import java.io.Serializable;
import java.io.PrintWriter;
import java.util.Map;

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
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;

/**
 *
 *
 * @version      
 *
 */
public class SOAPExtSerializer
    implements ExtensionSerializer, ExtensionDeserializer, Serializable {
    private static final long serialVersionUID = 1L;
    private static final Messages mMessages = Messages.getMessages(SOAPExtSerializer.class);
    
    // Policy element constants
    // No I18N
    private static final String WSP_NAMESPACE = "http://schemas.xmlsoap.org/ws/2004/09/policy";
    private static final String SP_NAMESPACE = "http://schemas.xmlsoap.org/ws/2005/07/securitypolicy";
    private static final String MY_SP_NAMESPACE = "http://sun.com/ws/httpbc/security/BasicauthSecurityPolicy";
    private static final String WSU_NAMESPACE = "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd";
    private static final String USER_TOKEN_NAME_SPACE = "http://schemas.xmlsoap.org/ws/2005/07/securitypolicy/IncludeToken/AlwaysToRecipient";
    
    private RuntimeConfigurationMBean mRuntimeConfig = null;
    private Map mApplicationVariablesMap = null;
    private boolean mResolveTokens = false;
    
    public SOAPExtSerializer(RuntimeConfigurationMBean runtimeConfig, boolean resolveTokens) {
    	mRuntimeConfig = runtimeConfig;
        mApplicationVariablesMap = runtimeConfig.retrieveApplicationVariablesMap();
        mResolveTokens = resolveTokens;
    }

    public void registerSerializer(ExtensionRegistry registry) {

        registry.registerSerializer(Port.class, 
                                    PolicyReference.QNAME_ADDRESS,
                                    this);
        registry.registerDeserializer(Port.class,
                                      PolicyReference.QNAME_ADDRESS,
                                      this);
        registry.mapExtensionTypes(Port.class,
                                   PolicyReference.QNAME_ADDRESS,
                                   PolicyReference.class);
        
        registry.registerSerializer(Definition.class, 
                                    Policy.QNAME_ADDRESS,
                                    this);
        registry.registerDeserializer(Definition.class,
                                      Policy.QNAME_ADDRESS,
                                      this);
        registry.mapExtensionTypes(Definition.class,
                                   Policy.QNAME_ADDRESS,
                                   Policy.class);
        
    }

    public void marshall(Class parentType, QName elementType,
                         ExtensibilityElement extension, PrintWriter pw,
                         Definition def, ExtensionRegistry extReg)
        throws WSDLException {
        // NOTE: no I18N
        String wspNs = (def.getPrefix(WSP_NAMESPACE) != null && !def.getPrefix(WSP_NAMESPACE).equals("")) ? def.getPrefix(WSP_NAMESPACE) : "wsp";
        String myspNs = (def.getPrefix(MY_SP_NAMESPACE) != null && !def.getPrefix(MY_SP_NAMESPACE).equals(""))? def.getPrefix(MY_SP_NAMESPACE) : "mysp";
        String wsuNs =  (def.getPrefix(WSU_NAMESPACE) != null && !def.getPrefix(WSU_NAMESPACE).equals(""))? def.getPrefix(WSU_NAMESPACE) : "wsu";
        String spNs = (def.getPrefix(SP_NAMESPACE) != null && !def.getPrefix(SP_NAMESPACE).equals(""))? def.getPrefix(SP_NAMESPACE) : "sp";
        if (extension instanceof Policy) {
            Policy policy = (Policy) extension;
            
            pw.print("<" + wspNs+":Policy");
            String policyId = policy.getID();
            if (policyId != null && !policyId.equals("")) {
                DOMUtils.printAttribute(wsuNs + ":Id", policyId, pw);
            }
            pw.print(">");
            if (policy.getMustSupportBasicAuthentication() != null) {
                MustSupportBasicAuthentication msba = policy.getMustSupportBasicAuthentication();
                pw.print("<" + myspNs + ":MustSupportBasicAuthentication");
                String authEnabled = msba.getAuthEnabled() == Boolean.TRUE? "true" : "false";
                DOMUtils.printAttribute("on",authEnabled, pw);
                pw.print(">");
                if (policy.getBasicAuthenticationDetail() != null) {
                    boolean useUserNameToken = false;
                    BasicAuthenticationDetail detail = policy.getBasicAuthenticationDetail();
                    pw.print("<" + myspNs + ":BasicAuthenticationDetail>");
                    CredentialValidationType type = detail.getCredentialValidationType();
                    if (type == CredentialValidationType.StringCompare) {
                        useUserNameToken = true;
                        pw.print("<" + myspNs + ":WssTokenCompare/>");
                    } else if (type == CredentialValidationType.AM) {
                        pw.print("<" + myspNs + ":AccessManager/>");
                    } else if (type == CredentialValidationType.Realm) {
                        pw.print("<" + myspNs + ":Realm");
                        RealmValidation realm = (RealmValidation) detail.getCredentialValidation();
                        if (realm.getRealmName() != null) {
                            DOMUtils.printAttribute("realmName", realm.getRealmName(), pw);
                        }
                        pw.print("/>");
                    }
                    pw.print("</" + myspNs + ":BasicAuthenticationDetail>");
                    pw.print("</" + myspNs + ":MustSupportBasicAuthentication>");

                    if (useUserNameToken) {
                        pw.print("<" + myspNs + ":UsernameToken>");
                        DOMUtils.printAttribute(myspNs + ":IncludeToken", USER_TOKEN_NAME_SPACE, pw);
                        pw.println(">");
                        pw.print("<" + wspNs +":Policy>");
                        pw.print("<" + spNs +":WssUsernameToken10>");
                        StringCompareValidation stringCompare = (StringCompareValidation) policy.getBasicAuthenticationDetail().getCredentialValidation();
                        if (stringCompare.getUsername() != null) {
                            pw.print(stringCompare.getUsername());
                        }
                        pw.print("</" + spNs +":WssUsernameToken10>");
                        if (stringCompare.getPassword() != null) {
                            pw.print("<" + spNs +":WssPassword>******</" + spNs + ":WssPassword>");
                        }
                        pw.print("</" + wspNs + ":Policy>");
                        pw.print("</" + myspNs + ":UsernameToken>");
                    }
            }
            }
            pw.print("</" + wspNs + ":Policy>");
        } else if (extension instanceof PolicyReference) {
            PolicyReference pref = (PolicyReference) extension;
            pw.print("<" + wspNs + ":PolicyReference");
            String uri = pref.getURI();
            if (uri != null && !uri.equals("")) {
                DOMUtils.printAttribute("URI", uri, pw);
            }
            pw.println("/>");
        } else {
            ExtensionSerializer extSerializer = extReg.getDefaultSerializer();
            extSerializer.marshall(parentType, elementType, extension, pw, def, extReg);
        }
    }

    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
                                           Element el, Definition def,
                                           ExtensionRegistry extReg)
        throws WSDLException {

        if(Policy.QNAME_ADDRESS.equals(elementType)) {
            String wsuNs =  (def.getPrefix(WSU_NAMESPACE) != null && !def.getPrefix(WSU_NAMESPACE).equals(""))? def.getPrefix(WSU_NAMESPACE) : "wsu";
            String id = DOMUtils.getAttribute(el, wsuNs + ":Id");
            Policy pol = null;
            if(id != null) {                                
                NodeList list = el.getElementsByTagNameNS(Policy.NS_URI_BASIC_AUTHENTICATION_SECURITY_POLICY, "MustSupportBasicAuthentication");
                if(list != null && list.getLength() > 0) {
                    Node msbaNode = list.item(0);
                    MustSupportBasicAuthentication msba = new MustSupportBasicAuthentication();
                    NamedNodeMap attrs = msbaNode.getAttributes();
                    Node onNode = attrs.getNamedItem(MustSupportBasicAuthentication.ATTR_ON);
                    if (onNode != null) {
                        String on = onNode.getNodeValue();
                        msba.setAuthEnabled(Boolean.valueOf(on.equalsIgnoreCase("true") ||
                                                            on.equalsIgnoreCase("yes")  ||
                                                            on.equalsIgnoreCase("1")));
                    }
                    
                    BasicAuthenticationDetail bad = new BasicAuthenticationDetail();
                    try {
                        getAuthenticationDetail(msbaNode, bad);
                    } catch (Exception e) {
                        throw new WSDLException("INVALID_WSDL", e.getMessage(), e);                        
                    }
                    
                    pol = new Policy();
                    pol.setID(id);
                    pol.setMustSupportBasicAuthentication(msba);
                    pol.setBasicAuthenticationDetail(bad);
                    
                    NodeList userNameList = el.getElementsByTagNameNS(Policy.NS_URI_SECURITY_POLICY, "WssUsernameToken10");                    
                    if(userNameList != null) {
                        Element userElem = (Element)userNameList.item(0);
                        if(userElem != null && userElem.getChildNodes().getLength() > 0) {
                            String username = userElem.getChildNodes().item(0).getNodeValue();
                            try {
                                if (isAToken(username)) {
                        	    String token = username;
                        	    String appVariableName = getApplicationVariableName(token);
                        	        
                        	    if (!mResolveTokens) {
                        	        if (!mApplicationVariablesMap.containsKey(appVariableName)) {
                        	            String[] metadata = new String[] {null, "STRING"};
                        	            mApplicationVariablesMap.put(appVariableName, metadata);
                        	        }
                        	    } else {
                        	    	String[] metadata = (String[]) mApplicationVariablesMap.get(appVariableName);
                        	    	
                        	    	if (metadata == null || metadata[0] == null) {
                        	    	    throw new Exception(mMessages.getString("HTTPBC-E00252.Application_variable_not_defined", token));
                        	    	}
                                        
                                        pol.setUserName(metadata[0]);
                                        
                                        if (bad.getCredentialValidation() instanceof StringCompareValidation) {                                            
                                            StringCompareValidation scv = (StringCompareValidation)bad.getCredentialValidation();
                                            scv.setUsername(metadata[0]);
                                        }
                        	    }
                        	} else {
                        	    pol.setUserName(username);
                                    
                                    if (bad.getCredentialValidation() instanceof StringCompareValidation) {                                            
                                        StringCompareValidation scv = (StringCompareValidation)bad.getCredentialValidation();
                                        scv.setUsername(username);
                                    }
                        	}
                            } catch (Exception e) {
                                throw new WSDLException("INVALID_WSDL", e.getMessage(), e);
                            }
                        }
                    }
                    NodeList passwordList = el.getElementsByTagNameNS(Policy.NS_URI_SECURITY_POLICY, "WssPassword");
                    if(passwordList != null) {
                        Element passwordElem = (Element)passwordList.item(0);
                        if(passwordElem != null && passwordElem.getChildNodes().getLength() > 0) {
                            String password = passwordElem.getChildNodes().item(0).getNodeValue();
                            try {
                                if (isAToken(password)) {
                    	            String token = password;
                    	            String appVariableName = getApplicationVariableName(token);
                    	          
                    	            if (!mResolveTokens) {
                        	        if (!mApplicationVariablesMap.containsKey(appVariableName)) {
                        	            String[] metadata = new String[] {null, "PASSWORD"};	
                        	            mApplicationVariablesMap.put(appVariableName, metadata);
                        	        }
                        	    } else {
                        	    	String[] metadata = (String[]) mApplicationVariablesMap.get(appVariableName);
                        	        if (metadata == null || metadata[0] == null) {
                        	            throw new Exception(mMessages.getString("HTTPBC-E00252.Application_variable_not_defined", token));
                        	        } 
                        	        
                                        pol.setPassword(metadata[0]);
                                        
                                        if (bad.getCredentialValidation() instanceof StringCompareValidation) {                                            
                                            StringCompareValidation scv = (StringCompareValidation)bad.getCredentialValidation();
                                            scv.setPassword(metadata[0].toCharArray());
                                        }
                        	    }
                                } else {
                        	    pol.setPassword(password);
                                    
                                    if (bad.getCredentialValidation() instanceof StringCompareValidation) {                                            
                                        StringCompareValidation scv = (StringCompareValidation)bad.getCredentialValidation();
                                        scv.setPassword(password.toCharArray());
                                    }
                        	}                                
                            } catch (Exception e) {
                                throw new WSDLException("INVALID_WSDL", e.getMessage(), e);
                            }
                        }
                    }
                    if ((bad.getCredentialValidationType() == BasicAuthenticationDetail.CredentialValidationType.StringCompare) &&
                        (userNameList == null || passwordList == null)) {
                        throw new WSDLException("INVALID_WSDL", mMessages.getString("HTTPBC-E00256.WssTokenCompare_authentication_no_username_or_password"));
                    }
                
                    return pol;
                }
            }
        }

        if(PolicyReference.QNAME_ADDRESS.equals(elementType)) {
            String URI = DOMUtils.getAttribute(el, "URI");
            if(URI != null) {
                PolicyReference pref = new PolicyReference();
                pref.setURI(URI);
                return pref;
            }
        }
        ExtensionDeserializer extDeserializer = extReg.getDefaultDeserializer();
        try {
            if (!mResolveTokens) {
                mRuntimeConfig.updateApplicationVariablesMap(mApplicationVariablesMap);
            }
        } catch (Exception e) {
            throw new WSDLException("OTHER_ERROR", mMessages.getString("HTTPBC-E00251.Application_variable_values_update_failed"), e);
        }
        return extDeserializer.unmarshall(parentType, elementType, el, def, extReg);
    }

   
    protected boolean isAToken(String name) throws Exception {
    	boolean isToken = false;
    	
        if (name.startsWith("${")) {
            if (name.endsWith("}")) {
                isToken = true;
            } else {
                throw new Exception(mMessages.getString("HTTPBC-E00253.Token_name_invalid", name));
            }
        }
        
        return isToken;
    }
    
    protected String getApplicationVariableName(String aToken) throws Exception {
        String tokenName = null;
        
        if (aToken == null || "".equals(aToken)) {
            throw new Exception(mMessages.getString("HTTPBC-E00253.Token_name_invalid", aToken));  // fixme!
        }
            
        tokenName = aToken.substring(2, aToken.length() - 1);
        if ("".equals(tokenName)) {
            throw new Exception(mMessages.getString("HTTPBC-E00253.Token_name_invalid", aToken)); // fixme!
        } 
        
        return tokenName;
        
    }

    private void getAuthenticationDetail (Node mustSupportAuthNode, BasicAuthenticationDetail detail) throws Exception {
        if (mustSupportAuthNode != null) {
            NodeList list = mustSupportAuthNode.getChildNodes();
            if (list == null) { // fall back to string compare 
                detail.setCredentialValidationType(BasicAuthenticationDetail.CredentialValidationType.StringCompare);
                detail.setCredentialValidation(new StringCompareValidation());                
            } else {
                for (int i=0; i < list.getLength(); i++) {
                    Node cNode = list.item(i);
                    if (cNode.getNodeType() == Node.ELEMENT_NODE) {
                        Element elem = (Element)cNode;
                        String elemLN = elem.getLocalName();
                        if (elemLN.equals(BasicAuthenticationDetail.ELEM_BasicAuthenticationDetail)) {
                            NodeList detailChildren = cNode.getChildNodes();
                            if (detailChildren == null) { // fall back to string compare
                                detail.setCredentialValidationType(BasicAuthenticationDetail.CredentialValidationType.StringCompare);
                                detail.setCredentialValidation(new StringCompareValidation());                
                            } else {
                                for (int ii=0; ii < detailChildren.getLength(); ii++) {
                                    Node detailChild = detailChildren.item(ii);
                                    if (detailChild.getNodeType() == Node.ELEMENT_NODE) {
                                        Element dcelem = (Element)detailChild;
                                        String dcelemLN = dcelem.getLocalName();
                                        if (dcelemLN.equals(StringCompareValidation.ELEM_StringCompare)) {
                                            detail.setCredentialValidationType(BasicAuthenticationDetail.CredentialValidationType.StringCompare);
                                            detail.setCredentialValidation(new StringCompareValidation()); 
                                            break;
                                        } else if (dcelemLN.equals(AccessManagerValidation.ELEM_AccessManager)) {
                                            AccessManagerValidation am = new AccessManagerValidation();
                                            detail.setCredentialValidationType(BasicAuthenticationDetail.CredentialValidationType.AM);
                                            detail.setCredentialValidation(am);
                                            NamedNodeMap attrs = detailChild.getAttributes();
                                            Node authNode = attrs.getNamedItem(AccessManagerValidation.ATTR_AUTHORIZATION);
                                            if(authNode != null){
                                            	String authValue = authNode.getNodeValue();
                                            	am.setAuthorization(authValue);
                                            }
                                            break;
                                        } else if (dcelemLN.equals(RealmValidation.ELEM_Realm)) {
                                            RealmValidation rv = new RealmValidation();
                                            NamedNodeMap attrs = detailChild.getAttributes();
                                            Node realmNameNode = attrs.getNamedItem(RealmValidation.ATTR_REALM_NAME);
                                            if (realmNameNode != null) {
                                                String realmName = realmNameNode.getNodeValue();
                                                if (isAToken(realmName)) {
                                                    String token = realmName;
                                                    String appVariableName = getApplicationVariableName(token);
                                                    if (!mResolveTokens) {
                                                        if (!mApplicationVariablesMap.containsKey(appVariableName)) {
                                                            String[] metadata = new String[] {null, "STRING"};
                                                            mApplicationVariablesMap.put(appVariableName, metadata);
                                                        }
                                                    } else {
                                                        String[] metadata = (String[]) mApplicationVariablesMap.get(appVariableName);
                                                        if (metadata == null || metadata[0] == null) {
                                                            throw new Exception(mMessages.getString("HTTPBC-E00252.Application_variable_not_defined", token));
                                                        }
                                                        rv.setRealmName(metadata[0]);                       
                                                    }
                                                } else {
                                                    rv.setRealmName(realmName);
                                                }                            
                                            } else {
                                                throw new Exception (mMessages.getString("HTTPBC-E00255.Realm_authentication_no_realm_name"));
                                            }
                                            detail.setCredentialValidationType(BasicAuthenticationDetail.CredentialValidationType.Realm);
                                            detail.setCredentialValidation(rv);                                                                                        
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }            
        }
    }
}
