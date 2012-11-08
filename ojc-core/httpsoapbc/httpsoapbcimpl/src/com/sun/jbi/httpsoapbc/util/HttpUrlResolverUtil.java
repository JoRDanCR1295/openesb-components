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
 * @(#)ConfigReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.internationalization.Messages;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.UnknownHostException;

public class HttpUrlResolverUtil {
    private static final Messages mMessages =
        Messages.getMessages(HttpUrlResolverUtil.class);
    
    protected static final String TOKEN_START_SYMBOL = "${";
    protected static final String TOKEN_END_SYMBOL = "}";
    protected static final String HTTP_DEFAULT_PORT_TOKEN = "${" + RuntimeConfigurationMBean.CONFIG_HTTP_DEFAULT_PORT + "}";
    protected static final String HTTPS_DEFAULT_PORT_TOKEN = "${" + RuntimeConfigurationMBean.CONFIG_HTTPS_DEFAULT_PORT + "}";
        
    protected Map mApplicationVariables;
    
    public HttpUrlResolverUtil() {
        this.mApplicationVariables = new HashMap();
    }
    
    public HttpUrlResolverUtil(Map appVariables) {
        this.mApplicationVariables = appVariables;
    }
    
    public static boolean isAToken(String name) throws Exception {
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
    
    public String resolveTokens(String url) throws Exception {
    	return resolveEmbeddedTokens(url);
    }
    
    public String resolveEmbeddedTokensInURL(String aString, Integer httpDefaultPort, Integer httpsDefaultPort) throws Exception {
        String newString = aString;
        Map unresolvedTokens = new HashMap();
        
        TokenData tokenData = findNextToken(newString, 0);
    	while (tokenData != null) {
    	    String tokenName = tokenData.getTokenName();
    	    String tokenValue = "";
    	    
            if (tokenName.equals(HTTP_DEFAULT_PORT_TOKEN)) {
                if (httpDefaultPort == null || 
                    (httpDefaultPort != null && httpDefaultPort.intValue() == -1)) {
                    unresolvedTokens.put(HTTP_DEFAULT_PORT_TOKEN, "");
                } else {
                    tokenValue = httpDefaultPort.toString();
                }
    	    } else if (tokenName.equals(HTTPS_DEFAULT_PORT_TOKEN)) {
                if (httpsDefaultPort == null || 
                    (httpDefaultPort != null && httpDefaultPort.intValue() == -1)) {
            	    unresolvedTokens.put(HTTPS_DEFAULT_PORT_TOKEN, "");
                } else {
                    tokenValue = httpsDefaultPort.toString();
                }
    	    } else {
    	    	String appVariableName = getApplicationVariableName(tokenName);
                String[] metadata = (String[]) mApplicationVariables.get(appVariableName);
                if (metadata == null || metadata[0] == null) {
                    unresolvedTokens.put(tokenName, "");
                    // update the application variable map as well
                    mApplicationVariables.put(appVariableName, new String[] { null, "STRING"} );
                } else {
                    tokenValue = metadata[0];
                }
            }
            
            // replace the token with actual value
            if (tokenValue != null) {
            	newString = newString.replace(tokenName, tokenValue);   
            }
            
            // update the start index to search for the next token
            tokenData.setEndIndex(tokenData.getStartIndex() + tokenValue.length());
            
            if (tokenData.getEndIndex() < newString.length()) {
                tokenData = findNextToken(newString, tokenData.getEndIndex());
            } else {
                break;
            }
    	}
    	
    	if (unresolvedTokens.size() > 0) {
    	    StringBuffer unresolvedTokenNameList = new StringBuffer();
    	    for (Iterator it = unresolvedTokens.keySet().iterator(); it.hasNext(); ) {
    	        unresolvedTokenNameList.append((String) it.next() + " ");
    	    }
    	    throw new Exception(mMessages.getString("HTTPBC-E00297.token_values_not_defined", unresolvedTokenNameList.toString()));
    	}
        return newString;
    }
    
    public String resolveEmbeddedTokens(String aString) throws Exception {
        String newString = aString;
        
        TokenData tokenData = findNextToken(newString, 0);
    	while (tokenData != null) {
    	    String tokenName = tokenData.getTokenName();
    	    String tokenValue = null;
            String[] metadata = (String[]) mApplicationVariables.get(getApplicationVariableName(tokenName));
            
            if (metadata == null || metadata[0] == null) {
            	throw new Exception(mMessages.getString("HTTPBC-E00252.Application_variable_not_defined", tokenName));
            }
            
            tokenValue = metadata[0];
            
            // replace the token with actual value
            newString = newString.replace(tokenName, tokenValue);    
            // update the start index to search for the next token
            tokenData.setEndIndex(tokenData.getStartIndex() + tokenValue.length());
            
            if (tokenData.getEndIndex() < newString.length()) {
                tokenData = findNextToken(newString, tokenData.getEndIndex());
            } else {
                break;
            }
    	}
        return newString;
    }
    
    public String resolveHostNameInUrl(String aLocation) throws Exception {
    	String newLocation = aLocation;
        if (aLocation.toLowerCase().indexOf("localhost") > 0) {
            int localhostBeginIndex = newLocation.toLowerCase().indexOf("localhost");
            int localHostEndIndex = localhostBeginIndex + "localhost".length();
            newLocation = new StringBuffer(newLocation).replace(localhostBeginIndex, 
                                                                localHostEndIndex, 
                                                                InetAddress.getLocalHost().getCanonicalHostName()).toString();   
        }
        
        return newLocation;
    }
    
    public void validateLocationURI(String aLocation) throws Exception {
        URI uri = new URI(aLocation);
        String scheme = uri.getScheme();
        if (scheme == null || 
            (!scheme.equalsIgnoreCase("http") && !scheme.equalsIgnoreCase("https"))) { // NOI18N
            throw new Exception(mMessages.getString("HTTPBC-E00292.Protocol_unsupported",
                    new Object[] {scheme, aLocation}));
        }
        uri.toURL();
    }
    
    public void validateHttpUrl(String aUrlLocation) throws Exception {
    	TokenData token = findNextToken(aUrlLocation, 0);
        if (token == null) { // no tokens in the URL 
            try {
                URL url = new URL(aUrlLocation);
            } catch (MalformedURLException e) {
                throw new Exception (mMessages.getString("HTTPBC-E01231.Invalid_http_url", aUrlLocation));   
            }
        } else {
            if (token.getTokenName().length() == aUrlLocation.length()) {  // whole string is a token
                return;
            } else {
                // more complex validations when multiple tokens are present
                // make sure that protocol is valid. Tokens are not allowed for this.
                if (!aUrlLocation.toLowerCase().startsWith("http://") &&
                    !aUrlLocation.toLowerCase().startsWith("https://")) {
                    throw new Exception (mMessages.getString("HTTPBC-E01231.Invalid_http_url", aUrlLocation));
                }
                
                boolean isSecure = (aUrlLocation.toLowerCase().startsWith("https://")) ? true: false;
                int pathDelim = (isSecure)? aUrlLocation.indexOf("/", 8) : aUrlLocation.indexOf("/", 7);
                
                if (pathDelim > 0) {    // there is a context separator
                    // make sure that host and port are there and valid
                    String hostPort = (isSecure)? aUrlLocation.substring(8, pathDelim): aUrlLocation.substring(7, pathDelim);
                    validateHostPortInUrl(aUrlLocation, hostPort, isSecure);
                    
                    // context is pretty free-formed. We are not validating it here - a malformed URL will be caught at deployment time
                } else {
                    // make sure that host and port are there and valid
                    String hostPort = (isSecure)? aUrlLocation.substring(8): aUrlLocation.substring(7);
                    validateHostPortInUrl(aUrlLocation, hostPort, isSecure);
                }
            }
        }
        
    }
    
    public void validateHostPortInUrl(String aUrlLocation, String hostPort, boolean isSecure) throws Exception {
    	int colonIndex = hostPort.indexOf(":");
        // make sure the host port separator is there
        if (colonIndex <= 0) {
            throw new Exception(mMessages.getString("HTTPBC-E01231.Invalid_http_url", aUrlLocation));
        }
        
        TokenData token = findNextToken(hostPort, 0);
        if (token != null ) {
            if ( token.getTokenName().length() != hostPort.length()) {  // either the host or the port, or both are tokens
                if (hostPort.startsWith("$")) { // hostname should be a token
                    // make sure the host port separator is immediately following the end token tag
                    if (colonIndex != token.getEndIndex()) {
                        throw new Exception(mMessages.getString("HTTPBC-E01231.Invalid_http_url", aUrlLocation));
                    }
                    
                    // let's see if port is a token too
                    String port = hostPort.substring(colonIndex + 1);
                    if (isAToken(port)) {
                        // make sure the token syntax is correct and it matches the http protocol
                        validateHttpPortTokens(port, isSecure);
                    } 
                } else {   // port name is the token 
                    // make sure the host port separator is right before the start token tag
                    if (colonIndex != token.getStartIndex() -1) {
                        throw new Exception(mMessages.getString("HTTPBC-E01231.Invalid_http_url", aUrlLocation));
                    }
                    
                    // make sure the token syntax is correct and it matches the http protocol
                    String port = token.getTokenName();
                    validateHttpPortTokens(port, isSecure);
                } 
            } else {
                throw new Exception(mMessages.getString("HTTPBC-E01234.Invalid_host_port_with_tokens", aUrlLocation));
            }
        }
    }
    
    public void validateHttpPortTokens(String httpPortToken, boolean isSecure) throws Exception {
    	// only the default token values are allowed
        if (!httpPortToken.equals(HTTP_DEFAULT_PORT_TOKEN) &&
            !httpPortToken.equals(HTTPS_DEFAULT_PORT_TOKEN)) {
            throw new Exception(mMessages.getString("HTTPBC-E01232.Invalid_http_port_tokens", httpPortToken));
        }
        
        // make sure the token matches the URL protocol
        if (isSecure && !httpPortToken.equals(HTTPS_DEFAULT_PORT_TOKEN)) {
            throw new Exception(mMessages.getString("HTTPBC-E01233.Address_protocol_mismatch", new Object[] {httpPortToken, "https://"} ));
        }
        if (!isSecure && !httpPortToken.equals(HTTP_DEFAULT_PORT_TOKEN)) {
            throw new Exception(mMessages.getString("HTTPBC-E01233.Address_protocol_mismatch", new Object[] {httpPortToken, "http://"} ));
        }
    }
    
    public TokenData findNextToken(String aString, int aStartIndex) {
    	TokenData retVal = null;
    	int start = aString.indexOf(TOKEN_START_SYMBOL, aStartIndex);
    	int end = aString.indexOf(TOKEN_END_SYMBOL, aStartIndex);
    	if ( (start >= 0 && end > 0) && (start < end)) {
            retVal = new TokenData(start, end + 1, aString.substring(start, end + 1));
        } 
        
        return retVal;
    }
    
    public String getApplicationVariableName(String aToken) throws Exception {
        String tokenName = null;
        
        if (aToken == null || "".equals(aToken)) {
            throw new Exception(mMessages.getString("HTTPBC-E00253.Token_name_invalid", aToken));
        }
            
        tokenName = aToken.substring(2, aToken.length() - 1);
        if ("".equals(tokenName)) {
            throw new Exception(mMessages.getString("HTTPBC-E00253.Token_name_invalid", aToken));
        } 
        
        return tokenName;
        
    }
    
    public class TokenData {
        private final String tokenName;
        
    	private int startIndex;
        private int endIndex;
        
        public TokenData(int startIndex, int aIndex, String aName) {
            this.startIndex = startIndex;
            this.endIndex = aIndex;
            this.tokenName = aName;
        }
        
        public int getStartIndex() {
            return this.startIndex;
        }
        
        public int getEndIndex() {
            return this.endIndex;
        }
        
        public String getTokenName() {
            return this.tokenName;
        }
        
        public void setStartIndex(int index) {
            this.startIndex = index;
        }
        
        public void setEndIndex(int index) {
            this.endIndex = index;
        }
    }
}