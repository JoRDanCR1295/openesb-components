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
 * @(#)AbstractValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.wsdl.extensions.ExtensibilityElement;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.httpsoapbc.util.HttpUrlResolverUtil;
import com.sun.jbi.httpsoapbc.util.HttpUrlResolverUtil.TokenData;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidationException;

public abstract class AbstractValidator implements Validator {
    private static final Messages mMessages =
        Messages.getMessages(AbstractValidator.class);
    
    protected static final String HTTP_DEFAULT_PORT_TOKEN = "${" + RuntimeConfigurationMBean.CONFIG_HTTP_DEFAULT_PORT + "}";
    protected static final String HTTPS_DEFAULT_PORT_TOKEN = "${" + RuntimeConfigurationMBean.CONFIG_HTTPS_DEFAULT_PORT + "}";
        
    protected RuntimeConfigurationMBean mRuntimeConfig;
    protected HttpUrlResolverUtil mUrlResolverUtil;
    protected Map mApplicationVariables;
    protected boolean mResolveTokens = false;
    
    // default constructor
    public AbstractValidator() {}
    
    public AbstractValidator(RuntimeConfigurationMBean runtimeConfig, boolean resolveTokens) {
        this.mRuntimeConfig = runtimeConfig;
        this.mApplicationVariables = runtimeConfig.retrieveApplicationVariablesMap();
        this.mUrlResolverUtil = new HttpUrlResolverUtil(mApplicationVariables);
        this.mResolveTokens = resolveTokens;
    }
    
    public abstract void validate(ExtensibilityElement element)
        throws ValidationException;
        
        
    protected boolean isAToken(String name) throws Exception {
    	return mUrlResolverUtil.isAToken(name);
    }
    
    protected String resolveTokens(String url) throws Exception {
    	return mUrlResolverUtil.resolveEmbeddedTokens(url);
    }
    
    protected String resolveEmbeddedTokens(String aString) throws Exception {
    	return mUrlResolverUtil.resolveEmbeddedTokens(aString);
    }
    
    protected String resolveEmbeddedTokensInURL(String aString) throws Exception {
        return mUrlResolverUtil.resolveEmbeddedTokensInURL(aString, mRuntimeConfig.getHttpDefaultPort(), mRuntimeConfig.getHttpsDefaultPort());
    }
    
    protected String resolveHostNameInUrl(String aLocation) throws Exception {
        return mUrlResolverUtil.resolveHostNameInUrl(aLocation);
    }
    
    protected void validateLocationURI(String aLocation) throws Exception {
        mUrlResolverUtil.validateLocationURI(aLocation);
    }
    
    protected void updateAppVariableMap(String aString, String type) throws Exception {
        TokenData tokenData = mUrlResolverUtil.findNextToken(aString, 0);
        
        if (tokenData == null) {   
            // not a token, nothing to do
            return;
        }
        
    	while (tokenData != null) {
    	    String tokenName = tokenData.getTokenName();
    	    String appVariableName = mUrlResolverUtil.getApplicationVariableName(tokenName);
    	    
            if (!tokenName.equals(HTTP_DEFAULT_PORT_TOKEN) &&
                !tokenName.equals(HTTPS_DEFAULT_PORT_TOKEN)&&
                !mApplicationVariables.containsKey(appVariableName)) {
                String[] metadata = new String[] {null, type};
                mApplicationVariables.put(appVariableName, metadata);
            } 
            
            if (tokenData.getEndIndex() < aString.length()) {
                tokenData = mUrlResolverUtil.findNextToken(aString, tokenData.getEndIndex());
            } else {
                break;
            }
    	}
    	
    }
    
}
