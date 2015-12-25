/**
 * 
 */
package com.sun.jbi.httpsoapbc.security.am.impl;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import com.sun.jbi.httpsoapbc.security.impl.AuthInfo;

/**
 * @author Sujit Biswas
 * 
 */
public class AMPolicyEvaluator {

    private AuthInfo authInfo;
    private Object pEvaluator;;

    /**
     * @param ssoToken
     * @param authInfo
     * @throws PolicyException
     * @throws SSOException
     * @throws NameNotFoundException
     */
    public AMPolicyEvaluator(AuthInfo authInfo) throws Exception {

	this.authInfo = authInfo;
	
	
	Class c = getClassloader().loadClass("com.sun.identity.policy.client.PolicyEvaluator");
	
	Constructor cons =  c.getConstructor(String.class);
	
	pEvaluator = cons.newInstance("iPlanetAMWebAgentService");

    }

    public boolean evaluate(Object ssoToken) throws Exception {
	Class c = getClassloader().loadClass("com.sun.identity.policy.client.PolicyEvaluator");
	
	Class ssoC = getClassloader().loadClass("com.iplanet.sso.SSOToken");
	
	Method m = c.getMethod("isAllowed", ssoC,String.class,String.class);
	
	
	Boolean b=  (Boolean)m.invoke(pEvaluator,ssoToken, authInfo.getResource(), authInfo.getAction());
	
	return b;
    }

    public AuthInfo getAuthInfo() {
	return authInfo;
    }

    public void setAuthInfo(AuthInfo authInfo) {
	this.authInfo = authInfo;
    }

    private static ClassLoader getClassloader() {
	return Thread.currentThread().getContextClassLoader();
    }

}
