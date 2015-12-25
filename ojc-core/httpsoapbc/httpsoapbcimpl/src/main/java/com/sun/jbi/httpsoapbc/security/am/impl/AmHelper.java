/**
 * 
 */
package com.sun.jbi.httpsoapbc.security.am.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.security.Principal;
import java.util.Enumeration;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.Subject;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidationException;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfigConstants;
import com.sun.jbi.httpsoapbc.security.impl.AuthInfo;
import com.sun.jbi.httpsoapbc.security.impl.UserPrincipal;
import com.sun.jbi.httpsoapbc.util.AlertsUtil;
import com.sun.jbi.internationalization.Messages;

/**
 * @author Sujit Biswas
 * 
 */
public class AmHelper {

    private AuthInfo authorizationInfo;

    private boolean authorizationEnabled;

    private AMPolicyEvaluator amPolicyEvaluator;

    private RuntimeConfigurationMBean rtc;

    private static boolean isInitialized = false;

    private static final Messages mMessages = Messages.getMessages(AmHelper.class);

    private static final Logger mLog = Messages.getLogger(AmHelper.class);

    /**
     * @param authorizationInfo
     * @param authorizationEnabled
     */
    public AmHelper(RuntimeConfigurationMBean rtc, AuthInfo authorizationInfo, boolean authorizationEnabled) {
	super();
	this.rtc = rtc;
	this.authorizationInfo = authorizationInfo;
	this.authorizationEnabled = authorizationEnabled;
    }

  
    public Subject validateCredentials(String id, char[] password) throws CredentialValidationException {

	// Need to do lazy loading of am config file since runtime configuration
	// does not have am config dir set when bc starts up. When an SU is
	// deployed,
	// then initialized (by any endpoint).
	if (!isInitialized) {
	    String amconfigFileDir = rtc.getAMConfigDirectory();
	    if (amconfigFileDir == null || amconfigFileDir.length() == 0) {
		throw new CredentialValidationException(mMessages.getString("HTTPBC-E01029.Invalid_access_manager_config_dir_undefined"));
	    }

	    initializeAM(amconfigFileDir);
	    isInitialized = true;
	}else{
	    try {
		initializeProperties(amProperties);
	    } catch (Exception e) {
		throw new CredentialValidationException(mMessages.getString("HTTPBC-E01029.Invalid_access_manager_config_dir_undefined"));
	    }
	}

	// If the subject ID is a Distinguished Name (DN), parse the DN
	// for domain component(s), and use those domain components over
	// the default one that is assumed initially
	// See: constructor
	// See:
	// EndpointSecurityConfigConstants.AM_AUTHCONTEXT_ORG_NAME_DEFAULT_VALUE)
	// See: http://en.wikipedia.org/wiki/LDIF
	

	Object authCtx = null;
	javax.security.auth.Subject subject = null;

	try {
	    String aOrgName = getOrgName();
	    authCtx = login(aOrgName);
	} catch (Exception le) {
	    throw new CredentialValidationException(le.getMessage(), le);
	}

	Callback[] callbacks = null;
	while (authCtxHasMoreRequirements(authCtx)) {
	    callbacks = authCtxGetRequirements(authCtx);

	    if (callbacks != null) {
		for (int i = 0; i < callbacks.length; i++) {
		    if (callbacks[i] instanceof NameCallback) {
			((NameCallback) callbacks[i]).setName(id);
		    } else if (callbacks[i] instanceof PasswordCallback) {
			((PasswordCallback) callbacks[i]).setPassword(password);
		    } else {
			throw new CredentialValidationException(mMessages.getString("HTTPBC-E01030.Unsupported_access_manager_callback"));
		    }
		}
	    }

	    authCtxSubmitRequirements(authCtx, callbacks);
	}

	if (authCtxGetStatusIsSuccess(authCtx)) {
	    boolean isAuthorized = false;
	    Principal p = null;
	    try {
		Object ssoToken = authCtxGetSSOToken(authCtx);
		subject = new Subject();
		p = getSSOTokenPrincipal(ssoToken);
		subject.getPrincipals().add(p);

		if (authorizationEnabled) {
		    amPolicyEvaluator = new AMPolicyEvaluator(authorizationInfo);
		    isAuthorized = amPolicyEvaluator.evaluate(ssoToken);
		}

	    } catch (Exception e) {
		throw new CredentialValidationException(e.getMessage());
	    }
	    if (authorizationEnabled && !isAuthorized) {
		throw new AMAuthorizationException(mMessages.getString("HTTPBC-E01031.Unauthorized_access_to_resource", new Object[] { authorizationInfo.getResource(), p }));
	    }

	} else {
	    throw new CredentialValidationException(authCtxGetErrorMessage(authCtx));
	}
	return subject;
    }

    private Principal getSSOTokenPrincipal(Object ssotoken) throws CredentialValidationException {

	UserPrincipal p = null;

	try {
	    Class cl = getClassloader().loadClass("com.iplanet.sso.SSOToken");

	    Method m = cl.getMethod("getPrincipal");

	    Object o = m.invoke(ssotoken);

	    // Class cl1 =
	    // getClassloader().loadClass("com.iplanet.sso.providers.dpro.SSOPrincipal");
	    // Method m1 = cl1.getMethod("getName");

	    // String s = (String) m1.invoke(o);

	    p = new UserPrincipal(((Principal) o).getName());

	} catch (Exception e) {
	    throw new CredentialValidationException(e.getMessage(),e);
	}

	return p;

    }

    private boolean authCtxGetStatusIsSuccess(Object authCtx) throws CredentialValidationException {

	try {
	    Class cl = getClassloader().loadClass(authCtx.getClass().getName());

	    Method m = cl.getMethod("getStatus");

	    Object o = m.invoke(authCtx);

	    Class cl1 = getClassloader().loadClass(o.getClass().getName());
	    Method m1 = cl1.getMethod("toString");

	    String s = (String) m1.invoke(o);

	    if (s.equalsIgnoreCase("success")) {
		return true;
	    }

	} catch (Exception e) {
	    throw new CredentialValidationException(e.getMessage(),e);
	}

	return false;

    }

    private boolean authCtxHasMoreRequirements(Object authCtx) throws CredentialValidationException {
	Boolean b = false;
	try {
	    Class cl = getClassloader().loadClass("com.sun.identity.authentication.AuthContext");

	    Method m = cl.getMethod("hasMoreRequirements");

	    b = (Boolean) m.invoke(authCtx);
	} catch (Exception e) {
	    throw new CredentialValidationException(e.getMessage(),e);
	}

	return b;
    }

    private Object authCtxGetSSOToken(Object authCtx) throws CredentialValidationException {
	Object b = null;
	try {
	    Class cl = getClassloader().loadClass("com.sun.identity.authentication.AuthContext");

	    Method m = cl.getMethod("getSSOToken");

	    b = m.invoke(authCtx);
	} catch (Exception e) {
	    throw new CredentialValidationException(e.getMessage(),e);
	}

	return b;
    }

    private String authCtxGetErrorMessage(Object authCtx) throws CredentialValidationException {
	String b = null;
	try {
	    Class cl = getClassloader().loadClass("com.sun.identity.authentication.AuthContext");

	    Method m = cl.getMethod("getErrorMessage");

	    b = (String) m.invoke(authCtx);
	} catch (Exception e) {
	    throw new CredentialValidationException(e.getMessage(),e);
	}

	return b;
    }

    private Callback[] authCtxGetRequirements(Object authCtx) throws CredentialValidationException {

	Callback[] b = null;
	try {
	    Class cl = getClassloader().loadClass("com.sun.identity.authentication.AuthContext");

	    Method m = cl.getMethod("getRequirements");

	    b = (Callback[]) m.invoke(authCtx);
	} catch (Exception e) {
	    throw new CredentialValidationException(e.getMessage(),e);
	}

	return b;

    }

    private void authCtxSubmitRequirements(Object authCtx, Callback[] callbacks) throws CredentialValidationException {
	try {
	    Class cl = getClassloader().loadClass("com.sun.identity.authentication.AuthContext");

	    Method m = cl.getMethod("submitRequirements", callbacks.getClass());

	    m.invoke(authCtx, (Object) callbacks);
	} catch (Exception e) {
	    throw new CredentialValidationException(e.getMessage(),e);
	}

    }

    /**
     * Starts the login process for the given <code>AuthContext</code> object *
     * 
     * @param aOrgName
     * @return
     * @throws Exception
     */
    private Object login(String aOrgName) throws Exception {
	Object authCtx;

	Class cl = getClassloader().loadClass("com.sun.identity.authentication.AuthContext");

	Constructor cons = cl.getConstructor(String.class);
	authCtx = cons.newInstance(aOrgName);

	// authCtx.login(AuthContext.IndexType.MODULE_INSTANCE,
	// authModulendexName);

	// note the above is bit more efficient, however if the
	// access-manger is running with different module instance name or
	// say the access manager is running in legacy mode where the module
	// name can be different from DataStore say for example LDAP the above
	// will fail or we need to make some of the properties configurable

	Method m = cl.getMethod("login");

	m.invoke(authCtx);

	return authCtx;
    }

    /**
     * read the AMConfig.properties file for "com.iplanet.am.domaincomponent" if
     * the property is not present use the default value of "/"
     * 
     * @return realm or Organization name
     */
    private String getOrgName() throws Exception {
	String s = null;
	s = getDomainComponentName();
	return s != null ? s : EndpointSecurityConfigConstants.AM_AUTHCONTEXT_ORG_NAME_DEFAULT_VALUE;
    }

    synchronized public static void resetInitializeFlag() {
	isInitialized = false;
    }

    static Properties amProperties = new Properties();
    synchronized public static void initializeAM(String amConfigDir) throws CredentialValidationException {
	String amconfigFile = new StringBuffer(amConfigDir).append(File.separatorChar).append(EndpointSecurityConfigConstants.AM_CLIENT_SDK_CONFIG_PROPS_FILE_NAME).toString();

	if (!(new File(amconfigFile)).exists()) {
	    String error = mMessages.getString("HTTPBC-E01041.Access_manager_config_props_file_not_found", new Object[] {
		    EndpointSecurityConfigConstants.AM_CLIENT_SDK_CONFIG_PROPS_FILE_NAME, amConfigDir });
	    mLog.log(Level.WARNING, error);
	    AlertsUtil.getAlerter().warning(error, HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, null, AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
		    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HTTPBC-E01041");
	    throw new CredentialValidationException(error);
	}

	resetAM();

	amProperties = new Properties();

	try {
	    FileInputStream fis = new FileInputStream(amconfigFile);
	    amProperties.load(fis);
	} catch (IOException e) {
	    String error = mMessages.getString("HTTPBC-E01028.Failed_load_access_manager_config_props", new Object[] { amConfigDir, e.getLocalizedMessage() });
	    mLog.log(Level.WARNING, error, e);
	    AlertsUtil.getAlerter().warning(error, HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, null, AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
		    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HTTPBC-E01028");
	    throw new CredentialValidationException(error, e);
	}

	initializeAMProperties();
    }


    private static void initializeAMProperties() throws CredentialValidationException {
	try {
	    initializeProperties(amProperties);

	    // Need to manually set it to false, since the false value from
	    // AMConfig.properties does not stick.
	    initializeProperties(EndpointSecurityConfigConstants.AM_CLIENT_SDK_CONFIG_SERVER_MODE_KEY, "false");
	} catch (Exception e) {
	    
	    String error = mMessages.getString("HTTPBC-E01028.Failed_load_access_manager_config_props", new Object[] { "amConfigDir", e.getLocalizedMessage() });
		   
	    throw new CredentialValidationException(error, e);
	}
    }

    private static void initializeProperties(Properties props) throws Exception {

	Class cl = getClassloader().loadClass("com.iplanet.am.util.SystemProperties");

	Method m = cl.getMethod("initializeProperties", Properties.class);

	m.invoke(null, props);

    }

    private static void initializeProperties(String name, String value) throws Exception {
	Class cl = getClassloader().loadClass("com.iplanet.am.util.SystemProperties");

	Method m = cl.getMethod("initializeProperties", String.class, String.class);

	m.invoke(null, name, value);
    }

    private static Properties getAllSystemProperties() throws Exception {

	Class cl = getClassloader().loadClass("com.iplanet.am.util.SystemProperties");

	Method m = cl.getMethod("getAll");

	return (Properties) m.invoke(null);

    }

    private String getDomainComponentName() throws Exception {

	Class cl = getClassloader().loadClass("com.iplanet.am.util.SystemProperties");

	Method m = cl.getMethod("get", String.class);

	return (String) m.invoke(null, "com.iplanet.am.domaincomponent");
    }

    @SuppressWarnings("unchecked")
    private static void resetAM() {
	try {
	    Properties existingProps = new Properties(getAllSystemProperties());
	    Enumeration propsEnum = existingProps.propertyNames();
	    while (propsEnum.hasMoreElements()) {
		String propName = (String) propsEnum.nextElement();

		initializeProperties(propName, "");

	    }
	} catch (Exception e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}
    }

    private static ClassLoader getClassloader() {
	return Thread.currentThread().getContextClassLoader();
    }

}
