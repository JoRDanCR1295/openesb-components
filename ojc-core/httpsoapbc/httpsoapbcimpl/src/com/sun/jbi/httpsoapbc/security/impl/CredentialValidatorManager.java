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
 * @(#)CredentialValidatorManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.impl;

import com.sun.jbi.httpsoapbc.security.api.CredentialValidator;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidationException;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;

import com.sun.jbi.httpsoapbc.security.am.impl.SunAccessManagerCredentialValidator;
import com.sun.jbi.httpsoapbc.security.sc.impl.SunStringCompareCredentialValidator;
import com.sun.jbi.httpsoapbc.security.realm.impl.SunRealmCredentialValidator;

import com.sun.jbi.httpsoapbc.extensions.Policy;
import com.sun.jbi.httpsoapbc.extensions.BasicAuthenticationDetail.CredentialValidationType;
import com.sun.jbi.httpsoapbc.extensions.AccessManagerValidation;
import com.sun.jbi.httpsoapbc.extensions.PropertiesFileValidation;
import com.sun.jbi.httpsoapbc.extensions.RealmValidation;
import com.sun.jbi.httpsoapbc.extensions.StringCompareValidation;
import com.sun.jbi.httpsoapbc.extensions.ValidationBaseType;
import com.sun.jbi.httpsoapbc.security.sc.impl.PropertiesFileCredentialValidator;

import com.sun.jbi.internationalization.Messages;

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;

/**
 *
 * Manages various credential validators (i.e., AccessManager validator, Realm validator).
 */
public class CredentialValidatorManager {
    private static final Messages mMessages =
        Messages.getMessages(CredentialValidatorManager.class);

    private Map<String /*endpointName*/, SunStringCompareCredentialValidator> scValidators;
    private SunAccessManagerCredentialValidator amValidator;
    private Map<String /*realmName*/, RealmRefCount> realmValidators;
    private RuntimeConfigurationMBean rtc;
    private Map<String /*endpointName*/, PropertiesFileCredentialValidator> propertiesFileValidators;

    public CredentialValidatorManager (RuntimeConfigurationMBean rtc) {
        this.rtc = rtc;
        scValidators = Collections.synchronizedMap(new HashMap());
        realmValidators = Collections.synchronizedMap(new HashMap());
        propertiesFileValidators=Collections.synchronizedMap(new HashMap());
    }
    
    private class RealmRefCount {
        private int refcount = 0;
        private SunRealmCredentialValidator rv;
        RealmRefCount (SunRealmCredentialValidator rv) {
            this.rv = rv;
        }
        CredentialValidator acquireValidator() {
            refcount++;
            return rv;
        }
        void releaseValidator() {
            refcount--;
        }
        boolean canDestroy() {return refcount==0;}
    }
    
    public CredentialValidator acquireCredentialValidator(String uniqueEndpointName, Policy authPolicy, AuthInfo authInfo) throws CredentialValidationException {
		CredentialValidationType cvType = authPolicy.getBasicAuthenticationDetail().getCredentialValidationType();
		ValidationBaseType vbt = authPolicy.getBasicAuthenticationDetail().getCredentialValidation();
		CredentialValidator cv = null;
		
		switch (cvType) {
		case StringCompare:
			synchronized (scValidators) {
				if (!scValidators.containsKey(uniqueEndpointName)) {
					StringCompareValidation sv = (StringCompareValidation) vbt;
					SunStringCompareCredentialValidator scv = new SunStringCompareCredentialValidator(
																	uniqueEndpointName, 
																	sv.getUsername(), 
																	sv.getPassword());
					scValidators.put(uniqueEndpointName, scv);
				}
				cv = scValidators.get(uniqueEndpointName);
			}
			break;
                    
                      case PropertyFileAuthentication:
			synchronized (propertiesFileValidators) {
				if (!propertiesFileValidators.containsKey(uniqueEndpointName)) {
					PropertiesFileValidation pfv = (PropertiesFileValidation) vbt;
					PropertiesFileCredentialValidator pfcv = new PropertiesFileCredentialValidator(
											uniqueEndpointName, 
											pfv.getPropertiesFileLocation());
					propertiesFileValidators.put(uniqueEndpointName, pfcv);
				}
				cv = propertiesFileValidators.get(uniqueEndpointName);
			}
			break;
                    
                    
		case AM:
			// lazy instantiation needed to prevent no class def error
			// on bc startup if am sdk jars are not in the classpath
			AccessManagerValidation av = (AccessManagerValidation) vbt;
			String authorization = av.getAuthorization();

			try {
				boolean authEnabled = false;
				if (authorization.equals("true")) {
					authEnabled = true;
				}
				amValidator = new SunAccessManagerCredentialValidator(rtc, authEnabled, authInfo);
			} catch (NoClassDefFoundError ncdfe) {
				String err = mMessages.getString("HTTPBC-E01037.AM_credential_validator_create_failed_no_AM_client_library");
				throw new CredentialValidationException(err, ncdfe);
			} catch (Throwable t) {
				String err = mMessages.getString("HTTPBC-E01038.AM_credential_validator_create_failed_unexpected_runtime_error", new Object[] { t.getLocalizedMessage() });
				throw new CredentialValidationException(err, t);
			}

			cv = amValidator;
			break;
		case Realm:
			RealmValidation rv = (RealmValidation) vbt;
			String realm = rv.getRealmName();
			synchronized (realmValidators) {
				if (!realmValidators.containsKey(realm)) {
					try {
						SunRealmCredentialValidator rcv = new SunRealmCredentialValidator(realm);
						RealmRefCount rrc = new RealmRefCount(rcv);
						realmValidators.put(realm, rrc);
					} catch (NoClassDefFoundError ncdfe) {
						String err = mMessages.getString("HTTPBC-E01039.Realm_credential_validator_create_failed_no_Realm_client_library");
						throw new CredentialValidationException(err, ncdfe);
					} catch (Throwable t) {
						String err = mMessages.getString("HTTPBC-E01040.Realm_credential_validator_create_failed_unexpected_runtime_error",
								new Object[] { t.getLocalizedMessage() });
						throw new CredentialValidationException(err, t);
					}
				}
				cv = realmValidators.get(realm).acquireValidator();
			}
			break;
		}
		return cv;
	}
    
    public void releaseCredentialValidator(CredentialValidator cv) {
        if (cv instanceof SunStringCompareCredentialValidator) {
            SunStringCompareCredentialValidator scv = (SunStringCompareCredentialValidator)cv;
            synchronized (scValidators) {
                scValidators.remove(scv.getEndpointName());
            }
        } 
          else if (cv instanceof PropertiesFileCredentialValidator) {
            PropertiesFileCredentialValidator pfcv = (PropertiesFileCredentialValidator)cv;
            synchronized (propertiesFileValidators) {
                propertiesFileValidators.remove(pfcv.getEndpointName());
            }
        }
        
        else if (cv instanceof SunRealmCredentialValidator) {
            SunRealmCredentialValidator rv = (SunRealmCredentialValidator)cv;
            synchronized (realmValidators) {
                RealmRefCount rrc = realmValidators.get(rv.getRealmName());
                rrc.releaseValidator();
                if (rrc.canDestroy()) {
                    realmValidators.remove(rv.getRealmName());
                }
            }
        }
    }

    public boolean isAmPolicy(Policy secPolicy) {
	CredentialValidationType cvType = secPolicy.getBasicAuthenticationDetail().getCredentialValidationType();
	
	return cvType.equals(cvType.AM);
	
    }

    public RuntimeConfigurationMBean getRuntimeConfiguration() {
	// TODO Auto-generated method stub
	return rtc;
    }
}
