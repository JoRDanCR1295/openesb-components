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
 * @(#)SunAccessManagerCredentialValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.am.impl;

import java.util.logging.Logger;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.security.auth.Subject;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfiguration;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidationException;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidator;
import com.sun.jbi.httpsoapbc.security.impl.AuthInfo;
import com.sun.jbi.internationalization.Messages;

public class SunAccessManagerCredentialValidator implements CredentialValidator {
    private static final Messages mMessages = Messages.getMessages(SunAccessManagerCredentialValidator.class);

    private static final Logger mLog = Messages.getLogger(SunAccessManagerCredentialValidator.class);

    private RuntimeConfigurationMBean rtc;

    private static boolean isRegistered = false;
    private AuthInfo authorizationInfo;

    private boolean authorizationEnabled;

    private AMPolicyEvaluator amPolicyEvaluator;

    // Inner class to handle configuration change notifications
    private static NotificationListener listener = new NotificationListener() {
	public void handleNotification(Notification notification, Object obj) {
	    if (notification instanceof AttributeChangeNotification) {
		AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
		String attrName = attrNotif.getAttributeName();
		if (attrName.equals(RuntimeConfiguration.CONFIG_ACCESS_MANAGER_CONFIG_DIR)) {
		    String newVal = (String) (attrNotif.getNewValue());
		    try {
			AmHelper.resetInitializeFlag();
		    } catch (Exception ex) {
			// Ignore intentionally
		    }
		}
	    }
	}
    };

    public SunAccessManagerCredentialValidator(RuntimeConfigurationMBean rtc, boolean authEnabled, AuthInfo authInfo) {
	this.rtc = rtc;
	// should be synchronized
	if (!isRegistered) {
	    ((RuntimeConfiguration) rtc).addNotificationListener(listener, null, null);
	    isRegistered = true;
	}
	this.authorizationEnabled = authEnabled;
	this.authorizationInfo = authInfo;
    }

    /*
     * @see com.sun.jbi.httpsoapbc.security.api.CredentialValidator#validateCredential(java.lang.String,
     *      java.lang.String)
     */

    private AmHelper amHelper = null;

    synchronized public Subject validateCredential(String id, char[] password) throws CredentialValidationException {
	if (amHelper == null) {
	    amHelper = new AmHelper(rtc, authorizationInfo, authorizationEnabled);
	}

	return amHelper.validateCredentials(id, password);
    }

}
