/**
 *   sip-binding-component - SIP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.sip.component;

import com.gestalt.jbi.sip.component.security.CryptoSecurityUtils;
import com.gestalt.jbi.sip.component.security.IOUtils;
import com.gestalt.jbi.sip.component.security.SecurityUtils;
import com.sun.jbi.internationalization.Messages;

import java.io.File;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.management.*;


public class SIPConfigExtensions extends StandardMBean
    implements SIPConfigExtensionsMBean {
    public static final String CONFIG_FILE_NAME = "config.file";
    private static final String PASSWORD_FILE_NAME = "SipBc.passwd";
    private static Map<String, String> users = new HashMap<String, String>();
    private File persist;
    private Logger log = Messages.getLogger(SIPConfigExtensions.class);
    private Messages messages = Messages.getMessages(SIPConfigExtensions.class);
    SecurityUtils securityTools;

    public SIPConfigExtensions(Object implementation, Class mbeanInterface)
        throws NotCompliantMBeanException {
        super(implementation, mbeanInterface);
    }

    public SIPConfigExtensions(Class mbeanInterface)
        throws NotCompliantMBeanException {
        super(mbeanInterface);
    }

    public void init(String workspaceRoot) {
        log.log(Level.INFO,messages.getString("SIPBC-R00406.initializingConfigurationExtensions"));
        try {
            File file = new File(workspaceRoot, CONFIG_FILE_NAME);

            if (file.exists()) {
                Map<String, String> mapconfig = (Map) IOUtils.loadObject(file);
                securityTools = new CryptoSecurityUtils(mapconfig.get(
                            "EncryptMethod"));
            } else {
                securityTools = new CryptoSecurityUtils();
            }
        } catch (Exception e) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00407.exceptionDuringInitialization"),e);
        }

        persist = new File(workspaceRoot, PASSWORD_FILE_NAME);

        if (persist.exists()) {
            try {
                users = decryptPasswords((Map) IOUtils.loadObject(persist));
            } catch (Exception e) {
                log.log(Level.WARNING,messages.getString("SIPBC-W00409.unableToDecryptPasswords"),e);
            }
        }
    }

    public void updatePassword(String username, String currentPassword,
        String newPassword1, String newPassword2) throws Exception {
        if (!users.containsKey(username)) {

            log.log(Level.WARNING,messages.getString("SIPBC-W00410.attemptedToUpdatePasswordForAnInvalidUser"));

            throw new MBeanException(new Exception("No such user"));
        }

        if (!users.get(username).equals(currentPassword)) {

            log.log(Level.WARNING,messages.getString("SIPBC-W00411.attemptedToUpdateThePasswordForTheUserButAnInvalidPasswordWasSpecified"));

            throw new MBeanException(new Exception("Invalid password"));
        }

        if (!newPassword1.equals(newPassword2)) {

            log.log(Level.WARNING,messages.getString("SIPBC-W00412.attemptedToUpdateThePasswordForTheUserButTheNewPasswordsDoNotMatch"));

            throw new MBeanException(new Exception("New passwords do not match"));
        }

        users.put(username, newPassword1);

        try {
            IOUtils.storeObject(encryptPasswords(users), persist);
        } catch (Exception e) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00413.passwordIsNotPersistingUnableToEncryptPasswords"),e);
        }
            log.log(Level.INFO,messages.getString("SIPBC-R00414.passwordUpdatedForTheUser", username));
    }

    protected String getParameterName(MBeanOperationInfo op,
        MBeanParameterInfo param, int sequence) {
        if (op.getName().equals("updatePassword")) {
            switch (sequence) {
            case 0:
                return "User Name";

            case 1:
                return "Old Password";

            case 2:
                return "New Password";

            case 3:
                return "Confirm Password";

            default:
                return super.getParameterName(op, param, sequence);
            }
        } else {
            return super.getParameterName(op, param, sequence);
        }
    }

    private Map encryptPasswords(Map<String, String> map)
        throws Exception {
        Map<String, String> encrypted = new HashMap<String, String>();

        for (String name : map.keySet()) {
            encrypted.put(name, securityTools.encrypt(users.get(name)));
        }

        return encrypted;
    }

    private Map decryptPasswords(Map<String, String> map)
        throws Exception {
        Map<String, String> decrypted = new HashMap<String, String>();

        for (String name : map.keySet()) {
            decrypted.put(name, securityTools.decrypt(map.get(name)));
        }

        return decrypted;
    }

    public static Map<String, String> getUsers() {
        return users;
    }
}
