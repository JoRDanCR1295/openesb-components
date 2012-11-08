package com.gestalt.jbi.sip.component;

import javax.management.MBeanException;


public interface SIPConfigExtensionsMBean {
    public void updatePassword(String username, String currentPassword,
        String newPassword1, String newPassword2) throws Exception;
}
