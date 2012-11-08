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
package com.gestalt.jbi.sip;

import com.gestalt.jbi.sip.component.SIPConfigExtensions;
import com.gestalt.jbi.sip.component.SIPConfigExtensionsMBean;

import junit.framework.TestCase;

import javax.management.MBeanException;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;


public class SipConfigExtensionsTest extends TestCase {
    private final String user = "jthorn";
    private final String opass = "oldPassword";
    private final String npass = "newPassword";
    private final String[] sig = new String[] {
            "java.lang.String", "java.lang.String", "java.lang.String",
            "java.lang.String"
        };

    public void testUpdatePassword() throws Exception {
        MBeanServer mbs = MBeanServerFactory.createMBeanServer();
        ObjectName name = new ObjectName(
                "com.gestalt.jbi:IdentificationName=gestalt-sip-binding");

        SIPConfigExtensions mbean = new SIPConfigExtensions(SIPConfigExtensionsMBean.class);
        mbean.init("target/test-classes");
        mbean.getUsers().put(user, opass);

        mbs.registerMBean(mbean, name);

        try {
            mbs.invoke(name, "updatePassword",
                new String[] { "invalid user", opass, npass, npass }, sig);
            assertTrue("An invalid user exception should have been thrown",
                false);
        } catch (MBeanException e) {
        }

        try {
            mbs.invoke(name, "updatePassword",
                new String[] { user, "invalid password", npass, npass }, sig);
            assertTrue("An invalid password exception should have been thrown",
                false);
        } catch (MBeanException e) {
        }

        try {
            mbs.invoke(name, "updatePassword",
                new String[] { user, opass, npass, "diffent new password" }, sig);
            assertTrue("A passwords dont match exception should have been thrown",
                false);
        } catch (MBeanException e) {
        }

        try {
            mbs.invoke(name, "updatePassword",
                new String[] { user, opass, npass, npass }, sig);
        } catch (Exception e) {
            assertTrue("An exception should not have been thrown", false);
        }

        SIPConfigExtensions mbean2 = new SIPConfigExtensions(SIPConfigExtensionsMBean.class);

        mbean2.init("target/test-classes");
        assertTrue("User was not persisted", mbean.getUsers().containsKey(user));
        assertEquals("Password not same after persist",
            mbean.getUsers().get(user), npass);
    }
}
