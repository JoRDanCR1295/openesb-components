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

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.component.lifecycle.AbstractMessageExchangeProcessor;
import com.gestalt.jbi.sip.SIPConnectionManager;
import com.sun.jbi.internationalization.Messages;

import javax.jbi.JBIException;

import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * @author : csturtz
 */
public class SIPComponentLifeCycle extends AbstractComponentLifeCycle {
    private static final Logger log = Messages.getLogger(SIPComponentLifeCycle.class);
    private static Messages messages = Messages.getMessages(SIPComponentLifeCycle.class);
    private SIPConnectionManager sipConnectionManager;

    public SIPComponentLifeCycle(AbstractComponent abstractComponent) {
        super(abstractComponent);
    }

    public SIPConnectionManager getSIPConnectionManager() {
        return this.sipConnectionManager;
    }

    public void setSipConnectionManager(
        SIPConnectionManager sipConnectionManager) {
        this.sipConnectionManager = sipConnectionManager;
    }

    public Object createMBean() {
        SIPConfigExtensions mbean = null;

        try {
            if (log.isLoggable(Level.FINE)) {
                log.log(Level.FINE,"Creating MBean");
            }
            mbean = new SIPConfigExtensions(SIPConfigExtensionsMBean.class);
            mbean.init(componentContext.getWorkspaceRoot());
        } catch (NotCompliantMBeanException e) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00400.unableToCreateMbean"),e);
        }

        return mbean;
    }

    public ObjectName createMBeanName() {
        ObjectName on = null;

        try {
            if (log.isLoggable(Level.FINE)) {
                log.log(Level.FINE,"Creating MBean Name");
            }
            on = constructObjectName(componentContext.getComponentName());
        } catch (MalformedObjectNameException e) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00401.exceptionCreatingMBeanName"),e);
        }

        return on;
    }

    public AbstractMessageExchangeProcessor createMessageExchangeProcessor() {
        return new AbstractMessageExchangeProcessor(component);
    }

    public void doInit() throws JBIException {
        log.log(Level.INFO,messages.getString("SIPBC-R00402.initializing"));
        this.sipConnectionManager = SIPConnectionManager.getInstance();
    }

    public void doShutDown() throws JBIException {
        log.log(Level.INFO,messages.getString("SIPBC-R00403.shuttingDown"));
    }

    public void doStart() throws JBIException {
        log.log(Level.INFO,messages.getString("SIPBC-R00404.starting"));
    }

    public void doStop() throws JBIException {
        log.log(Level.INFO,messages.getString("SIPBC-R00405.stopping"));
    }

    public static ObjectName constructObjectName(String componentName)
        throws MalformedObjectNameException {
        String objectNameString = "com.sun.ebi:ServiceType=Configuration,InstallationType=bindingComponents,IdentificationName=" +
            componentName;

        return new ObjectName(objectNameString);
    }
}
