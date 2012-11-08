/**
 *   uddi-binding-component - UDDI Binding Component
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
package com.gestalt.jbi.uddi.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.component.lifecycle.AbstractMessageExchangeProcessor;
import com.gestalt.jbi.uddi.component.uddi.UDDIConnectionManager;

import javax.jbi.JBIException;

import javax.management.ObjectName;


public class UDDIComponentLifeCycle extends AbstractComponentLifeCycle {
    private UDDIConnectionManager uddiConnectionManager;

    public UDDIComponentLifeCycle(AbstractComponent component) {
        super(component);
    }

    public UDDIConnectionManager getUDDIConnectionManager() {
        return uddiConnectionManager;
    }

    @Override
    public Object createMBean() {
        return null;
    }

    @Override
    public ObjectName createMBeanName() {
        return null;
    }

    @Override
    public void doInit() throws JBIException {
        this.uddiConnectionManager = new UDDIConnectionManager();
    }

    @Override
    public void doShutDown() throws JBIException {
    }

    @Override
    public void doStart() throws JBIException {
    }

    @Override
    public void doStop() throws JBIException {
    }

    @Override
    public AbstractMessageExchangeProcessor createMessageExchangeProcessor() {
        return new AbstractMessageExchangeProcessor(component);
    }
}
