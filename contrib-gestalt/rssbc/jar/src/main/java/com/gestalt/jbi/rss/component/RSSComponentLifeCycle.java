/**
 *   rss-binding-component - RSS Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
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
package com.gestalt.jbi.rss.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.component.lifecycle.AbstractMessageExchangeProcessor;
import com.gestalt.jbi.rss.component.rss.RSSManager;
import com.gestalt.jbi.rss.component.rss.persistence.ComponentType;
import com.gestalt.jbi.rss.component.rss.persistence.ConfigurationManager;
import com.gestalt.jbi.rss.component.rss.persistence.RSSConfigExtensions;

import javax.jbi.JBIException;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;


/**
 * The component life cycle for this component.
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSComponentLifeCycle extends AbstractComponentLifeCycle {
    private final String CONFIG_XML = "componentConfiguration.xml";
    private final String CONFIG_SCHEMA = "componentConfiguration.xsd";

    /**
     * Constructor
     *
     * @param abstractComponent
     */
    public RSSComponentLifeCycle(AbstractComponent abstractComponent) {
        super(abstractComponent);
    }

    /**
     * Creates a MBean
     *
     * @return Object - returns null at this time.
     */
    public Object createMBean() {
        return new RSSConfigExtensions(componentContext.getWorkspaceRoot());
    }

    /**
     * Creates a MBean Name
     *
     * @return ObjectName - returns null at this time.
     */
    public ObjectName createMBeanName() {
        ObjectName oname = null;

        try {
            oname = ConfigurationManager.constructObjectName(ComponentType.BC,
                    componentContext.getComponentName());
        } catch (MalformedObjectNameException e) {
            log.severe(e.getMessage());
        }

        return oname;
    }

    /**
     * Creates a message exchange processor
     *
     * @return AbstractMessageExchangeProcessor
     */
    public AbstractMessageExchangeProcessor createMessageExchangeProcessor() {
        return new AbstractMessageExchangeProcessor(component);
    }

    /**
     * Performs the initialization of this component.
     *
     * @throws JBIException
     */
    public void doInit() throws JBIException {
    }

    /**
     * Perfroms the shut down of this component.
     *
     * @throws JBIException
     */
    public void doShutDown() throws JBIException {
    }

    /**
     * perfroms the start of this component.
     *
     * @throws JBIException
     */
    public void doStart() throws JBIException {
        try {
            boolean persist = (Boolean) mBeanServer.getAttribute(mBeanName,
                    RSSConfigExtensions.PERSIST);

            RSSManager.setPersist(persist);
        } catch (Exception e) {
            log.warning("Exception getting mBean Attributes: " + e);
            e.printStackTrace();
        }
    }

    /**
     * perfroms the stop of this component.
     *
     * @throws JBIException
     */
    public void doStop() throws JBIException {
    }
}
