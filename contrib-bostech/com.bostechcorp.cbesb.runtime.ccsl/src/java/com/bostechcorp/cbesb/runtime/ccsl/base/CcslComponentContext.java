/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: CcslComponentContext.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.base;

import java.util.MissingResourceException;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.naming.InitialContext;
import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;


public class CcslComponentContext implements ComponentContext {
	ComponentContext realComponentContext;
	DeliveryChannel realDeliveryChannel;
	DeliveryChannel ccslDeliveryChannel;
	Log log;
	
	CcslComponentContext(CcslComponent comp, ComponentContext cc, DeliveryChannel dc, Log logger) {
		realComponentContext = cc;
		realDeliveryChannel = dc;
		log = LogFactory.getLog(this.getClass());
		ccslDeliveryChannel = new CcslDeliveryChannel(comp, realDeliveryChannel, log);
	}

	public ServiceEndpoint activateEndpoint(QName arg0, String arg1)
			throws JBIException {
		return realComponentContext.activateEndpoint(arg0, arg1);
	}

	public void deactivateEndpoint(ServiceEndpoint arg0) throws JBIException {
		realComponentContext.deactivateEndpoint(arg0);
	}

	public void deregisterExternalEndpoint(ServiceEndpoint arg0)
			throws JBIException {
		realComponentContext.deregisterExternalEndpoint(arg0);
	}

	public String getComponentName() {
		return realComponentContext.getComponentName();
	}

	public DeliveryChannel getDeliveryChannel() throws MessagingException {
		return ccslDeliveryChannel;
	}

	public ServiceEndpoint getEndpoint(QName arg0, String arg1) {
		return realComponentContext.getEndpoint(arg0, arg1);
	}

	public Document getEndpointDescriptor(ServiceEndpoint arg0)
			throws JBIException {
		try {
			return realComponentContext.getEndpointDescriptor(arg0);
		} catch (NullPointerException e) {
			log.debug("****CCSL RETURNING NULL FOR LINKED ENDPOINT getEndpointDescriptor() to avoid exception****");
			return null;
		}
	}

	public ServiceEndpoint[] getEndpoints(QName arg0) {
		return realComponentContext.getEndpoints(arg0);
	}

	public ServiceEndpoint[] getEndpointsForService(QName arg0) {
		return realComponentContext.getEndpointsForService(arg0);
	}

	public ServiceEndpoint[] getExternalEndpoints(QName arg0) {
		return realComponentContext.getExternalEndpoints(arg0);
	}

	public ServiceEndpoint[] getExternalEndpointsForService(QName arg0) {
		return realComponentContext.getExternalEndpointsForService(arg0);
	}

	public String getInstallRoot() {
		return realComponentContext.getInstallRoot();
	}

	public java.util.logging.Logger getLogger(String arg0, String arg1)
			throws MissingResourceException, JBIException {
		return realComponentContext.getLogger(arg0, arg1);
	}

	public MBeanNames getMBeanNames() {
		return realComponentContext.getMBeanNames();
	}

	public MBeanServer getMBeanServer() {
		return realComponentContext.getMBeanServer();
	}

	public InitialContext getNamingContext() {
		return realComponentContext.getNamingContext();
	}

	public Object getTransactionManager() {
		return realComponentContext.getTransactionManager();
	}

	public String getWorkspaceRoot() {
		return realComponentContext.getWorkspaceRoot();
	}

	public void registerExternalEndpoint(ServiceEndpoint arg0)
			throws JBIException {
		realComponentContext.registerExternalEndpoint(arg0);
	}

	public ServiceEndpoint resolveEndpointReference(DocumentFragment arg0) {
		return realComponentContext.resolveEndpointReference(arg0);
	}

}
