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
 * $Id: CcslComponent.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */

package com.bostechcorp.cbesb.runtime.ccsl.base;

import java.io.IOException;
import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import com.bostechcorp.cbesb.common.version.Version;
import com.bostechcorp.cbesb.runtime.ccsl.lib.CcslConfig;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ExceptionUtil;


public class CcslComponent implements Component, ComponentLifeCycle {
	String realComponentClassName;
	CcslConfig config;
	Component realComponent;
	ComponentLifeCycle realComponentLifeCycle;
	ServiceUnitManager realSuManager;
	
	ServiceUnitManager ccslSuManager;
	ComponentContext realComponentContext;
	ComponentContext ccslComponentContext;
	DeliveryChannel realDeliveryChannel;
	DeliveryChannel ccslDeliveryChannel;
	Log log;
	
	public ComponentLifeCycle getLifeCycle() {
		return this;
	}

	public Document getServiceDescription(ServiceEndpoint arg0) {
		Document desc = realComponent.getServiceDescription(arg0);
//		try {
//			ByteArrayOutputStream baos = new ByteArrayOutputStream();
//			StreamResult result = new StreamResult(baos);
//	        TransformerFactory tf = TransformerFactory.newInstance();
//	        Transformer t = tf.newTransformer();
//	        t.setOutputProperty("indent", "yes");
//	        DOMSource ds = new DOMSource(desc.getFirstChild());
//	        t.transform(ds, result);
//	        log.debug("CCSL-SERVICEDESC="+baos.toString("utf-8")+"\n");
//		}
//		catch (Exception e) {
//			log.error("\n\n\nException printing content: "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n\n");
//		}
//
		return desc;
	}

	public ServiceUnitManager getServiceUnitManager() {
		if (ccslSuManager == null) {
			realSuManager = realComponent.getServiceUnitManager();
			ccslSuManager = new CcslSuManager(this, realSuManager);
		}
		return ccslSuManager;
	}

	public boolean isExchangeWithConsumerOkay(ServiceEndpoint arg0,
			MessageExchange arg1) {
		return realComponent.isExchangeWithConsumerOkay(arg0, arg1);
	}

	public boolean isExchangeWithProviderOkay(ServiceEndpoint arg0,
			MessageExchange arg1) {
		return realComponent.isExchangeWithProviderOkay(arg0, arg1);
	}

	public ServiceEndpoint resolveEndpointReference(DocumentFragment arg0) {
		return realComponent.resolveEndpointReference(arg0);
	}

	public ObjectName getExtensionMBeanName() {
		return realComponentLifeCycle.getExtensionMBeanName();
	}

	public void init(ComponentContext arg0) throws JBIException {
		// save the real component context
		realComponentContext = arg0;
		// get a logger for our use
		log = LogFactory.getLog(this.getClass());
		// load the configuration file
		config = new CcslConfig(log, realComponentContext.getInstallRoot());
		realComponentClassName = config.getComponentClassName();
		realDeliveryChannel = realComponentContext.getDeliveryChannel();
		ccslDeliveryChannel = new CcslDeliveryChannel(this, realDeliveryChannel, log);
		// create our adapter context
		ccslComponentContext = new CcslComponentContext(this, realComponentContext, realDeliveryChannel, log);
		log.debug("CCSL is initializing");

		//Print the CCSL version
		Version versInfo = Version.getInstance(this.getClass());
		if (versInfo != null)
		{
			log.info("ChainBuilder ESB CCSL version: " + versInfo.toString() + 
				" built on " + versInfo.getBuildTimestamp());
		}
		else
		{
			log.warn("Unable to load CCSL version.");
		}
					
		// load the real component
		log.debug("CCSL is loading component class "+realComponentClassName);
		try {
			Class comp = Class.forName(realComponentClassName, true, this.getClass().getClassLoader());
			realComponent = (Component)comp.newInstance();
			//Print version info for real component
			versInfo = Version.getInstance(comp);
			if (versInfo != null)
			{
				log.info(realComponentClassName + " Component version:" + versInfo.toString() + 
						" built on " + versInfo.getBuildTimestamp());
			}
			realComponentLifeCycle = realComponent.getLifeCycle();
			realComponentLifeCycle.init(ccslComponentContext);
		}
		catch (ClassNotFoundException e) {
			log.error("\n\n\nCCSL error loading class: "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n\n");
			e.printStackTrace();
		}
		catch (Exception e) {
			log.error("\n\n\nCCSL component class "+realComponentClassName+" loaded but failed to instantiate");
			log.error("     exception="+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n\n");
		}
	}

	public void shutDown() throws JBIException {
		realComponentLifeCycle.shutDown();

	}

	public void start() throws JBIException {
		realComponentLifeCycle.start();

	}

	public void stop() throws JBIException {
		realComponentLifeCycle.stop();

	}

	public CcslConfig getConfig() {
		return config;
	}
}
