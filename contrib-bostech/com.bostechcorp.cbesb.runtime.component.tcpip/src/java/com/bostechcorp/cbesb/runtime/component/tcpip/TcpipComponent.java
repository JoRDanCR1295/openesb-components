/*
 * ChainBuilder ESB Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * 
 * $Id: TcpipComponent.java,v 1.1.1.1 2007/04/09 17:49:30 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip;

import javax.jbi.servicedesc.ServiceEndpoint;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.servicemix.common.BaseComponent;
import org.apache.servicemix.common.BaseLifeCycle;
import org.apache.servicemix.common.BaseServiceUnitManager;
import org.apache.servicemix.common.Deployer;
import org.w3c.dom.DocumentFragment;

public class TcpipComponent extends BaseComponent {

	protected final transient Log logger = LogFactory.getLog(getClass());
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.apache.servicemix.common.BaseComponent#createLifeCycle()
	 */
	protected BaseLifeCycle createLifeCycle() {
		return new TcpipLifeCycle(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.apache.servicemix.common.BaseComponent#createServiceUnitManager()
	 */
	public BaseServiceUnitManager createServiceUnitManager() {
		Deployer[] deployers = new Deployer[] { new TcpipWsdl1Deployer(this) };
		return new BaseServiceUnitManager(this, deployers);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.jbi.component.Component#resolveEndpointReference(org.w3c.dom.DocumentFragment)
	 */
	public ServiceEndpoint resolveEndpointReference(DocumentFragment epr) {
		// TODO - implement endpoint references
		// return JmsResolvedEndpoint.resolveEndpoint(epr);
		return null;
	}
}
