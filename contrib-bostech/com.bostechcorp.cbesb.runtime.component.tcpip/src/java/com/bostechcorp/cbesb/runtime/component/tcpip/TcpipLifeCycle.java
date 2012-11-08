/*
 * ChainBuilder ESB
 * 		Visual Enterprise Integration
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
 * $Id: TcpipLifeCycle.java,v 1.1.1.1 2007/04/09 17:49:30 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.apache.servicemix.common.BaseComponent;
import org.apache.servicemix.common.BaseLifeCycle;
import org.apache.servicemix.common.Endpoint;


public class TcpipLifeCycle extends BaseLifeCycle {

	public TcpipLifeCycle(BaseComponent component) {
		super(component);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.apache.servicemix.common.BaseComponentLifeCycle#getExtensionMBean()
	 */
	protected Object getExtensionMBean() throws Exception {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.apache.servicemix.common.BaseLifeCycle#doInit()
	 */
	protected void doInit() throws Exception {
		
		super.doInit();
	}

	protected QName getEPRServiceName() {
		return null;
	}

	protected Endpoint getResolvedEPR(ServiceEndpoint ep) throws Exception {
		return null;
	}
}
