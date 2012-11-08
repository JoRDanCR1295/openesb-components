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
 * $Id: CcslSuManager.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.base;

import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;


public class CcslSuManager implements ServiceUnitManager {
	ServiceUnitManager realSuManager;
	CcslComponent ccslComponent;
	
	CcslSuManager(CcslComponent comp, ServiceUnitManager realManager) {
		realSuManager = realManager;
		ccslComponent = comp;
	}

	public String deploy(String arg0, String arg1) throws DeploymentException {
		ccslComponent.config.loadSuSettings(arg0, arg1);
		return realSuManager.deploy(arg0, arg1);
	}

	public void init(String arg0, String arg1) throws DeploymentException {
		realSuManager.init(arg0, arg1);
	}

	public void shutDown(String arg0) throws DeploymentException {
		realSuManager.shutDown(arg0);
	}

	public void start(String arg0) throws DeploymentException {
		realSuManager.start(arg0);
	}

	public void stop(String arg0) throws DeploymentException {
		ccslComponent.config.stopSuUpocs(arg0);
		realSuManager.stop(arg0);
	}

	public String undeploy(String arg0, String arg1) throws DeploymentException {
		return realSuManager.undeploy(arg0, arg1);
	}

}
