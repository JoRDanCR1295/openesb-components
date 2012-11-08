/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.jbi;

import org.apache.servicemix.common.BaseComponent;
import org.apache.servicemix.common.BaseLifeCycle;
import org.apache.servicemix.common.BaseServiceUnitManager;
import org.apache.servicemix.common.Deployer;

public class Jbi4cicsComponent extends BaseComponent {
	
	/**
	 * void constructor.
	 */
	  public Jbi4cicsComponent(){
		  super();
	  }

  /* (non-Javadoc)
   * @see org.servicemix.common.BaseComponent#createLifeCycle()
   */
  protected BaseLifeCycle createLifeCycle() {
      return new Jbi4cicsLifeCycle(this);
  }

  /* (non-Javadoc)
   * @see org.servicemix.common.BaseComponent#createServiceUnitManager()
   */
  public BaseServiceUnitManager createServiceUnitManager() {
      Deployer[] deployers = new Deployer[] { new Jbi4cicsDeployer(this), new  Jbi4cicsWSDLDeployer(this) };
      return new BaseServiceUnitManager(this, deployers);
  }
}
