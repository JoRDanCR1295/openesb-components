/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.messageformat;

import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.service.ServiceContext;

public class NoOpMessageFormatter implements MessageFormatter {

  public NoOpMessageFormatter() {
    super();
    // TODO Auto-generated constructor stub
  }

  public void mapInputBeanToInputMessage(ServiceContext serviceContext) throws FormatException {
    serviceContext.setInputMessage(serviceContext.getInputBean());
    
  }

  public void mapOutputMessageToOutputBean(ServiceContext serviceContext) throws FormatException {
    serviceContext.setOutputBean(serviceContext.getOutputMessage());
  }

}
