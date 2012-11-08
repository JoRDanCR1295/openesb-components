/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
/**
 * 
 */
package it.imolinfo.jbi4cics.messageformat;

import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaFormatter;
import it.imolinfo.jbi4cics.messageformat.jdbc.JdbcBeanMappingDescriptor;
import it.imolinfo.jbi4cics.messageformat.jdbc.JdbcFormatter;
import it.imolinfo.jbi4cics.messageformat.jdbc.JdbcStatementDescriptor;
import it.imolinfo.jbi4cics.service.ServiceContext;

/**
 * @author raffaele
 *
 */
public class MessageFormatterFactory {
  private static CommareaFormatter commareaFormatter=new CommareaFormatter();
  private static JdbcFormatter jdbcFormatter=new JdbcFormatter();
  private static ToStringMessageFormatter toStringMessageFormatter=new ToStringMessageFormatter();
  private static NoOpMessageFormatter noOpMessageFormatter=new NoOpMessageFormatter();
  
  /**
   * void constructor.
   */
    public MessageFormatterFactory(){
    }
  
	public static MessageFormatter createMessageFormatter(ServiceContext serviceContext, boolean input){
    MappingDescriptor mappingDescriptor=null;
    if (input) {
      mappingDescriptor=serviceContext.getInputMappingDescriptor();
    }
    else {
      mappingDescriptor=serviceContext.getOutputMappingDescriptor();
    }
    if (mappingDescriptor instanceof CommareaBeanMappingDescriptor) {
      return commareaFormatter;
    }
    if (mappingDescriptor instanceof JdbcBeanMappingDescriptor) {
      return jdbcFormatter;
    }
    if (mappingDescriptor instanceof JdbcStatementDescriptor) {
      return jdbcFormatter;
    }
    if (mappingDescriptor instanceof NoOpMappingDescriptor) {
      return noOpMessageFormatter;
    }    
		return toStringMessageFormatter;
	}
}
