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
package it.imolinfo.jbi4cics.messageformat.jdbc;

import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;

public class JdbcParameterDescriptor implements FieldDescriptor {
  
  public static final int IN_PARAMETER=0;
  public static final int OUT_PARAMETER=1;
  public static final int IN_OUT_PARAMETER=2;
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(JdbcParameterDescriptor.class);

  private int inOutType;
  private int sqlType;

 /**
  * 
  * @param type  The type
  */
  
  public JdbcParameterDescriptor(int type) {
	    this.inOutType = type;
	  }
  
  /**
   * @return Returns the sqlType.
   */
  public int getSqlType() {
    return sqlType;
  }

  /**
   * @param sqlType The sqlType to set.
   */
  public void setSqlType(int sqlType) {
    this.sqlType = sqlType;
  }

 
  /**
   * @return Returns the inOutType.
   */
  public int getInOutType() {
    return inOutType;
  }
  /**
   * @param type  The type to set.
   */
  public void setInOutType(int type) {
    this.inOutType = type;
  }

  /**
   * @throws FormatException    The format exception
   * @return Class The prefered java type
   */
  public Class getPreferredJavaType() throws FormatException {
    //TODO gestire questo metodo
    throw new FormatException(MESSAGES.getString("CIC001800_Not_implemented"));
  }
  
}
