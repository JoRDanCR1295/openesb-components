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
package it.imolinfo.jbi4cics.security;

/**
 * @author raffaele
 *
 */
public class J2CAccount implements Account {
  
  private String username;
  private String password;
  
  /**
   * 
   */
  public J2CAccount() {
    super();
    // TODO Auto-generated constructor stub
  }
  
  /**
   * @param password The password to set.
   */
  public void setPassword(String password) {
    this.password = password;
  }

  /**
   * @param username The username to set.
   */
  public void setUsername(String username) {
    this.username = username;
  }

  

  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.security.Account#getUserName()
   */
  public String getUsername() {
    // TODO Auto-generated method stub
    return username;
  }

  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.security.Account#getPassword()
   */
  public String getPassword() {
    // TODO Auto-generated method stub
    return password;
  }

}
