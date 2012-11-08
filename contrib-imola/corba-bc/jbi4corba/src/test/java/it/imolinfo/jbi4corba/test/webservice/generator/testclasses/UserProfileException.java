 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator.testclasses;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;


@XmlAccessorType(XmlAccessType.FIELD)
public class UserProfileException extends Exception 
{    
  public String userCode = null;
  public String reason = null;  
  public UserProfile[] profiles = null;
  
  public UserProfileException ()
  {
    super();
  } // ctor
  
  
  public String getUserCode() {
      return userCode;
  }

  public void setUserCode(String userCode) {
      this.userCode = userCode;
  }
  
  
  public String getReason() {
      return reason;
  }

  /**
 * @param reason
 */
public void setReason(String reason) {
      this.reason = reason;
  }


public UserProfile[] getProfiles() {
    return profiles;
}

public void setProfiles(UserProfile[] profiles) {
    this.profiles = profiles;
}


    
} // class UserProfileException