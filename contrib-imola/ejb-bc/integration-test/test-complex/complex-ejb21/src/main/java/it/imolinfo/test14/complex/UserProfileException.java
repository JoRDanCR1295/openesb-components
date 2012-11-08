/*
 * UserProfileException.java
 *
 * Created on 21 May 2007, 15:07
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package it.imolinfo.test14.complex;

import java.rmi.RemoteException;

public class UserProfileException extends Exception
{
  public String code = null;
  public String reason = null;

  public UserProfileException () {} 

  public UserProfileException (String code, String reason) {
    this.code = code;
    this.reason =  reason;    
  }
  
  public String getCode() {
      return code;
  }
  
  public void setCode(String code) {
      this.code = code;
  }
  
  public String getReason() {
      return reason;
  }
  
  public void setReason(String reason) {
      this.reason = reason;
  }  
  
}
