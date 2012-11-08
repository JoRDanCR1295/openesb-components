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
package it.imolinfo.jbi4cics.connection.jca.cics;

import it.imolinfo.jbi4cics.connection.InteractionDescription;

/**
 * @author raffaele
 *
 */
public class CICSInteractionDescription implements InteractionDescription {

  private String programName;
  private int timeout;
  private boolean tpn;
  private String transactionName;
  
  /**
   * 
   */    
  public CICSInteractionDescription() {
    super();
    // TODO Auto-generated constructor stub
  }
  
  /**
   * @return Returns the transactionName.
   */
  public String getTransactionName() {
    return transactionName;
  }

  /**
   * @param transactionName The transactionName to set.
   */
  public void setTransactionName(String transactionName) {
    this.transactionName = transactionName;
  }

  /**
   * @return Returns the tpn.
   */
  public boolean isTpn() {
    return tpn;
  }

  /**
   * @param tpn The tpn to set.
   */
  public void setTpn(boolean tpn) {
    this.tpn = tpn;
  }

 

  /**
   * @return Returns the programName.
   */
  public String getProgramName() {
    return programName;
  }

  /**
   * @param programName The programName to set.
   */
  public void setProgramName(String programName) {
    this.programName = programName;
  }

  /**
   * @return Returns the timeout.
   */
  public int getTimeout() {
    return timeout;
  }

  /**
   * @param timeout The timeout to set.
   */
  public void setTimeout(int timeout) {
    this.timeout = timeout;
  }

}
