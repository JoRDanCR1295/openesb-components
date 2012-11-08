/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovidersimplemult;



import it.imolinfo.jbi4corba.test.testprovidersimplemult.Echo2POA;
import java.util.logging.Logger;

/**
 * This class is the corba servant used to manage the 'Echo.idl'
 */
public class EchoImpl2 extends Echo2POA {

  /**
   * logger.
   */
  private static Logger log = Logger.getLogger(EchoImpl2.class.getName());
  //LogFactory.getLog(EchoImpl.class);
  /**
   * main first argument is the port (host is supposed to be localhost) second argument is
   * daemon=true/false optional, default false.
   * If daemon is true the servan starts as daemon, useful for integration tests
   */
  

 
  // ==========================================
    //                  The operations in the IDL
    // ==========================================
  public String echo(String arg0) {
    log.info("echo(String) - message received: " + arg0);
    return arg0+"Interface2";
  }

    
}