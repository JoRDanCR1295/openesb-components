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
package it.imolinfo.jbi4cics.exception;

/**
 *
 * @author <a href="mailto:rspazzoli@imolinfo.it">Raffaele Spazzoli</a>
 * @author <a href="mailto:acannone@imolinfo.it">Amedeo Cannone</a>
 */
public class ParseException extends Jbi4cicsException {

  /**
   * Serial Version UID.
   */
  private static final long serialVersionUID = 3758635048268496280L;

  /**
   * @param message The message
   */
  public ParseException(String message) {
    super(message);
    // TODO Auto-generated constructor stub
  }

  /**
   * @param message    The message
   * @param cause    The case
   */
  public ParseException(String message, Throwable cause) {
    super(message, cause);
    // TODO Auto-generated constructor stub
  }

  /**
   * @param cause    The case
   */
  public ParseException(Throwable cause) {
    super(cause);
    // TODO Auto-generated constructor stub
  }

    /**
     * A constructor with i18n support.
     *
     * @param   message  The message of the exception.
     * @param   args     The <code>MessageFormat</code> arguments.
     */
    public ParseException(String message, Object[] args) {
        super(message, args);
    }
    
    /**
     * A constructor with i18n support.
     *
     * @param   message  The message of the exception.
     * @param   args     The <code>MessageFormat</code> arguments.
     * @param   cause    The cause of the exception.
     */
    public ParseException(String message, Object[] args, Throwable cause) {
        super(message, args, cause);
    }

}
