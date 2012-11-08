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
package it.imolinfo.jbi4cics.locator;

/**
 * @author raffaele
 *
 */
public interface ServiceLocation {
	public final static int DUMMY=-1;
	public final static int CICS=0;
	public final static int IMS=1;
	public final static int JDBC=2;
	/*public final static int JMS=3;
	public final static int MQ=4;*/
	
  /**
   * Restituisce il nome della locazione:
   * nel caso di CICS, IMS, JDBC, e JMS rappresenta il nome jndi,
   * nel caso di JACADA rappresenta il nome del connection pool.
   * @return String    The location name
   */
	public String getLocationName();
	
	/**
	 * 
	 * @return   The connection type
	 */
	public int getConnectionType();
}
