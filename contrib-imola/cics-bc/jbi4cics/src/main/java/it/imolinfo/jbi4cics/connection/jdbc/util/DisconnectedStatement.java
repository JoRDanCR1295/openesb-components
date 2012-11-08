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
package it.imolinfo.jbi4cics.connection.jdbc.util;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;


/**
 * @author raffaele
 *
 */
public interface DisconnectedStatement extends Statement{
  public void setConnection(Connection con);
  public void setConnected(boolean connected) throws SQLException;
  public boolean isConnected();
}
