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

import it.imolinfo.jbi4cics.connection.jdbc.util.DisconnectedStatement;

import java.sql.ResultSet;

/**
 * @author raffaele
 *
 */
public class JdbcOutputMessage {
  private DisconnectedStatement disconnectedSatetement;
  private ResultSet resultSet;
  
  public JdbcOutputMessage() {
  }
  
  public JdbcOutputMessage(DisconnectedStatement disconnectedSatetement, ResultSet cacheRowSet) {
    this.disconnectedSatetement = disconnectedSatetement;
    this.resultSet = cacheRowSet;
  }
  /**
   * @return Returns the resultSet.
   */
  public ResultSet getResultSet() {
    return resultSet;
  }
  /**
   * @param resultSet The resultSet to set.
   */
  public void setResultSet(ResultSet cacheRowSet) {
    this.resultSet = cacheRowSet;
  }
  /**
   * @return Returns the disconnectedSatetement.
   */
  public DisconnectedStatement getDisconnectedSatetement() {
    return disconnectedSatetement;
  }
  /**
   * @param disconnectedSatetement The disconnectedSatetement to set.
   */
  public void setDisconnectedSatetement(DisconnectedStatement disconnectedSatetement) {
    this.disconnectedSatetement = disconnectedSatetement;
  }


}
