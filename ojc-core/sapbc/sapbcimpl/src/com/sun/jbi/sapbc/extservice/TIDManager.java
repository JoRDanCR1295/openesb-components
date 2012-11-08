/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)TIDManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.sapbc.extservice;

/**
 * An interface to define the methods required by a SAP tRFC TID manager for
 * inbound and outbound transaction from and to SAP, respectively.
 * @author Rajesh Dhingra
 * @version 
 */
public abstract interface TIDManager {
    
  /** SAP's tRFC TID length: 24
   */
  public static final int TID_SIZE = 24;
  
  /** IQ Event ID length.
   *  <Event GUID><Publisher GUID><Major Seq>-<Minor Seq>-<Priority>-<Enqueue Time>
   *  |----38----||-----38-------||----10---|1|---10----|1|---10---|1|----23------| = 132
   */
  public static final int EID_SIZE = 132;

  /**
   * Checks if the TID has been reserved (R) or committed (C).  It not, the TID
   * will be stored persistently and marked as reserved (R).
   * 
   * @param       tid       The incoming tRFC Transaction ID (TID).
   * @return      <code>true</code> if TID has not been reserved or committed;
   *              <code>false</code> otherwise.
   */
  public boolean onCheckTID(String tid);
  
  /**
   * Commits the TID into the persistent database.
   * 
   * @param       tid       The TID to commit.
   */
  public void onCommit(String tid);
  
  /**
   * Confirms the TID in the persistent database.  From SAP's standpoint, this
   * means the TID can be removed from the database, since it will never be sent
   * again by SAP.
   * 
   * @param       tid       The TID to confirm.
   */
  public void onConfirmTID(String tid);
  
  /**
   * Rollbacks the TID from the persistent database (marks it as unprocessed, U).
   * 
   * @param       tid       The TID to confirm.
   */
  public void onRollback(String tid);
  
  /**
   * Creates a TID (using SAP's method) and stores it persistently, marking it
   * as reserved (R).
   *
   * @param eid     The eid for which a tid will be created.
   * @return the TID created for the given eid
   */
  public String createTID(String eid);
  
  /**
   * Confirms the oldest TID in SAP's database.  SAP will then remove the TID from its
   * tracking since e*Gate is basically guaranteeing that the TID will never be
   * sent again. Always confirm the earliest tid
   *
   */
  public void confirmTID();
  
  /**
   * Sets the mbean object for alerts
   *
   * @param mbean     The mbean object.
  */
   // public void setMonitor(com.stc.connector.management.util.ObjectReference mbean);

  /**
   * Gets the mbean object for alerts
   *
   * @return the mbean object
   */
   // public com.stc.connector.management.util.ObjectReference getMonitor();
 }
