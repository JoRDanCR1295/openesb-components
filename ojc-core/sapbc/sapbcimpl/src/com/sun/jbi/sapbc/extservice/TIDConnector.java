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
 * @(#)TIDConnector.java 
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
 * An interface to define the methods required by a SAP BAPI and IDOC connectors
 * inbound and outbound transaction from and to SAP, respectively.
 * @author Rajesh Dhingra
 * @version 
 */
//Formally Connector
public abstract interface TIDConnector {
    
    
    /**
     * Retrieves the TID Database to use for client mode.  Interpretation subject
     * to TID Manager Class.
     * 
     * @return    The TID Database name.
     */
    public String getTidDatabase();

    /**
     * Checks whether tRFC (transactional RFC) is enabled.
     * 
     * @return    <code>true</code> if tRFC is enabled; else <code>false</code>.
     */
    public boolean isTrfcEnabled();

    /**
     * Retrieves the SAP jCO client object.
     *
     * @return    The jCO client handle.
     */
    //public com.sap.mw.jco.JCO.Client getClient();
    
    /**
     * Retrieves the SAPJCoClient client object.
     *
     * @return    The jCO client handle.
     */
    public SAPJCoClient getClient();

    /**
     * DOCUMENT ME!
     *
     * @param lastActivityTime DOCUMENT ME!
     */
    public void setLastActivityTime(long lastActivityTime);

    /**
     * Retrieves the Maximum number of Rows the TID Database for client mode
     * should have.
     * 
     * @return      Maximum Rows.
     */
    public int getClientMaxDBRows();
}
