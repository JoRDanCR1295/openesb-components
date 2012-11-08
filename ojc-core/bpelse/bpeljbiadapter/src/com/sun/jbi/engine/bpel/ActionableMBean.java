/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.jbi.engine.bpel;



/**
 * @author Sun Inc
 * Dec 4, 2007
 */
public interface ActionableMBean {

    public String getActions();
    
    public void purgePersistenceData() throws Exception;
    
    public void purgeMonitorData() throws Exception;
}
