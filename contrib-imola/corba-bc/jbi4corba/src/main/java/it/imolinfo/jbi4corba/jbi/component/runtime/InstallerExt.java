 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component.runtime;


import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.Logger;

/**
 * Installer Extension MBean, allow configuration to be changed before installation
 */
public class InstallerExt implements InstallerExtMBean {

    String mOutboundThreads;    
    
 
    private static final Logger LOG = LoggerFactory
            .getLogger(InstallerExt.class);
        
    /** Creates a new instance of InstallerExt */
    public InstallerExt() {
    }

    public String getOutboundThreads() {
        LOG.info("CRB000440_Number_of_outbound_threads",new Object[] {mOutboundThreads});
        
        return mOutboundThreads;
    }
    public void setOutboundThreads(String val) {
        LOG.info("CRB000441_Setting_number_of_outbound_threads_to",new Object[] {val});
        
        mOutboundThreads = val;
    }
    
    
    
}

