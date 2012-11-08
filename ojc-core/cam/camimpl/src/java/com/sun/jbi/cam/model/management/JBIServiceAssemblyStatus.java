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
 * @(#)JBIServiceAssemblyStatus.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.management;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author ylee
 * @author Graj
 *
 */
public class JBIServiceAssemblyStatus implements Serializable {

    /** status Deployed. */
    public static final String DEPLOYED_STATUS = "DEPLOYED";

    /** state  Loaded status.  */
    public static final String UNKNOWN_STATUS = "Unknown";
//    public static final String UNKNOWN_STATUS = "UNKNOWN";
    /** status Deployed. */
    public static final String SHUTDOWN_STATUS = "Shutdown";
//    public static final String SHUTDOWN_STATUS = "SHUTDOWN";
    /** Stopped status  */
    public static final String STOP_STATUS = "Stopped";
//    public static final String STOP_STATUS = "STOP";
    /** Started status */
    public static final String START_STATUS = "Started";
//    public static final String START_STATUS = "START";

    protected String serviceAssemblyName; // {01000000-31778EC7020100-0A124913-01}
    protected String serviceAssemblyDescription; // Represents this Assembly Unit
    protected String status; // DEPLOYED

    protected List<JBIServiceUnitStatus> jbiServiceUnitStatusList = new ArrayList<JBIServiceUnitStatus>();
    
    private Logger logger = Logger.getLogger(JBIServiceAssemblyStatus.class.getName()); 

    /**
     *
     */
    public JBIServiceAssemblyStatus() {
    }

    /**
     * @param serviceAssemblyName
     * @param serviceAssemblyDescription
     * @param status
     */
    public JBIServiceAssemblyStatus(String serviceAssemblyName,
            String serviceAssemblyDescription, String status) {
        super();
        this.serviceAssemblyName = serviceAssemblyName;
        this.serviceAssemblyDescription = serviceAssemblyDescription;
        this.status = status;
    }


    /**
     * @param serviceAssemblyName
     * @param serviceAssemblyDescription
     * @param status
     * @param jbiServiceUnitStatusList
     */
    public JBIServiceAssemblyStatus(String serviceAssemblyName,
            String serviceAssemblyDescription, String status,
            List<JBIServiceUnitStatus> jbiServiceUnitStatusList) {
        super();
        this.serviceAssemblyName = serviceAssemblyName;
        this.serviceAssemblyDescription = serviceAssemblyDescription;
        this.status = status;
        this.jbiServiceUnitStatusList = jbiServiceUnitStatusList;
    }
    
    /**
     * @return Returns the jbiServiceUnitList.
     */
    public List<JBIServiceUnitStatus> getJbiServiceUnitStatusList() {
        return this.jbiServiceUnitStatusList;
    }
    
    public void addJbiServiceUnitStatus(JBIServiceUnitStatus suStatus) {
        jbiServiceUnitStatusList.add(suStatus);
    }
    
    /**
     * @param jbiServiceUnitList The jbiServiceUnitList to set.
     */
    public void setJbiServiceUnitStatusList(List<JBIServiceUnitStatus> jbiServiceUnitStatusList) {
        this.jbiServiceUnitStatusList = jbiServiceUnitStatusList;
    }
    
    /**
     * @return Returns the serviceAssemblyDescription.
     */
    public String getServiceAssemblyDescription() {
        return this.serviceAssemblyDescription;
    }
    
    /**
     * @param serviceAssemblyDescription The serviceAssemblyDescription to set.
     */
    public void setServiceAssemblyDescription(String serviceAssemblyDescription) {
        this.serviceAssemblyDescription = serviceAssemblyDescription;
    }
    
    /**
     * @return Returns the serviceAssemblyName.
     */
    public String getServiceAssemblyName() {
        return this.serviceAssemblyName;
    }
    
    /**
     * @param serviceAssemblyName The serviceAssemblyName to set.
     */
    public void setServiceAssemblyName(String serviceAssemblyName) {
        this.serviceAssemblyName = serviceAssemblyName;
    }
    
    /**
     * @return Returns the status.
     */
    public String getStatus() {
        return this.status;
    }
    
    /**
     * @param status The status to set.
     */
    public void setStatus(String status) {
        this.status = status;
    }
    
    public void dump() {
        logger.info("/////////////////////////////////////////////////");
        logger.info("//  -- JBI Component --                        //");
        logger.info("/////////////////////////////////////////////////");

        logger.info("//  name is: "+ serviceAssemblyName);
        logger.info("//  description is: "+ serviceAssemblyDescription);
        logger.info("//  state is: "+ status);
        JBIServiceUnitStatus unitStatus = null;
        Iterator iterator = this.jbiServiceUnitStatusList.iterator();
        while(iterator.hasNext() == true) {
            unitStatus = (JBIServiceUnitStatus)iterator.next();
            if(unitStatus != null) {
                logger.info("// --------------------------------------");
                logger.info("// ------ Service Unit ------------------");
                logger.info("// --------------------------------------");
                logger.info("//    name is: "+ unitStatus.getServiceUnitName());
                logger.info("//    description is: "+ unitStatus.getServiceUnitDescription());
                logger.info("//    state is: "+ unitStatus.getStatus());
                logger.info("//    Target Name is: "+ unitStatus.getTargetName());
            }
        }
       logger.info("/////////////////////////////////////////////////");
    }

    public static void main(String[] args) {
    }
}
