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
 * @(#)JBIServiceAssemblyStatusDocument.java 
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
public class JBIServiceAssemblyStatusDocument implements Serializable {

    public static final String SERVICE_ASSEMBLY_INFO_LIST_NODE_NAME = "service-assembly-info-list";
    public static final String SERVICE_ASSEMBLY_INFO_NODE_NAME = "service-assembly-info";
    public static final String NAME_NODE_NAME = "name";
    public static final String DESCRIPTION_NODE_NAME = "description";
    public static final String STATUS_NODE_NAME = "status";
    public static final String SERVICE_UNIT_INFO_LIST_NODE_NAME = "service-unit-info-list";
    public static final String SERVICE_UNIT_INFO_NODE_NAME = "service-unit-info";
    public static final String TARGET_NAME_NODE_NAME = "target-name";

    protected List<JBIServiceAssemblyStatus> jbiServiceAssemblyStatusList = new ArrayList<JBIServiceAssemblyStatus>();
    
    private Logger logger = Logger.getLogger(JBIServiceAssemblyStatusDocument.class.getName());

    /**
     *
     */
    public JBIServiceAssemblyStatusDocument() {
    }


    /**
     * @return Returns the jbiServiceAssemblyList.
     */
    public List<JBIServiceAssemblyStatus> getJbiServiceAssemblyStatusList() {
        return this.jbiServiceAssemblyStatusList;
    }

    /**
     * @param jbiServiceAssemblyList The jbiServiceAssemblyList to set.
     */
    public void setJbiServiceAssemblyStatusList(List<JBIServiceAssemblyStatus> jbiServiceAssemblyStatusList) {
        this.jbiServiceAssemblyStatusList = jbiServiceAssemblyStatusList;
    }

    
    public void addJbiServiceAssemblyStatus(JBIServiceAssemblyStatus serviceAssemblyStatus) {
        jbiServiceAssemblyStatusList.add(serviceAssemblyStatus);
    }

    public void addJbiServiceAssemblyStatusList(List<JBIServiceAssemblyStatus> serviceAssemblyStatus) {
        jbiServiceAssemblyStatusList.addAll(serviceAssemblyStatus);
    }
    

    public void dump() {
        Iterator iterator = this.jbiServiceAssemblyStatusList.iterator();
        JBIServiceAssemblyStatus serviceAssemblyStatus = null;
        while((iterator != null) && (iterator.hasNext() == true)) {
            serviceAssemblyStatus = (JBIServiceAssemblyStatus) iterator.next();
            if(serviceAssemblyStatus != null) {
                logger.info("/////////////////////////////////////////////////");
                logger.info("//  -- JBI Component --                        //");
                logger.info("/////////////////////////////////////////////////");
                logger.info("//  name is: "+ serviceAssemblyStatus.serviceAssemblyName);
                logger.info("//  description is: "+ serviceAssemblyStatus.serviceAssemblyDescription);
                logger.info("//  state is: "+ serviceAssemblyStatus.status);
                JBIServiceUnitStatus unitStatus = null;
                Iterator innerIterator = serviceAssemblyStatus.jbiServiceUnitStatusList.iterator();
                while(innerIterator.hasNext() == true) {
                    unitStatus = (JBIServiceUnitStatus)innerIterator.next();
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
        }

    }
    
    
    public static void main(String[] args) {
    }
}
