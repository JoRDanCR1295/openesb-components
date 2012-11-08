/*
 * @(#)InstallationConfigurationMBean.java        $Revision: 1.2 $ $Date: 2008/07/14 16:30:26 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.component;

/**
 * JMX MBean interface for configuring component's installation specific parameters.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/14 16:30:26 $
 * 
 * @since 0.1
 */
public interface InstallationConfigurationMBean {
    
    int getPoolSize();

    void setPoolSize(int poolSize);
    
    int getMaxServiceUnits();

    void setMaxServiceUnits(int maxServiceUnits);
}
