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
 * @(#)InstallerExtMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.bootstrap;

import java.util.Properties;
import javax.management.InvalidAttributeValueException;

import com.sun.jbi.common.qos.config.ComponentConfig;

/**
 * Installer Extension MBean, allow configuration to be changed before
 * installation.
 * 
 * @author Noel.Ang@sun.com
 */
public interface InstallerExtMBean {
    Integer getOutboundThreads();

    void setOutboundThreads(Integer val)
            throws InvalidAttributeValueException;
    
    Boolean getSeparateTrxBranches();
    
    void setSeparateTrxBranches(Boolean val)
            throws InvalidAttributeValueException;
    
    void read(ComponentConfig config);
    
    Properties toProperties();
}
