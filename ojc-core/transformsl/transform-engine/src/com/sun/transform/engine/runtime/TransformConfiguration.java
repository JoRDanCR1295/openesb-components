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
 * @(#)TransformConfiguration.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime;

import javax.management.MBeanException;
import com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean;
import com.sun.transform.engine.EngineConfig.TransformEngine;

/**
 * Defines configurable properties for transformation engine.
 * @author Kevan Simpson
 */
public interface TransformConfiguration extends PollerConfigMBean {
    /**
     * Fetches the name of the {@link TransformEngine} configuration.
     * @return the name of the {@link TransformEngine} configuration.
     */
    public String getTransformEngine();
    
    /**
     * Sets the name of the {@link TransformEngine} configuration.
     * @param eng The name of the {@link TransformEngine} configuration.
     * @throws MBeanException if an error occurs persisting configuration changes.
     */
    public void setTransformEngine(String eng) throws MBeanException;
}
