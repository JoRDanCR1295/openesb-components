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
 * @(#)XsltConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.xslt;

import com.sun.transform.engine.EngineConfig;
import com.sun.transform.engine.model.ProcessFactory;
import com.sun.transform.engine.runtime.Engine;

/**
 * Default implementation of {@link EngineConfig} supporting Xslt 1.
 * @author Kevan Simpson
 */
public class XsltConfig extends EngineConfig {

    /**
     * 
     */
    public XsltConfig() {
    }

    /** @see com.sun.transform.engine.EngineConfig#getEngine() */
    @Override
    public Engine getEngine() {
        return new XsltEngine();
    }

    /** @see com.sun.transform.engine.EngineConfig#getProcessFactory() */
    @Override
    public ProcessFactory getProcessFactory() {
        return new XsltProcessFactory();
    }
}
