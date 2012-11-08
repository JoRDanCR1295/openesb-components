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
 * @(#)EngineInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

import com.sun.jbi.engine.bpel.DeploymentBindings;

public class EngineInfo {
    EngineSimulator engine;
    DeploymentBindings deplBindings;
    String engineId;
    
    public EngineInfo(EngineSimulator engine, DeploymentBindings deplBindings, String engineId) {
        this.engine = engine;
        this.deplBindings = deplBindings;
        this.engineId = engineId;
    }

    public DeploymentBindings getDeplBindings() {
        return deplBindings;
    }

    public EngineSimulator getEngine() {
        return engine;
    }

    public String getEngineId() {
        return engineId;
    }
}
