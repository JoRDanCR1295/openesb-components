/*
 * @(#)ExecutorModule.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
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

package org.openesb.components.rules4jbi.engine.guice.modules;

import com.google.inject.AbstractModule;

import org.openesb.components.rules4jbi.engine.guice.annotations.MaxServiceUnits;

/**
 * Guice module for configuring constants.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
 * 
 * @since 0.1
 */
public class ConstantModule extends AbstractModule {
    
    private final int maxServiceUnits;
    
    public ConstantModule(int maxServiceUnits) {
        this.maxServiceUnits = maxServiceUnits;
    }

    @Override
    protected void configure() {
        bindConstant().annotatedWith(MaxServiceUnits.class).to(maxServiceUnits);
    }
}
