/**
 *   uddi-binding-component - UDDI Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.uddi.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.manager.AbstractServiceUnitManager;
import com.gestalt.jbi.component.manager.deployment.AbstractDeployer;


public class UDDIServiceUnitManager extends AbstractServiceUnitManager {
    public UDDIServiceUnitManager(AbstractComponent component,
        AbstractDeployer[] deployers) {
        super(component, deployers);
    }

    @Override
    public void doInit(String serviceUnitName, String serviceUnitRootPath)
        throws Exception {
    }
}
