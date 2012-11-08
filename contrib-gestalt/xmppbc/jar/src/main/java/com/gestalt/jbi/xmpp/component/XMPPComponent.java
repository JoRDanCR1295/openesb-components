/**
 *   xmpp-binding-component - XMPP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
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
package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.component.manager.AbstractServiceUnitManager;
import com.gestalt.jbi.component.manager.deployment.AbstractDeployer;


public class XMPPComponent extends AbstractComponent {
    public AbstractComponentLifeCycle createComponentLifeCycle(
        AbstractComponent arg0) {
        return new XMPPComponentLifeCycle(this);
    }

    public AbstractServiceUnitManager createServiceUnitManager(
        AbstractComponent arg0) {
        return new XMPPServiceUnitManager(this,
            new AbstractDeployer[] { new XMPPWSDLDeployer(this), new XMPPXBeanDeployer(this) });
    }
}
