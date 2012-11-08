/**
 *   rss-binding-component - RSS Binding Component
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
package com.gestalt.jbi.rss.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.component.manager.AbstractServiceUnitManager;
import com.gestalt.jbi.component.manager.deployment.AbstractDeployer;


/**
 * Component Class for this Component.
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSComponent extends AbstractComponent {
    /**
     * Creates a component life cycle for this component.
     *
     * @param abstractComponent
     * @return RSSComponentLifeCycle
     */
    public AbstractComponentLifeCycle createComponentLifeCycle(
        AbstractComponent abstractComponent) {
        return new RSSComponentLifeCycle(this);
    }

    /**
     * Creates a service unit manager for this component.
     *
     * @param abstractComponent
     * @return RSSServiceUnitManager
     */
    public AbstractServiceUnitManager createServiceUnitManager(
        AbstractComponent abstractComponent) {
        return new RSSServiceUnitManager(this,
            new AbstractDeployer[] { new RSSWSDLDeployer(this) });
    }
}
