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

import com.gestalt.jbi.component.bootstrap.AbstractBootstrapper;

import javax.jbi.JBIException;


/**
 * @author rriven
 *
 */
public class UDDIBootstrapper extends AbstractBootstrapper {
    /* (non-Javadoc)
     * @see com.gestalt.jbi.component.bootstrap.AbstractBootstrapper#createMBean()
     */
    @Override
    public Object createMBean() {
        return null;
    }

    /* (non-Javadoc)
     * @see com.gestalt.jbi.component.bootstrap.AbstractBootstrapper#doCleanUp()
     */
    @Override
    public void doCleanUp() throws JBIException {
    }

    /* (non-Javadoc)
     * @see com.gestalt.jbi.component.bootstrap.AbstractBootstrapper#doInit()
     */
    @Override
    public void doInit() throws JBIException {
    }

    /* (non-Javadoc)
     * @see com.gestalt.jbi.component.bootstrap.AbstractBootstrapper#doInstall()
     */
    @Override
    public void doInstall() throws JBIException {
    }

    /* (non-Javadoc)
     * @see com.gestalt.jbi.component.bootstrap.AbstractBootstrapper#doUninstall()
     */
    @Override
    public void doUninstall() throws JBIException {
    }
}
