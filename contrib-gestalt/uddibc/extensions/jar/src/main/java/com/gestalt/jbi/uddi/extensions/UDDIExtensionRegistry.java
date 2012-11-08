/**
 *   uddi-binding-component-extensions - Extensions for the UDDI Binding Component
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
package com.gestalt.jbi.uddi.extensions;

import javax.wsdl.extensions.ExtensionRegistry;


/**
 *
 * @author rriven
 */
public class UDDIExtensionRegistry extends ExtensionRegistry {
    private static final long serialVersionUID = 1L;

    /** Creates a new instance of UDDIExtensionRegistry */
    public UDDIExtensionRegistry() {
        super();

        final UDDIExtSerializer uddiExtSerializer = new UDDIExtSerializer();
        uddiExtSerializer.registerSerializer(this);
    }
}
