/**
 *   rss-binding-component-extensions - Extensions for the RSS Binding Component
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
package com.gestalt.jbi.rss.extensions;

import javax.wsdl.extensions.ExtensionRegistry;


/**
 * Creates a new RSSExtSerializer and registers this object with that.
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSExtensionRegistry extends ExtensionRegistry {
    private static final long serialVersionUID = 1L;

    /**
     * Constructor
     */
    public RSSExtensionRegistry() {
        super();

        final RSSExtSerializer rssExtSerializer = new RSSExtSerializer();
        rssExtSerializer.registerSerializer(this);
    }
}
