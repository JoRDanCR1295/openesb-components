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
package com.gestalt.jbi.rss.component.rss.persistence;

import com.sun.syndication.feed.module.georss.GeoRSSModule;
import com.sun.syndication.feed.module.georss.GeoRSSUtils;
import com.sun.syndication.feed.synd.SyndEntryImpl;


/**
 * The class was created because the Hibernate mapping needed a getter and setter
 * for the georss module. This should probably be replaced by a better hibernate
 * mapping.
 * todo FIGURE OUT BETTER HIBERNATE MAPPING SO THIS WONT BE NECESSARY
 */
public class SyndEntryPersistenceImpl extends SyndEntryImpl {
    public GeoRSSModule getGeoRSSModule() {
        return GeoRSSUtils.getGeoRSS(this);
    }

    public void setGeoRSSModule(GeoRSSModule module) {
        super.getModules().add(module);
    }
}
