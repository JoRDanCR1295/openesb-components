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

import java.io.File;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;


/**
 * Manager class for RSS configuration runtime changes
 */
public class RSSConfigExtensions implements RSSConfigExtensionsMBean {
    public static final String PERSIST_FILE_NAME = "persist.config";
    public static final String PERSIST = "Persist";
    private Map<String, String> props;
    private Logger log = Logger.getLogger(this.getClass().getName());
    private File file;
    private String mConfigSchema;
    private String mConfigData;

    public RSSConfigExtensions(String path) {
        init(path);
    }

    private void init(String path) {
        this.file = new File(path + File.separator + PERSIST_FILE_NAME);

        if (file.exists()) {
            props = (Map) IOUtils.loadObject(file);
        }

        if (null == props) {
            log.fine("Configuration Property file not found");
            props = new HashMap<String, String>();
        }

        if (null == props.get(PERSIST)) {
            log.fine("Persist property not found defaulting to false");
            props.put(PERSIST, Boolean.FALSE.toString());
        }
    }

    public Boolean getPersist() {
        return Boolean.valueOf(props.get(PERSIST));
    }

    public void setPersist(Boolean persist) {
        log.fine("Persist was changed to: " + persist);
        props.put(PERSIST, persist.toString());
        IOUtils.storeObject(props, file);
    }

    public String retrieveConfigurationDisplayData() {
        return mConfigData;
    }

    public String retrieveConfigurationDisplaySchema() {
        return mConfigSchema;
    }

    public void setConfigurationDisplaySchema(String configurationDisplaySchema) {
        this.mConfigSchema = configurationDisplaySchema;
    }

    public void setConfigurationDisplayData(String configurationDisplayData) {
        this.mConfigData = configurationDisplayData;
    }
}
