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

import com.gestalt.jbi.component.bootstrap.AbstractBootstrapper;
import com.gestalt.jbi.rss.component.rss.persistence.ConfigurationManager;
import com.gestalt.jbi.rss.component.rss.persistence.IOUtils;
import com.gestalt.jbi.rss.component.rss.persistence.RSSConfigExtensions;

import org.w3c.dom.DocumentFragment;

import java.io.File;

import java.util.HashMap;
import java.util.Map;

import javax.jbi.JBIException;


/**
 * Bootstrap class for this Component.
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSBootstrapper extends AbstractBootstrapper {
    public void doCleanUp() throws JBIException {
    }

    public Object createMBean() {
        return null;
    }

    public void doInit() throws JBIException {
    }

    public void doInstall() throws JBIException {
        DocumentFragment doc = installationContext.getInstallationDescriptorExtension();
        Map props = new HashMap();
        ConfigurationManager.parseConfigExtensions(props, doc);

        File file = new File(installationContext.getContext().getWorkspaceRoot() +
                File.separator + RSSConfigExtensions.PERSIST_FILE_NAME);

        IOUtils.storeObject(props, file);
    }

    public void doUninstall() throws JBIException {
    }
}
