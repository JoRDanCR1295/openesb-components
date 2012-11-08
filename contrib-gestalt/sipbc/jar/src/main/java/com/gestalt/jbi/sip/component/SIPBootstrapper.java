/**
 *   sip-binding-component - SIP Binding Component
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
package com.gestalt.jbi.sip.component;

import com.gestalt.jbi.component.bootstrap.AbstractBootstrapper;
import com.gestalt.jbi.sip.component.security.IOUtils;
import com.sun.jbi.internationalization.Messages;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Node;

import java.io.File;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;


/**
 * @author : csturtz
 */
public class SIPBootstrapper extends AbstractBootstrapper {
    private static final Logger log = Messages.getLogger(SIPBootstrapper.class);
    private static Messages messages = Messages.getMessages(SIPBootstrapper.class);

    public Object createMBean() {
        return null;
    }

    public void doCleanUp() throws JBIException {
    }

    public void doInit() throws JBIException {
    }

    public void doInstall() throws JBIException {
        log.log(Level.INFO,messages.getString("SIPBC-R00701.bootstrapInstalling"));

        DocumentFragment doc = installationContext.getInstallationDescriptorExtension();
        Map props = new HashMap();
        parseConfigExtensions(props, doc);

        File file = new File(installationContext.getContext().getWorkspaceRoot() +
                File.separator + SIPConfigExtensions.CONFIG_FILE_NAME);

        IOUtils.storeObject(props, file);
        log.log(Level.INFO,messages.getString("SIPBC-R00702.bootstrapInstalled"));
    }

    public void doUninstall() throws JBIException {
    }

    private Map parseConfigExtensions(Map props, Node node) {
        if (node.hasChildNodes()) {
            for (int i = 0; i < node.getChildNodes().getLength(); i++) {
                parseConfigExtensions(props, node.getChildNodes().item(i));
            }
        } else {
            String content = node.getTextContent().trim();

            if ((null != content) && (content.length() != 0)) {
                String p = node.getParentNode().getNodeName().contains(":")
                    ? node.getParentNode().getNodeName().split(":")[1]
                    : node.getParentNode().getNodeName();
                props.put(p, node.getTextContent());
            }
        }

        return props;
    }
}
