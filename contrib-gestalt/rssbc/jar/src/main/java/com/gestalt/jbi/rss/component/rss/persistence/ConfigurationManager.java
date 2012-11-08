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

import org.w3c.dom.Node;

import java.util.Map;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;


public class ConfigurationManager {
    public static Map parseConfigExtensions(Map props, Node node) {
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

    public static ObjectName constructObjectName(ComponentType componentType,
        String componentName) throws MalformedObjectNameException {
        String objectNameString = "com.sun.ebi:ServiceType=Configuration,InstallationType=" +
            componentType + ",IdentificationName=" + componentName;

        return new ObjectName(objectNameString);
    }
}
