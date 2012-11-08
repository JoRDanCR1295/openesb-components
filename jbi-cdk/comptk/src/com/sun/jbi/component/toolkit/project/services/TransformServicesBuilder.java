/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)TransformServicesBuilder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.services;

import java.io.File;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.model.Services;
import com.sun.jbi.component.toolkit.project.util.XPathElement;

/**
 * 
 * @author Kevan Simpson
 */
public class TransformServicesBuilder extends AbstractServicesBuilder {
    /** @see com.sun.jbi.component.toolkit.project.services.AbstractServicesBuilder#generateServices(java.lang.String, java.lang.String) */
    @Override
    public Services generateServices(String unitName, String artifactRoot) {
        File file = new File(artifactRoot, "transformmap.xml");
        if (file.exists()) {
            XPathElement tmap = XPathElement.loadXmlFile(
                    file,
                    "//tmap:invoke", "//tmap:service", "//tmap:import");
            String tns = tmap.getElement().getAttribute("targetNamespace");
            NodeList importList = tmap.getNodeSet("//tmap:import");
            if (importList != null) {
                for (int i = 0, n = importList.getLength(); i < n; i++) {
                    Element elem = (Element) importList.item(i);
                    readWsdl(new File(elem.getAttribute("location")));
                }
            }
            // provides
            NodeList srvcList = tmap.getNodeSet("//tmap:service");
            if (srvcList != null) {
                for (int i = 0, n = srvcList.getLength(); i < n; i++) {
                    addService(new XPathElement((Element) srvcList.item(i)), tns, true);
                }
            }
            // consumes
            NodeList invokeList = tmap.getNodeSet("//tmap:invoke");
            if (invokeList != null) {
                for (int i = 0, n = invokeList.getLength(); i < n; i++) {
                    addService(new XPathElement((Element) invokeList.item(i)), tns, false);
                }
            }
        }

        return createServices();
    }

    protected void addService(XPathElement elem, String tns, boolean provides) {
        addService(new EndpointInfo(provides,
                                    elem.getString("@name"),
                                    elem.getQName("@portType"),
                                    new QName(tns, "xsltse"),
                                    null));
    }
}
