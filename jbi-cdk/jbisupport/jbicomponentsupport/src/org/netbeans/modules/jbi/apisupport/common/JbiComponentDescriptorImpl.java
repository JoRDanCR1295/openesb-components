/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.jbi.apisupport.common;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author chikkala
 */
public class JbiComponentDescriptorImpl extends JbiDescriptor.AbstractJbiDescriptor implements JbiComponentDescriptor {

    private Element mCompEl;
    private Element mIdEl;
    private Element mNameEl;
    private Element mDescEl;
    private Element mCompClassNameEl;
    private Element mBootstrapClassNameEl;
    private Element mCompClassPathEl;
    private Element mBootstrapClassPathEl;

    protected JbiComponentDescriptorImpl(Document domDocument) {
        super(domDocument);
    }

    private Element getComponentElement() {
        if (this.mCompEl == null) {
            Document xmlDoc = getDOMDocument();
            Element jbiEl = getJbiElement();
            this.mCompEl = DOMUtil.UTIL.getChildElement(jbiEl, "component");
            if (this.mCompEl == null) {
                this.mCompEl = xmlDoc.createElement("component");
                Node firstChild = jbiEl.getFirstChild();
                if (firstChild != null) {
                    jbiEl.insertBefore(this.mCompEl, firstChild);
                } else {
                    jbiEl.appendChild(this.mCompEl);
                }
            }
        }
        return this.mCompEl;
    }

    private Element getIdentificationElement() {
        if (this.mIdEl == null) {
            Document xmlDoc = getDOMDocument();
            Element compEl = getComponentElement();
            this.mIdEl = DOMUtil.UTIL.getChildElement(compEl, "identification");
            if (this.mIdEl == null) {
                this.mIdEl = xmlDoc.createElement("identification");
                Node firstChild = compEl.getFirstChild();
                if (firstChild != null) {
                    compEl.insertBefore(this.mIdEl, firstChild);
                } else {
                    compEl.appendChild(this.mIdEl);
                }
            }
        }
        return this.mIdEl;
    }

    private Element getNameElement() {
        if (this.mNameEl == null) {
            Document xmlDoc = getDOMDocument();
            Element idEl = getIdentificationElement();
            this.mNameEl = DOMUtil.UTIL.getChildElement(idEl, "name");
            if (this.mNameEl == null) {
                this.mNameEl = xmlDoc.createElement("name");
                Node firstChild = idEl.getFirstChild();
                if (firstChild != null) {
                    idEl.insertBefore(this.mNameEl, firstChild);
                } else {
                    idEl.appendChild(this.mNameEl);
                }
            }
        }
        return this.mNameEl;
    }

    private Element getDescriptionElement() {
        if (this.mDescEl == null) {
            Document xmlDoc = getDOMDocument();
            Element idEl = getIdentificationElement();
            this.mDescEl = DOMUtil.UTIL.getChildElement(idEl, "description");
            if (this.mDescEl == null) {
                this.mDescEl = xmlDoc.createElement("description");
                Node nameEl = getNameElement();
                Node nextNode = nameEl.getNextSibling();
                if (nextNode != null) {
                    idEl.insertBefore(this.mDescEl, nextNode);
                } else {
                    idEl.appendChild(this.mDescEl);
                }
            }
        }
        return this.mDescEl;
    }

    private Element getComponentClassNameElement() {
        if (this.mCompClassNameEl == null) {
            Document xmlDoc = getDOMDocument();
            Element compEl = getComponentElement();
            this.mCompClassNameEl = DOMUtil.UTIL.getChildElement(compEl, "component-class-name");
            if (this.mCompClassNameEl == null) {
                this.mCompClassNameEl = xmlDoc.createElement("component-class-name");
                Node idEl = this.getIdentificationElement();
                Node nextNode = idEl.getNextSibling();
                if (nextNode != null) {
                    compEl.insertBefore(this.mCompClassNameEl, nextNode);
                } else {
                    compEl.appendChild(this.mCompClassNameEl);
                }
            }
        }
        return this.mCompClassNameEl;
    }

    private Element getComponentClassPathElement() {
        if (this.mCompClassPathEl == null) {
            Document xmlDoc = getDOMDocument();
            Element compEl = getComponentElement();
            this.mCompClassPathEl = DOMUtil.UTIL.getChildElement(compEl, "component-class-path");
            if (this.mCompClassPathEl == null) {
                this.mCompClassPathEl = xmlDoc.createElement("component-class-path");
                Node compClassNameEl = this.getComponentClassNameElement();
                Node nextNode = compClassNameEl.getNextSibling();
                if (nextNode != null) {
                    compEl.insertBefore(this.mCompClassPathEl, nextNode);
                } else {
                    compEl.appendChild(this.mCompClassPathEl);
                }
            }
        }
        return this.mCompClassPathEl;
    }

    private Element getBootstrapClassNameElement() {
        if (this.mBootstrapClassNameEl == null) {
            Document xmlDoc = getDOMDocument();
            Element compEl = getComponentElement();
            this.mBootstrapClassNameEl = DOMUtil.UTIL.getChildElement(compEl, "bootstrap-class-name");
            if (this.mBootstrapClassNameEl == null) {
                this.mBootstrapClassNameEl = xmlDoc.createElement("bootstrap-class-name");
                Node compClassPathEl = this.getComponentClassPathElement();
                Node nextNode = compClassPathEl.getNextSibling();
                if (nextNode != null) {
                    compEl.insertBefore(this.mBootstrapClassNameEl, nextNode);
                } else {
                    compEl.appendChild(this.mBootstrapClassNameEl);
                }
            }
        }
        return this.mBootstrapClassNameEl;
    }

    private Element getBootstrapClassPathElement() {
        if (this.mBootstrapClassPathEl == null) {
            Document xmlDoc = getDOMDocument();
            Element compEl = getComponentElement();
            this.mBootstrapClassPathEl = DOMUtil.UTIL.getChildElement(compEl, "bootstrap-class-path");
            if (this.mBootstrapClassPathEl == null) {
                this.mBootstrapClassPathEl = xmlDoc.createElement("bootstrap-class-path");
                Node bootstrapClassNameEl = this.getBootstrapClassNameElement();
                Node nextNode = bootstrapClassNameEl.getNextSibling();
                if (nextNode != null) {
                    compEl.insertBefore(this.mBootstrapClassPathEl, nextNode);
                } else {
                    compEl.appendChild(this.mBootstrapClassPathEl);
                }
            }
        }
        return this.mBootstrapClassPathEl;
    }

    public String getType() {
        Element compEl = getComponentElement();
        return compEl.getAttribute("type");
    }

    public void setType(String type) {
        Element compEl = getComponentElement();
        compEl.setAttribute("type", type);
    }

    public String getComponentCLDelegation() {
        Element compEl = getComponentElement();
        return compEl.getAttribute(COMPONENT_CL_DELEGATION_ATTR);
    }

    public void setComponentCLDelegation(String clDelegation) {
        Element compEl = getComponentElement();
        if (clDelegation == null || clDelegation.trim().length() == 0) {
            try {
                compEl.removeAttribute(COMPONENT_CL_DELEGATION_ATTR);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } else {
            compEl.setAttribute(COMPONENT_CL_DELEGATION_ATTR, clDelegation);
        }
    }

    public String getBootstrapCLDelegation() {
        Element compEl = getComponentElement();
        return compEl.getAttribute(BOOTSTRAP_CL_DELEGATION_ATTR);
    }

    public void setBootstrapCLDelegation(String clDelegation) {
        Element compEl = getComponentElement();
        if (clDelegation == null || clDelegation.trim().length() == 0) {
            try {
                compEl.removeAttribute(BOOTSTRAP_CL_DELEGATION_ATTR);
            } catch (Exception ex) {
                //ignore
                ex.printStackTrace();
            }
        } else {
            compEl.setAttribute(BOOTSTRAP_CL_DELEGATION_ATTR, clDelegation);
        }
    }

    public String getName() {
        Element nameEl = getNameElement();
        return DOMUtil.UTIL.getTextData(nameEl);
    }

    public void setName(String name) {
        Element nameEl = getNameElement();
        DOMUtil.UTIL.setTextData(nameEl, name);
    }

    public String getDescription() {
        Element descEl = getDescriptionElement();
        return DOMUtil.UTIL.getTextData(descEl);
    }

    public void setDescription(String description) {
        Element descEl = getDescriptionElement();
        DOMUtil.UTIL.setTextData(descEl, description);
    }

    public String getComponentClassName() {
        Element compClassNameEl = getComponentClassNameElement();
        return DOMUtil.UTIL.getTextData(compClassNameEl);
    }

    public void setComponentClassName(String componentClassName) {
        Element compClassNameEl = getComponentClassNameElement();
        DOMUtil.UTIL.setTextData(compClassNameEl, componentClassName);
    }

    private String[] getClassPath(Element classPathElement) {

        NodeList nodeList = DOMUtil.UTIL.getChildElements(classPathElement, "path-element");
        if (nodeList == null) {
            return new String[0];
        }
        int size = nodeList.getLength();
        String[] classPaths = new String[size];

        for (int i = 0; i < size; ++i) {
            classPaths[i] = DOMUtil.UTIL.getTextData((Element) nodeList.item(i));
        }
        return classPaths;
    }

    private void setClassPath(Element classPathElement, String[] classPath) {
        Document xmlDoc = this.getDOMDocument();
        try {
            NodeList nodeList = classPathElement.getChildNodes();
            int size = nodeList.getLength();
            Node[] nodes = new Node[size];
            for (int i = 0; i < size; ++i) {
                nodes[i] = nodeList.item(i);
            }
            for (int i = 0; i < nodes.length; ++i) {
                classPathElement.removeChild(nodes[i]);
            }
        } catch (DOMException ex) {
            ex.printStackTrace();
        }

        for (int i = 0; i < classPath.length; ++i) {
            Element pathEl = xmlDoc.createElement("path-element");
            DOMUtil.UTIL.setTextData(pathEl, classPath[i]);
            classPathElement.appendChild(pathEl);
        }
    }

    public String[] getComponentClassPath() {
        Element compClassPathEl = getComponentClassPathElement();
        return getClassPath(compClassPathEl);
    }

    public void setComponentClassPath(String[] componentClassPath) {
        Element compClassPathEl = getComponentClassPathElement();
        setClassPath(compClassPathEl, componentClassPath);
    }

    public String getBootstrapClassName() {
        Element bootstrapClassNameEl = getBootstrapClassNameElement();
        return DOMUtil.UTIL.getTextData(bootstrapClassNameEl);
    }

    public void setBootstrapClassName(String bootstrapClassName) {
        Element bootstrapClassNameEl = getBootstrapClassNameElement();
        DOMUtil.UTIL.setTextData(bootstrapClassNameEl, bootstrapClassName);
    }

    public String[] getBootstrapClassPath() {
        Element bootstrapClassPathEl = getBootstrapClassPathElement();
        return getClassPath(bootstrapClassPathEl);
    }

    public void setBootstrapClassPath(String[] bootstrapClassPath) {
        Element bootstrapClassPathEl = getBootstrapClassPathElement();
        setClassPath(bootstrapClassPathEl, bootstrapClassPath);
    }

    public String[] getSharedLibraryNames() {
        Element compEl = getComponentElement();
        NodeList nodeList = DOMUtil.UTIL.getChildElements(compEl, "shared-library");
        if (nodeList == null) {
            return new String[0];
        }
        int size = nodeList.getLength();
        String[] slibs = new String[size];

        for (int i = 0; i < size; ++i) {
            slibs[i] = DOMUtil.UTIL.getTextData((Element) nodeList.item(i));
        }
        return slibs;
    }

    public void setSharedLibraryNames(String[] sharedLibraries) {
        Document xmlDoc = this.getDOMDocument();
        Element compEl = getComponentElement();
        Element bootstrapClassPathEl = getBootstrapClassPathElement();

        try {
            NodeList nodeList = DOMUtil.UTIL.getChildElements(compEl, "shared-library");
            int size = nodeList.getLength();
            Node[] nodes = new Node[size];
            for (int i = 0; i < size; ++i) {
                nodes[i] = nodeList.item(i);
            }
            for (int i = 0; i < nodes.length; ++i) {
                compEl.removeChild(nodes[i]);
            }
        } catch (DOMException ex) {
            ex.printStackTrace();
        }
        Node refNode = bootstrapClassPathEl.getNextSibling();
        for (String slibName : sharedLibraries) {
            Element slibEl = xmlDoc.createElement("shared-library");
            DOMUtil.UTIL.setTextData(slibEl, slibName);
            if (refNode != null) {
                compEl.insertBefore(slibEl, refNode);
            } else {
                compEl.appendChild(slibEl);
            }
        }
    }
}