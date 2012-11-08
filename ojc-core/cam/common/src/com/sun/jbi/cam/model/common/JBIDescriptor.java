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
 * @(#)JBIDescriptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import java.io.Reader;
import java.io.StringReader;
import java.util.Properties;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
 * @author ylee
 * @author Graj
 *
 */

/**
 * This class is the java model for jbi.xml. Right now it is constructed from the DOM
 * model. In future it will be constructed from the JAXB model.
 */
public abstract class JBIDescriptor {

    protected Reader jbiXMLReader;

    public abstract boolean isSharedLibraryDescriptor();

    public abstract boolean isServiceEngineDescriptor();

    public abstract boolean isBindingComponentDescriptor();

    public abstract boolean isServiceAssemblyDescriptor();

    /**
     * get Element Tag Name with striped prefix.
     * @param aElement Element object
     * @return String with stripped prefix
     */
    public static String getElementName(Element aElement) {
        String tagName = aElement.getTagName();
        return getName(tagName);
    }

    /**
     * strips the prefix of the name if present
     * @param aName String value of Name with prefix
     * @return String for name after striping prefix
     */
    public static String getName(String aName) {
        int lastIdx = aName.lastIndexOf(':');
        if (lastIdx >= 0) {
            return aName.substring(lastIdx + 1);
        }
        return aName;
    }

    public static Properties getIdentificationProperties(Reader xmlReader) throws Exception {
        Properties properties = new Properties();
        Document xmlDoc = DOMUtil.util.buildDOMDocument(xmlReader);

        Element jbiElement = DOMUtil.util.getElement(xmlDoc, "jbi");
        if (jbiElement == null) {
            throw new Exception("xml text is not a valid jbi decriptor xml.");
        }
        // get Identification and name
        // get the main element (component, shared-library, service-assembly)
        Element saEl = DOMUtil.util.getChildElement(jbiElement, "service-assembly");
        if (saEl != null) {
            Element identificationElement = DOMUtil.util.getChildElement(saEl, "identification");
            if(identificationElement != null) {
                NodeList nodeList = identificationElement.getChildNodes();
                int count = nodeList.getLength();
                for (int i = 0; i < count; ++i) {
                    Node node = nodeList.item(i);
                    if ((node != null) && (node instanceof Element)) {
                        Element element = (Element) node;
                        String key = node.getNodeName();
                        String value = "";
                        Text text = (Text)node.getChildNodes().item(0);
                        if(text != null) {
                            value = text.getData();
                            if((key != null) && (value != null)) {
                                properties.put(key, value);
                            }
                        }
                    }
                }
            }
        }

        Element slibEl = DOMUtil.util.getChildElement(jbiElement, "shared-library");
        if (slibEl != null) {
            Element identificationElement = DOMUtil.util.getChildElement(slibEl, "identification");
            if(identificationElement != null) {
                NodeList nodeList = identificationElement.getChildNodes();
                int count = nodeList.getLength();
                for (int i = 0; i < count; ++i) {
                    Node node = nodeList.item(i);
                    if ((node != null) && (node instanceof Element)) {
                        Element element = (Element) node;
                        String key = node.getNodeName();
                        String value = "";
                        Text text = (Text)node.getChildNodes().item(0);
                        if(text != null) {
                            value = text.getData();
                            if((key != null) && (value != null)) {
                                properties.put(key, value);
                            }
                        }
                    }
                }
            }
        }

        Element componentEl = DOMUtil.util.getChildElement(jbiElement, "component");
        if (componentEl != null) {
            Element identificationElement = DOMUtil.util.getChildElement(componentEl, "identification");
            if(identificationElement != null) {
                NodeList nodeList = identificationElement.getChildNodes();
                int count = nodeList.getLength();
                for (int i = 0; i < count; ++i) {
                    Node node = nodeList.item(i);
                    if ((node != null) && (node instanceof Element)) {
                        Element element = (Element) node;
                        String key = node.getNodeName();
                        Text text = (Text)node.getChildNodes().item(0);
                        String value = "";
                        if(text != null) {
                            value = text.getData();
                            if((key != null) && (value != null)) {
                                properties.put(key, value);
                            }
                        }
                    }
                }
            }
        }
        return properties;
    }


    public static Properties getFactoryConfigProperties(Reader xmlReader) throws Exception {
        Properties properties = new Properties();
        Document xmlDoc = DOMUtil.util.buildDOMDocument(xmlReader);

//        Element jbiElement = DOMUtil.UTIL.getElement(xmlDoc, "config");
        Element jbiElement = DOMUtil.util.getElement(xmlDoc, "config:Configuration");
        if (jbiElement == null) {
            throw new Exception("xml text is not a valid jbi decriptor xml.");
        }
        if(jbiElement != null) {
            NodeList nodeList = jbiElement.getChildNodes();
            int count = nodeList.getLength();
            for (int i = 0; i < count; ++i) {
                Node node = nodeList.item(i);
                if((node != null) && (node instanceof Element)) {
                    Element element = (Element) node;
                    String key = node.getNodeName();
                    Text text = (Text)node.getChildNodes().item(0);
                    String value = null;
                    if(text != null) {
                        value = text.getData();
                    }
                    if(key != null) {
                        if(value == null) {
                            value = "";
                        }
                        properties.put(key, value);
                    }
                }
            }
        }

        return properties;
    }

    public static JBIDescriptor createJBIDescriptor(Reader xmlReader)
            throws Exception {
        Document xmlDoc = DOMUtil.util.buildDOMDocument(xmlReader);

        Element jbiElement = DOMUtil.util.getElement(xmlDoc, "jbi");
        if (jbiElement == null) {
            throw new Exception("xml text is not a valid jbi decriptor xml.");
        }
        // get the main element (component, shared-library, service-assembly)
        Element saEl = DOMUtil.util.getChildElement(jbiElement, "service-assembly");
        if (saEl != null) {
            return ServiceAssemblyDD.createServiceAssemblyDD(saEl);
        }

        Element slibEl = DOMUtil.util.getChildElement(jbiElement, "shared-library");
        if (slibEl != null) {
            return SharedLibraryDD.createSharedLibraryDD(slibEl);
        }

        Element compEl = DOMUtil.util.getChildElement(jbiElement, "component");
        if (compEl != null) {
            String type = compEl.getAttribute("type");
            // System.out.println("COMPONENT TYPE: " + type);
            if ("service-engine".equals(type)) {
                return ServiceEngineDD.createServiceEngineDD(compEl);
            } else if ("binding-component".equals(type)) {
                return BindingComponentDD.createBindingComponentDD(compEl);
            }
        }

        // not a jbi decriptor
        throw new Exception("xml text is not a valid jbi decriptor xml.");
    }

    public static JBIDescriptor createJBIDescriptor(String xmlText)
            throws Exception {
        return createJBIDescriptor(new StringReader(xmlText));
    }

    public static Properties getFactoryConfigProperties(String xmlText) throws Exception {
        return getFactoryConfigProperties(new StringReader(xmlText));
    }

    public static Properties getIdentificationProperties(String xmlText) throws Exception {
        return getIdentificationProperties(new StringReader(xmlText));
    }

    public static class SharedLibraryDD extends JBIDescriptor {
        protected SharedLibraryDD() {
            super();
        }

        public boolean isSharedLibraryDescriptor() {
            return true;
        }

        public boolean isServiceEngineDescriptor() {
            return false;
        }

        public boolean isBindingComponentDescriptor() {
            return false;
        }

        public boolean isServiceAssemblyDescriptor() {
            return false;
        }

        public static SharedLibraryDD createSharedLibraryDD(Element slibEl) {
            return new SharedLibraryDD();
        }
    }

    /**
     * in future this will implement common code for engine and binding dd
     */
    public static abstract class ComponentDD extends JBIDescriptor {
        protected ComponentDD() {
            super();
        }
    }

    public static class ServiceEngineDD extends ComponentDD {
        protected ServiceEngineDD() {
            super();
        }

        public boolean isSharedLibraryDescriptor() {
            return false;
        }

        public boolean isServiceEngineDescriptor() {
            return true;
        }

        public boolean isBindingComponentDescriptor() {
            return false;
        }

        public boolean isServiceAssemblyDescriptor() {
            return false;
        }

        public static ServiceEngineDD createServiceEngineDD(Element compEl) {
            return new ServiceEngineDD();
        }

    }

    public static class BindingComponentDD extends ComponentDD {
        protected BindingComponentDD() {
            super();
        }

        public boolean isSharedLibraryDescriptor() {
            return false;
        }

        public boolean isServiceEngineDescriptor() {
            return false;
        }

        public boolean isBindingComponentDescriptor()

        {
            return true;
        }

        public boolean isServiceAssemblyDescriptor() {
            return false;
        }

        public static BindingComponentDD createBindingComponentDD(Element compEl) {
            return new BindingComponentDD();
        }
    }

}
