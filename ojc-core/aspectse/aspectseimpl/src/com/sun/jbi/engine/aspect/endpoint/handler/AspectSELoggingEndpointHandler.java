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
 * @(#)AspectSELoggingEndpointHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.handler;

import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.aspect.endpoint.AspectSELoggingEndpoint;
import com.sun.jbi.engine.aspect.utils.XMLFile;
import java.io.File;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;

/**
 * @author Sujit Biswas
 *
 */
public class AspectSELoggingEndpointHandler extends AspectSEEndpointHandler {
    
    private Logger logger = Logger.getLogger(AspectSELoggingEndpointHandler.class.getName());
    
    public AspectSELoggingEndpointHandler(AspectSELoggingEndpoint ept) {
        super(ept);
    }
    
    
    
    protected void parseAdvice() {
        /* XSD CURRENTLY DOES NOT SUPPORT PROPERTIES
        //Get Reference to Advice Map from
        Element adviceElement  = this.endpoint.getAdvice();
         
        if (adviceElement != null)
        {
            XMLFile xmlAdviceRoot = new XMLFile(adviceElement);
            NodeList configs = xmlAdviceRoot.getElementByTagName(AspectConstants.CONFIG_TAG);
            // No Iteration needed as config tag would always be one.
            NodeList properties =  ((Element)configs.item(0)).getElementsByTagName(AspectConstants.PROPERTY_TAG);
         
            for (int i=0; i < properties.getLength(); i++)
            {
                Element property = (Element)properties.item(i);
                String key = property.getAttribute(AspectConstants.PROPERTY_ATTR_NAME);
                String value = property.getAttribute(AspectConstants.PROPERTY_ATTR_VALUE);
                if (key == null) continue;
                value = (value == null ? "" : value);
                configObj.setProperties(key, value);
            }
         
             // Logging API does not support chanding rotationpolicy, once lofile is set and activated for logging.
             // This call will take care of initializing logging roration before setting up the logging file. -- Refactor/Change with better impl later.
             setRotationPolicyfromConfig();
         
            Set<String> keys = configObj.getProperties().keySet();
            Iterator<String> iter = keys.iterator();
         
            while (iter.hasNext())
            {
                String element = iter.next();
                String value = configObj.getProperties().get(element);
                involeEndPointMethods(element, value);
            }
        }
         */
        // use the rootpath and filename to get the config file.
        
        File confFile = new File(rootPath, configFile);
        try {
            if (confFile.exists()) {
                XMLFile xmlFile = new XMLFile(confFile);
                
                NodeList properties = xmlFile
                        .getElementByTagName(AspectConstants.PROPERTY_TAG);
                for (int i = 0; i < properties.getLength(); i++) {
                    Element property = (Element) properties.item(i);
                    String key = property
                            .getAttribute(AspectConstants.PROPERTY_ATTR_NAME);
                    String value = property
                            .getAttribute(AspectConstants.PROPERTY_ATTR_VALUE);
                    if (key == null)
                        continue;
                    value = (value == null ? "" : value);
                    configObj.setProperties(key, value);
                }
                
                Set<String> keys = configObj.getProperties().keySet();
                
                Iterator<String> iter = keys.iterator();
                while (iter.hasNext()) {
                    String element = iter.next();
                    String value = configObj.getProperties().get(element);
                    element = toCamelCase(element);
                    try {
                        Method m = endpoint.getClass().getMethod(
                                "set" + element, new Class[] { String.class });
                        m.invoke(endpoint, new Object[] { value });
                    } catch (Exception e) {
                        logger.info("no such method exist: " + "set" + element);
                    }
                }
            }
        } catch (Exception ex) {
            logger.log(Level.INFO, "Document Parsing Failure", ex);
        }
        
    }
    
    private void setRotationPolicyfromConfig() {
        String value = configObj.getProperties().get(AspectConstants.LOG_TAG_ROTATIONPOLICY);
        
        try {
            Method m = endpoint.getClass().getMethod("set" + this.toCamelCase(AspectConstants.LOG_TAG_ROTATIONPOLICY), new Class[] { String.class });
            m.invoke(endpoint, new Object[] { value });
        } catch (Exception e){
            logger.info("No Such Method Exists: " + "set" + this.toCamelCase(AspectConstants.LOG_TAG_ROTATIONPOLICY));
        }
    }
    
    private void involeEndPointMethods(String element, String value) {
        if (element != null && element.length() > 0) {
            element = toCamelCase(element);
        }
        
        try {
            Method m = endpoint.getClass().getMethod("set" + element, new Class[] { String.class });
            m.invoke(endpoint, new Object[] { value });
        } catch (Exception e){
            logger.info("No Such Method Exists: " + "set" + element);
        }
    }
    
    private String toCamelCase(String element) {
        return element.substring(0, 1).toUpperCase() + element.substring(1);
    }
    
    @Override
    public void save() {
        Set<String> keys = configObj.getProperties().keySet();
        
        Iterator<String> iter = keys.iterator();
        while (iter.hasNext()) {
            String element = iter.next();
            String elementC = null;
            if (element != null && element.length() > 0) {
                elementC = toCamelCase(element);
            }
            
            try {
                Method m = endpoint.getClass().getMethod("get" + elementC, (Class[])null);
                String value = (String) m.invoke(endpoint,(Object[]) null);
                configObj.setProperties(element, value);
                setAdviceProperties(element, value);
            } catch (Exception e) {
                logger.info("No Such Method Exist: " + "get" + elementC);
            }
        }
        
        // Persist Configutation Changed in the Aspect Map
        try {
            super.persistAspectMap();
        } catch (TransformerConfigurationException ex) {
            logger.log(Level.SEVERE, "TransformConfigurationException", ex);
        } catch (TransformerException ex) {
            logger.log(Level.SEVERE, "TransformerException", ex);
        }
    }
    
    private void setAdviceProperties(String pelement, String pvalue) {
        NodeList configs = endpoint.getAdvice().getElementsByTagName(AspectConstants.CONFIG_TAG);
        // No Iteration needed as config tag would always be one.
        NodeList properties =  ((Element)configs.item(0)).getElementsByTagName(AspectConstants.PROPERTY_TAG);
        for (int i=0; i < properties.getLength(); i++) {
            Element property = (Element)properties.item(i);
            String key = property.getAttribute(AspectConstants.PROPERTY_ATTR_NAME);
            if (key.equals(pelement)) {
                property.setAttribute(AspectConstants.PROPERTY_ATTR_VALUE, pvalue);
                logger.log(Level.FINE,"Setting property [" + pelement  + "] for advice [" + endpoint.getAdvice().getNodeName()  + "]. Value : " + pvalue);
            }
        }
    }
    
}