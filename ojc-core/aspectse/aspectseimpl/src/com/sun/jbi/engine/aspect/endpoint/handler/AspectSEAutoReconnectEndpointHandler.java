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
 * @(#)AspectSEAutoReconnectEndpointHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.handler;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEAutoReconnectEndpoint;
import com.sun.jbi.engine.aspect.endpoint.support.ConfigObject;
import com.sun.jbi.engine.aspect.utils.XMLFile;

/**
 * This Class handles the parsing of the configuration file
 * and setting the endpoint with those properties.
 *
 * @author karthikeyan s
 */
public class AspectSEAutoReconnectEndpointHandler extends AspectSEEndpointHandler {
    
    private static final Logger logger = Logger.getLogger(AspectSEAutoReconnectEndpointHandler.class.getName());
    
    /** Creates a new instance of AspectSEAutoReconnectEndpointHandler */
    public AspectSEAutoReconnectEndpointHandler(AspectSEEndpoint endPt) {
        super(endPt);
    }
    
    protected void parseAdvice() {        
        // use the rootpath and filename to get the config file.
        File confFile = new File(rootPath, configFile);
        try {
            if(confFile.exists() && (configFile != null && !configFile.equals("") ) ) {
                XMLFile xmlFile = new XMLFile(confFile);
                ConfigObject config = new ConfigObject();
                NodeList properties = xmlFile.getElementByTagName(AspectConstants.PROPERTY_TAG);
                for(int i = 0; i < properties.getLength(); i++) {
                    Element property = (Element)properties.item(i);
                    String key = property.getAttribute(AspectConstants.PROPERTY_ATTR_NAME);
                    String value = property.getAttribute(AspectConstants.PROPERTY_ATTR_VALUE);
                    if(key == null) continue;
                    value = (value == null? "" : value);
                    config.setProperties(key, value);
                }
                // now set all the properties into config object.
                String rate = config.getProperties().get(AspectConstants.RETRY_PROP_RATE);
                String timeout = config.getProperties().get(AspectConstants.RETRY_PROP_TIMEOUT);
                ((AspectSEAutoReconnectEndpoint)endpoint).setRate(rate);
                ((AspectSEAutoReconnectEndpoint)endpoint).setTimeout(timeout);
            }
        } catch (Exception ex) {
            logger.log(Level.INFO, "Document Parsing Failure", ex);
        }
    }
    
    @Override
    public void save() {
        configObj = new ConfigObject();
        configObj.setProperties(AspectConstants.RETRY_PROP_RATE, 
                ((AspectSEAutoReconnectEndpoint)endpoint).getRate());
        configObj.setProperties(AspectConstants.RETRY_PROP_TIMEOUT, 
                ((AspectSEAutoReconnectEndpoint)endpoint).getTimeout());
        super.save();
    }
}
