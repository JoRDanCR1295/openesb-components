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
 * @(#)AspectSEThrottlingEndpointHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.handler;

import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEThrottlingEndpoint;
import com.sun.jbi.engine.aspect.endpoint.support.ConfigObject;
import com.sun.jbi.engine.aspect.utils.XMLFile;
import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 *
 * @author karthikeyan s
 */
public class AspectSEThrottlingEndpointHandler extends AspectSEEndpointHandler {
    
private static final Logger logger = Logger.getLogger(AspectSEThrottlingEndpointHandler.class.getName());
    
    /** Creates a new instance of AspectSEThrottlingEndpointHandler */
    public AspectSEThrottlingEndpointHandler(AspectSEEndpoint endPt) {
        super(endPt);
    }

    protected void parseAdvice() {        
        // use the rootpath and filename to get the config file.
        File confFile = new File(rootPath, configFile);
        
        try {
            if(confFile.exists()) {
                XMLFile xmlFile = new XMLFile(confFile);
                ConfigObject config = new ConfigObject();
                NodeList properties = xmlFile.getElementByTagName(AspectConstants.PROPERTY_TAG);
                for(int i = 0; i < properties.getLength(); i++)
                {
                    Element property = (Element)properties.item(i);
                    String key = property.getAttribute(AspectConstants.PROPERTY_ATTR_NAME);
                    String value = property.getAttribute(AspectConstants.PROPERTY_ATTR_VALUE);
                    if(key == null) continue;
                    value = (value == null? "" : value);
                    config.setProperties(key, value);
                }
                
                // now set all the properties into config object.
                String polling_timeout = config.getProperties().get(AspectConstants.THROTTLE_POLLING_TIMER);
                String throttle_rate = config.getProperties().get(AspectConstants.THROTTLE_RATE);
                String queue_length = config.getProperties().get(AspectConstants.MAX_THROTTLE_QUEUE_SIZE);
                
                if (polling_timeout != null)
                {
                    ((AspectSEThrottlingEndpoint)endpoint).setPollingTimer(polling_timeout);
                }
                
                if (throttle_rate != null)
                {
                    ((AspectSEThrottlingEndpoint)endpoint).setThrottleRate(throttle_rate);
                }

                if (queue_length != null)
                {
                    ((AspectSEThrottlingEndpoint)endpoint).setMaxThrottleQueueSize(queue_length);
                }                
                
            }
        } catch (Exception ex)
        {
            logger.log(Level.INFO, "Document Parsing Failure", ex);
        }
    }
    
    @Override
    public void save()
    {
        configObj = new ConfigObject();
        configObj.setProperties(AspectConstants.THROTTLE_POLLING_TIMER, String.valueOf(((AspectSEThrottlingEndpoint)endpoint).getThrottlePollTimer()));
        configObj.setProperties(AspectConstants.THROTTLE_RATE, String.valueOf(((AspectSEThrottlingEndpoint)endpoint).getThrottleRate()));
        configObj.setProperties(AspectConstants.MAX_THROTTLE_QUEUE_SIZE, String.valueOf(((AspectSEThrottlingEndpoint)endpoint).getMaxThrottleQueueSize()));
        super.save();
    }    
    
}
