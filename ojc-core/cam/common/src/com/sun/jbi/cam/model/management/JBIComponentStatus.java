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
 * @(#)JBIComponentStatus.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.management;
import java.io.Serializable;
import java.util.logging.Logger;

/**
 * @author ylee
 * @author Graj
 *
 */
public class JBIComponentStatus implements Serializable {

    /** Deployment Type  */
    public static final String DEPLOYMENT_TYPE = "service-assembly";
//    public static final String DEPLOYMENT_TYPE = "Deployment";
    /** unknown type */
    public static final String UNKNOWN_TYPE = "unknown";
    /** Binding type  */
    public static final String BINDING_TYPE = "binding-component";
//    public static final String BINDING_TYPE = "Binding";
    /** Engine Type */
    public static final String ENGINE_TYPE = "service-engine";
//    public static final String ENGINE_TYPE = "Engine";
    /** Namespace Type  */
    public static final String NAMESPACE_TYPE = "shared-library";
//    public static final String NAMESPACE_TYPE = "SharedLibrary";

    /** state  Loaded state.  */
    public static final String UNKNOWN_STATE = "Unknown";
    /** state loaded */
    public static final String LOADED_STATE = "Loaded";
    /** Installed state */
//    public static final String INSTALLED_STATE = "Installed";
    public static final String INSTALLED_STATE = "Shutdown";
    /** Stopped state  */
    public static final String STOPPED_STATE = "Stopped";
    /** Started state */
    public static final String STARTED_STATE = "Started";

    protected String componentId;
    protected String state;
    protected String name;
    protected String description;
    protected String type;
    
    private Logger logger = Logger.getLogger(JBIComponentStatus.class.getName());

    /**
     *
     */
    public JBIComponentStatus() {
    }


    /**
     * @param componentId
     * @param state
     * @param name
     * @param description
     * @param type
     */
    public JBIComponentStatus(String componentId, String name, String description, String type, String state) {
        this.componentId = componentId;
        this.name = name;
        this.description = description;
        this.type = type;
        this.state = state;
    }
    
    
    /**
     * @return Returns the componentId.
     */
    public String getComponentId() {
        return this.componentId;
    }
    
    /**
     * @param componentId The componentId to set.
     */
    public void setComponentId(String componentId) {
        this.componentId = componentId;
    }
    
    /**
     * @return Returns the description.
     */
    public String getDescription() {
        return this.description;
    }
    
    /**
     * @param description The description to set.
     */
    public void setDescription(String description) {
        if((description != null) && (description.length() > 0)) {
            this.description = description;
        }
    }
    
    /**
     * @return Returns the name.
     */
    public String getName() {
        return this.name;
    }
    
    /**
     * @param name The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }
    
    /**
     * @return Returns the state.
     */
    public String getState() {
        return this.state;
    }
    
    /**
     * @param state The state to set.
     */
    public void setState(String status) {
        this.state = status;
    }


    /**
     * @return Returns the type.
     */
    public String getType() {
        return this.type;
    }
    /**
     * @param type The type to set.
     */
    public void setType(String type) {
        this.type = type;
    }

    public void dump() {
        logger.info("/////////////////////////////////////////////////");
        logger.info("//  -- JBI Component --                        //");
        logger.info("/////////////////////////////////////////////////");
        //logger.info("//  componentId is: "+ this.componentId);
        logger.info("//  name is: "+ this.name);
        logger.info("//  description is: "+ this.description);
        logger.info("//  type is: "+ this.type);
        logger.info("//  state is: "+ this.state);
        logger.info("/////////////////////////////////////////////////");
    }

    public static void main(String[] args) {
    }
}
