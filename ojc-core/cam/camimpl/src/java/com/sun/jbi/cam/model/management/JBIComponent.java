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
 * @(#)JBIComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.management;

import java.io.Serializable;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.management.ObjectName;

/**
 * @author ylee
 * @author Graj
 *
 */
public class JBIComponent implements Serializable {

    private static final String COMPONENT_NAME = "ComponentName";
    private static final String COMPONENT_ID = "ComponentId";
    private static final String CONTROL_TYPE = "ControlType";
    private static final String COMPONENT_TYPE = "ComponentType";
    private static final String INSTALLED_TYPE = "InstalledType";

    protected String componentName; // STCHttpSoapBinding
    protected String componentId;   // stchttpsoap-b393-4f54-aaad-9294ae9c5a23
    protected String controlType;   // Lifecycle
    protected String componentType; // Installed
    protected String installedType; // Binding
    
    private Logger logger = Logger.getLogger(JBIComponent.class.getName());
    

    /**
     *
     */
    public JBIComponent() {
    }

    public JBIComponent(ObjectName objectName) {
        this.populateMetadata(objectName);
    }

    private void populateMetadata(ObjectName objectName) {
        Hashtable properties = objectName.getKeyPropertyList();
        this.componentId = (String)properties.get(JBIComponent.COMPONENT_ID);
        this.componentName = (String)properties.get(JBIComponent.COMPONENT_NAME);
        this.componentType = (String)properties.get(JBIComponent.COMPONENT_TYPE);
        this.controlType = (String)properties.get(JBIComponent.CONTROL_TYPE);
        this.installedType = (String)properties.get(JBIComponent.INSTALLED_TYPE);
        //this.printOut();
    }

    public void printOut() {
        logger.info("// ComponentId = "+this.componentId);
        logger.info("// ComponentName = "+this.componentName);
        logger.info("// ComponentType = "+this.componentType);
        logger.info("// ControlType = "+this.controlType);
        logger.info("// InstalledType = "+this.installedType);
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
     * @return Returns the componentName.
     */
    public String getComponentName() {
        return this.componentName;
    }
    /**
     * @param componentName The componentName to set.
     */
    public void setComponentName(String componentName) {
        this.componentName = componentName;
    }
    /**
     * @return Returns the componentType.
     */
    public String getComponentType() {
        return this.componentType;
    }
    /**
     * @param componentType The componentType to set.
     */
    public void setComponentType(String componentType) {
        this.componentType = componentType;
    }
    /**
     * @return Returns the controlType.
     */
    public String getControlType() {
        return this.controlType;
    }
    /**
     * @param controlType The controlType to set.
     */
    public void setControlType(String controlType) {
        this.controlType = controlType;
    }
    /**
     * @return Returns the installedType.
     */
    public String getInstalledType() {
        return this.installedType;
    }
    /**
     * @param installedType The installedType to set.
     */
    public void setInstalledType(String installedType) {
        this.installedType = installedType;
    }
    public static void main(String[] args) {
    }
}
