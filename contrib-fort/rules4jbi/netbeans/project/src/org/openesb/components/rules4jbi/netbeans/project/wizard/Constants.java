/*
 * @(#)Constants.java        $Revision: 1.2 $ $Date: 2008/11/24 12:47:19 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.project.wizard;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/24 12:47:19 $
 * 
 * @since 0.1
 */
public final class Constants {

    public static final String PROPERTY_PROJECT_NAME = "projectName";
    
    public static final String PROPERTY_PROJECT_LOCATION = "projectLocation";
    
    public static final String PROPERTY_PROJECT_FOLDER = "projectFolder";
    
    /*
     * Value of this property is mandatory. It is used internally
     * by the new project wizard to determine if the newly created
     * project should be set as main. 
     */
    public static final String PROPERTY_SET_AS_MAIN = "setAsMain";
    
    public static final String PROPERTY_PROVIDER_URI = "providerUri";
    
    public static final String PROPERTY_PROVIDER_CLASS = "providerClass";

    
    public static final String DEFAULT_PROJECT_NAME = "RulesModule";

    
    private Constants() {}
}
