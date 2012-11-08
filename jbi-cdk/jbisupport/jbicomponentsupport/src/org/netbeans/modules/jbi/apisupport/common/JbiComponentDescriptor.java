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

/**
 *
 * @author chikkala
 */
public interface JbiComponentDescriptor extends JbiDescriptor {
    
    public static final String TYPE_ATTR = "type";
    public static final String COMPONENT_CL_DELEGATION_ATTR = "component-class-loader-delegation";
    public static final String BOOTSTRAP_CL_DELEGATION_ATTR = "bootstrap-class-loader-delegation";
    
    /**
     * Getter for property type.
     * @return Value of property type.
     */
    public String getType();
    /**
     * Setter for property type.
     * @param type New value of property type.
     */
    public void setType(java.lang.String type);

    /**
     * Getter for property ComponentCLDelegation.
     * @return Value of property ComponentCLDelegation.
     */
    public String getComponentCLDelegation();
    /**
     * Setter for property ComponentCLDelegation.
     * @param type New value of property ComponentCLDelegation.
     */
    public void setComponentCLDelegation(String clDelegation);
    
    /**
     * Getter for property BootstrapCLDelegation.
     * @return Value of property BootstrapCLDelegation.
     */
    public String getBootstrapCLDelegation();
    /**
     * Setter for property BootstrapCLDelegation.
     * @param type New value of property BootstrapCLDelegation.
     */
    public void setBootstrapCLDelegation(String clDelegation);
    
    /**
     * Getter for property componentClassName.
     * @return Value of property componentClassName.
     */
    public String getComponentClassName();
    
    /**
     * Setter for property componentClassName.
     * @param componentClassName New value of property componentClassName.
     */
    public void setComponentClassName(String componentClassName);
    
    /**
     * Getter for property componentClassPath.
     * @return Value of property componentClassPathElements.
     */
    public String[] getComponentClassPath();
    
    /**
     * Setter for property componentClassPath.
     * @param componentClassPathElements New value of property componentClassPath.
     */
    public void setComponentClassPath(String[] componentClassPath);
    
    /**
     * Getter for property bootstrapClassName.
     * @return Value of property bootstrapClassName.
     */
    public String getBootstrapClassName();
    
    /**
     * Setter for property bootstrapClassName.
     * @param bootstrapClassName New value of property bootstrapClassName.
     */
    public void setBootstrapClassName(String bootstrapClassName);
        
    /**
     * Getter for property bootstrapClassPath.
     * @return Value of property bootstrapClassPath.
     */
    public String[] getBootstrapClassPath();
    
    /**
     * Setter for property bootstrapClassPath.
     * @param bootstrapClassPathElements New value of property bootstrapClassPath.
     */
    public void setBootstrapClassPath(String[] bootstrapClassPath);

    /**
     * Getter for property sharedLibraries.
     * @return Value of property sharedLibraries.
     */
    public String[] getSharedLibraryNames();
    
    /**
     * Setter for property sharedLibraries.
     * @param bootstrapClassPathElements New value of property sharedLibraries.
     */
    public void setSharedLibraryNames(String[] sharedLibraries);
    
}
