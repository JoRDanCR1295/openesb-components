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

package org.netbeans.modules.jbi.apisupport.project.wizard;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author chikkala
 */
public class WSDLExtInfo {
    
    public static final String JMX_WSDL_EXT_NS = "http://java.sun.com/jbi/wsdl-extensions/sample/jmx-bc/";
    
    public static final String SAMPLE_BC_NAME = "BindingComponent";
    public static final String DEF_NS_BEGIN = "http://schemas.samples.com/jbi/wsdl-extensions/";
    
    public static final String DEF_WSDL_EXT_XSD_TEMPLATE = "codegen/components/binding/plugin/bc-wsdl-ext.xsd";
    public static final String JMX_WSDL_EXT_XSD_TEMPLATE = "codegen/components/binding/plugin/jmx-bc-wsdl-ext.xsd";
    
    public static final String DEF_WSDL_EXT_XSD_FILE_SUFFIX = "WsdlExt";

    public static final String DEF_WSDL_EXT_TEMPLATE_XML_TEMPLATE = "codegen/components/binding/plugin/ext-template.xml";
    public static final String JMX_WSDL_EXT_TEMPLATE_XML_TEMPLATE = "codegen/components/binding/plugin/jmx-ext-template.xml";
    
    private String mComponentName = SAMPLE_BC_NAME;
    
    private String mNamespacePrefix = null;
    private String mWsdlExtNamespace = null;
    private String mWsdlExtXSDFileName = null;
    private String mWsdlExtXSDFileTemplate = null;

    private String mTemplateXmlTemplate = null;
    
    private boolean mIsJMXExtSchema = false;
    
    /** Creates a new instance of WsdlExtInfo */
    public WSDLExtInfo() {
    }
    
    protected String getComponentName() {
        return this.mComponentName;
    }
    
    protected String getNormalizedComponentName() {
        String compName = this.getComponentName();
        if ( compName == null || compName.trim().length() == 0 ) {
            compName = SAMPLE_BC_NAME;
        }
        compName = compName.replace('.', '_');
        return compName;
    }
    
    public void setComponentName(String compName) {
        this.mComponentName = compName;
    }
    
    public boolean isJMXExtensionSchema() {
        return this.mIsJMXExtSchema;
    }
    public void setJMXExtensionSchema(boolean isJMXExtensionSchema) {
        this.mIsJMXExtSchema = isJMXExtensionSchema;
    }
    
    public String getDefaultNamespacePrefix() {
        String compName = this.getNormalizedComponentName();
        return compName.toLowerCase();
    }
    
    /**
     * Getter for property NamespacePrefix.
     * @return Value of property NamespacePrefix.
     */
    public String getNamespacePrefix() {
        if ( this.mNamespacePrefix == null ) {
            this.mNamespacePrefix = getDefaultNamespacePrefix();
        }
        return this.mNamespacePrefix;
    }
    
    /**
     * Setter for property NamespacePrefix.
     * @param packageName New value of property NamespacePrefix.
     */
    public void setNamespacePrefix(String namespacePrefix) {
        this.mNamespacePrefix = namespacePrefix;
    }
    
    public String getDefaultNamespace() {
        String compName = this.getNormalizedComponentName();
        return DEF_NS_BEGIN + compName + "/";
    }
    
    /**
     * Getter for property WsdlExtNamespace.
     * @return Value of property WsdlExtNamespace.
     */
    public String getNamespace() {
        if ( this.mWsdlExtNamespace == null ) {
            this.mWsdlExtNamespace = getDefaultNamespace();
        }
        return this.mWsdlExtNamespace;
    }
    
    /**
     * Setter for property WsdlExtNamespace.
     * @param packageName New value of property WsdlExtNamespace.
     */
    public void setNamespace(String wsdlExtNamespace) {
        this.mWsdlExtNamespace = wsdlExtNamespace;
    }
    
    public String getDefaultXSDFileName() {
        String compName = this.getNormalizedComponentName();
        return compName + DEF_WSDL_EXT_XSD_FILE_SUFFIX;
    }
    
    /**
     * Getter for property WsdlExtFileName.
     * @return Value of property WsdlExtFileName.
     */
    public String getXSDFileName() {
        if ( this.mWsdlExtXSDFileName == null ) {
            this.mWsdlExtXSDFileName = this.getDefaultXSDFileName();
        }
        return this.mWsdlExtXSDFileName;
    }
    
    /**
     * Setter for property WsdlExtFileName.
     * @param packageName New value of property WsdlExtFileName.
     */
    public void setXSDFileName(String wsdlExtXSDFileName) {
        this.mWsdlExtXSDFileName = wsdlExtXSDFileName;
    }
        
    /**
     * Getter for property WsdlExtFileTemplate.
     * @return Value of property WsdlExtFileTemplate.
     */
    public String getXSDFileTemplate() {
        if ( this.mWsdlExtXSDFileTemplate == null ) {
            
            if ( this.isJMXExtensionSchema()) {
                this.mWsdlExtXSDFileTemplate = JMX_WSDL_EXT_XSD_TEMPLATE;
            } else {
                this.mWsdlExtXSDFileTemplate = DEF_WSDL_EXT_XSD_TEMPLATE;
            }
        }
        return this.mWsdlExtXSDFileTemplate;
    }
    
    /**
     * Setter for property WsdlExtFileTemplate.
     * @param packageName New value of property WsdlExtFileTemplate.
     */
    public void setXSDFileTemplate(String wsdlExtXSDFileTemplate) {
        this.mWsdlExtXSDFileTemplate = wsdlExtXSDFileTemplate;
    }
    
    public String getTemplateXMLTemplate() {
        if ( this.mTemplateXmlTemplate == null ) {
            if ( this.isJMXExtensionSchema()) {
                this.mTemplateXmlTemplate = JMX_WSDL_EXT_TEMPLATE_XML_TEMPLATE;
            } else {
                this.mTemplateXmlTemplate = DEF_WSDL_EXT_TEMPLATE_XML_TEMPLATE;
            }
        }
        return this.mTemplateXmlTemplate;
    }

    public void setTemplateXMLTemplate(String templateXmlTemplate) {
        this.mTemplateXmlTemplate = templateXmlTemplate;
    }

    public Map getWsdlExtXSDFileTokenMap() {
        String namespace = this.getNamespace();
        String prefix = this.getNamespacePrefix();
        Map<String, String> tokenMap = new HashMap<String, String>();
        tokenMap.put("WSDL_EXT_NAMESPACE", namespace);
        tokenMap.put("WSDL_EXT_NS_PREFIX", prefix);
        return tokenMap;
    }
    
    public static String getNormalizedComponentName(String compName) {
        String normalizedName = compName;
        if ( normalizedName == null || normalizedName.trim().length() == 0 ) {
            normalizedName = SAMPLE_BC_NAME;
        }
        normalizedName = normalizedName.replace('.', '_');
        return normalizedName;
    }
    
    public static String getNamespacePrefix(String compName) {
        return getNormalizedComponentName(compName).toLowerCase();
    }
    
    public static String getNamespace(String compName) {
        return DEF_NS_BEGIN + getNormalizedComponentName(compName) + "/";   
    }
        
}
