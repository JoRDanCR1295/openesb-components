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
 * @(#)ComponentConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.management.DeploymentException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Identification;
import com.sun.jbi.common.descriptor.parsers.ServiceAssemblyParser;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;

/**
 * Represents a component's custom configuration, as defined by Sun's JBI
 * Systemic Requirements.
 *
 * @author Kevan Simpson
 */
public class ComponentConfig extends AbstractConfig {
    private static Logger mLogger =
            Logger.getLogger(ComponentConfig.class.getName());

    /* Component name. */
    private Identification mId;
    /* Application configuration Property instances, by name. */
    private Property[] mConfigMetadata;
//    private Map<String, Property> mConfigMetadata;
    /* Application configuration instances, by name. */
    private Map<String, AppConfig> mAppConfig;
    /* Application variables instances, by name. */
    private Map<String, AppVar> mAppVars;
    
    ComponentConfig() { // package access, for use by ConfigParser
    }

    /** @see com.sun.jbi.common.qos.config.AbstractConfig#getName() */
    public String getName() {
        return mId.getName();
    }

    void setId(Identification id) {
        mId = id;
    }

    /**
     * Fetches iteration of application configuration property names.
     * @return iteration of application configuration property names.
     * @throws UnsupportedOperationException if application configurations are not supported.
     */
    public Iterator<String> appConfigNames() {
        checkAppConfigSupport();
        return mAppConfig.keySet().iterator();
    }

    /**
     * Fetches iteration of application configuration property names.
     * @return iteration of application configuration property names.
     * @throws UnsupportedOperationException if application variables are not supported.
     */
    public Iterator<String> appVarNames() {
        checkAppVarSupport();
        return mAppVars.keySet().iterator();
    }

    /**
     * Fetches an application configuration instance by name, 
     * which may be <code>null</code>.
     *
     * @param name The configuration's name.
     * @return A <code>AppConfig</code> or <code>null</code>.
     * @throws UnsupportedOperationException if application configurations are not supported.
     */
    public AppConfig getAppConfig(String name) {
        checkAppConfigSupport();
        return mAppConfig.get(name);
    }

    /**
     * Returns a non-<code>null</code> array of {@link Property} instances 
     * representing the defined Application Configuration for this component.
     * 
     * @return a non-<code>null</code> array of application configuration properties.
     */
    public Property[] getAppConfigDefs() {
        if (mConfigMetadata == null || mConfigMetadata.length == 0) {
            return new Property[0];
        }
        
        int len = mConfigMetadata.length;
        Property[] copy = new Property[len];
        System.arraycopy(mConfigMetadata, 0, copy, 0, len);
        return copy;
    }

    /**
     * Fetches an application variable by name, which may be <code>null</code>.
     * 
     * @param name The variable name.
     * @return an <code>AppVar</code> or <code>null</code>.
     * @throws UnsupportedOperationException if application variables are not supported.
     */
    public AppVar getAppVar(String name) {
        checkAppVarSupport();
        return mAppVars.get(name);
    }
    
    /**
     * Adds the specified {@link AppConfig}.
     * @param config A new <code>AppConfig</code>.
     * @throws UnsupportedOperationException if application configurations are not supported.
     */
    public void putAppConfig(AppConfig config) {
        checkAppConfigSupport();
        if (config != null) {
            mAppConfig.put(config.getName(), config);
        }
    }

    /**
     * Adds the specified {@link AppVar} to the component.
     * @param var an application variable.
     * @throws UnsupportedOperationException if this component does not support
     *                                       application variables.
     */
    public void putAppVar(AppVar var) {
        checkAppVarSupport();
        if (var != null) {
            mAppVars.put(var.getName(), var);
        }
    }

    public boolean supportsAppConfigs() {
        return mAppConfig != null;
    }
    
    public boolean supportsAppVars() {
        return mAppVars != null;
    }
    
    void initializeAppConfig(Property[] metadata) {
        mAppConfig = new TreeMap<String, AppConfig>();
        mConfigMetadata = metadata;//new TreeMap<String, Property>();
    }
    void initializeAppVars() {
        mAppVars = new TreeMap<String, AppVar>();
    }
    
    /**
     * If application variables are not supported, an {@link UnsupportedOperationException} is thrown.
     */
    private void checkAppConfigSupport() {
        if (mAppConfig == null) {
            throw new UnsupportedOperationException(I18n.loc(
                    "QOS-6057: {0} does not support Application Configurations!",
                    this.getName()));
        }
    }
    /**
     * If application variables are not supported, an {@link UnsupportedOperationException} is thrown.
     */
    private void checkAppVarSupport() {
        if (mAppVars == null) {
            throw new UnsupportedOperationException(I18n.loc(
                    "QOS-6011: {0} does not support Application Variables!",
                    this.getName()));
        }
    }

    /**
     * Removes the named application configuration.
     *
     * @param name The configuration instance name.
     * @return the stored <code>AppConfig</code> or <code>null</code>.
     */
    public AppConfig removeAppConfig(String name) {
        return (name == null) ? null : mAppConfig.remove(name);
    }

    /**
     * Removes the named application variable.
     *
     * @param name The variable name.
     * @return the stored <code>AppVar</code> or <code>null</code>.
     */
    public AppVar removeAppVar(String name) {
        checkAppVarSupport();
        return (name == null) ? null : mAppVars.remove(name);
    }

    /** Builds a property key string. */
    String bld(String... str) { // package access, for use by ConfigParser
        StringBuffer buff = new StringBuffer();
        buff.append(str[0]);
        for (int i = 1, n = str.length; i < n; i++) {
            buff.append(".").append(str[i]);
        }
        return buff.toString();
    }

    /**
     * Parses a JBI component descriptor, residing in the specified workspace,
     * for configuration properties.
     * 
     * @param installRoot Installation root of the component.
     * @return a <code>ComponentConfig</code> instance.
     * @throws DeploymentException if an error occurs parsing the descriptor.
     */
    public static ComponentConfig parse(String installRoot) throws DeploymentException {
        FileInputStream stream = null;
        try {
            stream = new FileInputStream(new File(
                    new File(installRoot, JbiDescriptor.META_INF_DIR), 
                    JbiDescriptor.JBI_DESC_FILE_NAME));
            return parse(new InputSource(stream));
        }
        catch (FileNotFoundException fnfe) {
            throw new DeploymentException(I18n.loc(
                    "QOS-6012: Component's JBI descriptor not found: ", fnfe.getMessage()),
                    fnfe);
        }
        finally {
            if (stream != null) {
                try { stream.close(); }
                catch (Exception e) { /* ignore */ }
            }
        }
    }

    /**
     * Parses a JBI component descriptor for configuration properties.
     *
     * @param source The source of the descriptor.
     * @return a <code>ComponentConfig</code> instance.
     * @throws DeploymentException if an error occurs parsing the descriptor.
     */
    public static ComponentConfig parse(InputSource source) throws DeploymentException {
        try {
        	Document doc = XmlUtil.readXml(source);
        	if (doc == null) {
        		return null;
        	}
        	
            ConfigParser qosParser = new ConfigParser(); // holds state
            ServiceAssemblyParser idParser = new ServiceAssemblyParser(qosParser);

            ComponentConfig cfg = qosParser.parse(doc.getDocumentElement());
            // id - get parent element first: component
            NodeList list = doc.getDocumentElement().getElementsByTagNameNS(
            		JbiDescriptor.JBI_NS, JbiDescriptor.COMPONENT_ELEM);
            if (list == null || list.getLength() != 1) {
            	cfg.setId(new Identification("Unidentified Component",
            			"This component's descriptor did not contain an Identification configuration."));
            }
            else {
            	cfg.setId(idParser.parseIdentification((Element) list.item(0)));
            }
            return cfg;
        }
        catch (DeploymentException de) {
        	throw de;
        }
        catch (Exception e) {
            throw error(I18n.loc("QOS-6005: Failed to parse component configuration: {0}",
                                 e.getMessage()),
                        e);
        }
    }

    public static Integer toInteger(String str, Integer defaultValue) {
        try {
            return (Util.isEmpty(str)) ? defaultValue : Integer.valueOf(str);
        }
        catch (Exception e) {
            mLogger.fine(I18n.loc("QOS-3001: Failed to parse Integer: {0}", str));
            return defaultValue;
        }
    }
    
    /** Returns a logged and throwable <code>DeploymentException</code>. */ 
    private static DeploymentException error(String message, Exception thrown) {
        if (thrown == null) {
            mLogger.warning(message);
            return new DeploymentException(message);
        }
        else {
            mLogger.log(Level.WARNING, message, thrown);
            return new DeploymentException(message, thrown);
        }
    }
}
