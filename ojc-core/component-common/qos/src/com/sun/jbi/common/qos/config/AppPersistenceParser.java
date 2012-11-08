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
 * @(#)AppPersistenceParser.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.util.Iterator;

import javax.jbi.management.DeploymentException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.parsers.AbstractJbiParser;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.config.AppVar.VarType;
import com.sun.jbi.common.util.Util;

/**
 * 
 * @author Kevan Simpson
 */
public class AppPersistenceParser extends AbstractJbiParser<ComponentConfig> {
    public static final String APP_ROOT_ELEM    = "app";
    public static final String CONFIG_ELEM      = "config";
    public static final String PROPERTY_ELEM    = "property";
    public static final String VAR_ELEM         = "var";
    public static final String VALUE_ELEM       = "value";
    public static final String NAME_ATTR        = "name";
//    public static final String COUNT_ATTR       = "count";
//    public static final String INDEX_ATTR       = "index";
    public static final String TYPE_ATTR        = "type";
    
    private ComponentConfig mConfig = null;
    
    /**
     * 
     */
    public AppPersistenceParser(ComponentConfig config) {
        mConfig = config;
    }
    
    /** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
	public ComponentConfig parse(Element elem) throws DeploymentException {
		try {
			NodeList acList = elem.getElementsByTagName(CONFIG_ELEM);
			if (acList != null) {
				for (int i = 0, n = acList.getLength(); i < n; i++) {
					/*
					 * <config>
					 * 		<property name="">
					 * 			<value>txt</value>
					 * 			<value>txt</value>
					 * 
					 */
					Element cfg = (Element) acList.item(i);
					String cfgNm = getAttrValue(cfg, NAME_ATTR);
					NodeList props = cfg.getElementsByTagName(PROPERTY_ELEM);
					AppConfig ac = mConfig.getAppConfig(cfgNm);
					if (ac == null) {
						ac = new AppConfig(cfgNm, mConfig.getAppConfigDefs());
						mConfig.putAppConfig(ac);
					}
					
					if (props != null) {
						for (int j = 0, J = props.getLength(); j < J; j++) {
							Element ep = (Element) props.item(j);
							Property p = ac.getProperty(getAttrValue(ep, NAME_ATTR));
							NodeList vals = ep.getElementsByTagName(VALUE_ELEM);
							if (p != null && vals != null) {
								for (int k = 0, K = vals.getLength(); k < K; k++) {
									Element v = (Element) vals.item(k);
									p.addValue(v.getTextContent());
								}
							}
						}
					}
				}
			}
			
			NodeList varList = elem.getElementsByTagName(VAR_ELEM);
			if (varList != null) {
				for (int i = 0, n = varList.getLength(); i < n; i++) {
					Element v = (Element) varList.item(i);
		            AppVar av = new AppVar(getAttrValue(v, NAME_ATTR), 
		            					   v.getTextContent(), 
		            					   VarType.toVarType(getAttrValue(v, TYPE_ATTR)));
		            mConfig.putAppVar(av);
				}
			}
		}
		catch (Exception e) {
		    throw error(e,
	    			I18n.loc("Failed to parse persisted application configuration: {0}",
	    				     e.getMessage()));
		}
		
		return mConfig;
	}

    String getAttrValue(Element elem, String name) throws DeploymentException {
        String val = (elem == null) ? null : elem.getAttribute(name);
        if (Util.isEmpty(val)) {
            throw error(null, I18n.loc(
            		"Missing required attribute: {0}", name));
        }

        return val; 
    }
    
    static String toAppXml(ComponentConfig config) {
        StringBuffer buff = new StringBuffer();
        buff.append("<").append(APP_ROOT_ELEM).append(">\n");
        if (config != null) {
            if (config.supportsAppConfigs()) {
                // app configs
                for (Iterator<String> iter = config.appConfigNames(); iter.hasNext();) {
                    AppConfig ac = config.getAppConfig(iter.next());
                    buff.append("\t<").append(CONFIG_ELEM).append(" ")
                        .append(NAME_ATTR).append("=\"").append(ac.getName())
                        .append("\">\n");
                    for (Property prop : ac.propertySet()) {
                        if (prop.count() <= 0) {
                            continue;   // don't persist value-less properties
                        }
                        
                        buff.append("\t\t<").append(PROPERTY_ELEM).append(" ")
                            .append(NAME_ATTR).append("=\"").append(prop.getName())
                            .append("\">\n");
                        for (int i = 0, n = prop.count(); i < n; i++) {
                            buff.append("\t\t\t<").append(VALUE_ELEM).append(">")
                            // we're not escaping text for now...enclose w/ CDATA ???
                                .append(prop.getValueAt(i)).append("</")
                                .append(VALUE_ELEM).append(">\n");
                        }
                        buff.append("\t\t</").append(PROPERTY_ELEM).append(">\n");
                    }
                    buff.append("\t</").append(CONFIG_ELEM).append(">\n");
                }
            }
            
            if (config.supportsAppVars()) {
                // app vars
                for (Iterator<String> iter = config.appVarNames(); iter.hasNext();) {
                    AppVar var = config.getAppVar(iter.next());
                    buff.append("\t<").append(VAR_ELEM).append(" ").append(NAME_ATTR)
                        .append("=\"").append(var.getName()).append("\" ")
                        .append(TYPE_ATTR).append("=\"").append(var.getType())
                        .append("\">").append(var.getValue()).append("</var>\n");
                    
                }
            }
        }
        buff.append("</").append(APP_ROOT_ELEM).append(">\n");
        return buff.toString();
    }
}
