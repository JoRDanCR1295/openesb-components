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
 * @(#)ConfigParser.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.util.ArrayList;
import java.util.List;
import javax.jbi.management.DeploymentException;
import javax.xml.xpath.XPathConstants;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.descriptor.parsers.AbstractJbiParser;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.config.Constraint.Facet;
import com.sun.jbi.common.util.Util;

/**
 * Parses component configuration, as defined by Sun's SOA systemic requirements.
 * <p>
 * NOTE: This parser expects that each Property configuration must have a name
 * that is unique throughout the JBI descriptor, regardless of whether or not
 * the Property is defined in a PropertyGroup.
 * 
 * @author Kevan Simpson
 */
public class ConfigParser extends AbstractJbiParser<ComponentConfig> {
    //public static final String CONFIG_NS = "http://www.sun.com/jbi/descriptor/configuration";
    public static final String CONFIG_NS = "http://www.sun.com/jbi/Configuration/V1.0";

    public static final String CONFIGURATION_ELEM   = "Configuration";
    public static final String PROPERTY_ELEM        = "Property";
    public static final String PROPERTY_GROUP_ELEM  = "PropertyGroup";
    public static final String CONSTRAINT_ELEM      = "Constraint";
    public static final String APPL_CONFIG_ELEM     = "ApplicationConfiguration";
    public static final String APPL_VARS_ELEM       = "ApplicationVariable";
    public static final String DEFAULT_VALUE_ATTR   = "defaultValue";
    public static final String MAX_OCCURS_ATTR      = "maxOccurs";
    public static final String REQUIRED_ATTR        = "required";
    public static final String TYPE_ATTR            = "type";
    public static final String NAME_ATTR            = "name";
    public static final String FACET_ATTR           = "facet";
    public static final String VALUE_ATTR           = "value";

	private static final String CFG_PREFIX = "cfg";
	
    private List<Property> mMetadata;
    private ComponentConfig mConfig;
    
    public ConfigParser() {
    	mConfig = new ComponentConfig();
    	getNSContext().addNamespace(CFG_PREFIX, CONFIG_NS);
    }
    
    /** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
	public ComponentConfig parse(Element elem) throws DeploymentException {
		try {
			Element cfg = (Element) getXPath().evaluate(
					build("//", CFG_PREFIX, ":", CONFIGURATION_ELEM), 
					elem, XPathConstants.NODE);
			if (cfg != null) {
				NodeList list = cfg.getElementsByTagNameNS(
						CONFIG_NS, PROPERTY_GROUP_ELEM);
				// property groups
				for (int i = 0, n = list.getLength(); i < n; i++) {
					Element grp = (Element) list.item(i);
					String nm = grp.getAttribute(NAME_ATTR);
					NodeList propList = grp.getElementsByTagNameNS(
							CONFIG_NS, PROPERTY_ELEM);
					for (int j = 0, J = propList.getLength(); j < J; j++) {
						Property p = parseProperty((Element) propList.item(j));
						p.setGroup(nm);
						mConfig.addProperty(p);
					}
				}
				
				// properties
				list = cfg.getElementsByTagNameNS(CONFIG_NS, PROPERTY_ELEM);
				for (int j = 0, J = list.getLength(); j < J; j++) {
					Property p = parseProperty((Element) list.item(j));
					mConfig.addProperty(p);
				}
				
				// app config
				Element ac = (Element) getXPath().evaluate(
						build("./", CFG_PREFIX, ":", APPL_CONFIG_ELEM), 
						cfg, XPathConstants.NODE);
				if (ac != null) {
					list = ac.getElementsByTagNameNS(CONFIG_NS, PROPERTY_ELEM);
					mMetadata = new ArrayList<Property>();
					for (int j = 0, J = list.getLength(); j < J; j++) {
						Property p = parseProperty((Element) list.item(j));
						mMetadata.add(p);
					}					

					int len = mMetadata.size();
		            Property[] props = new Property[len];
		            mMetadata.toArray(props);
		            mConfig.initializeAppConfig(props);
				}
				
				// app vars
				if (getXPath().evaluate(	// presence of appvar element is enough...
						build("./", CFG_PREFIX, ":", APPL_VARS_ELEM), 
						cfg, XPathConstants.NODE) != null) {
					mConfig.initializeAppVars();
				}
			}
		}
		catch (Exception e) {
			throw error(e, I18n.loc(
					"QOS-6005: Failed to parse component configuration: {0}",
					e.getMessage()));
		}

		return mConfig;
	}

    protected Property parseProperty(Element elem) throws DeploymentException {
        String prop = elem.getAttribute(NAME_ATTR),
               value = elem.getAttribute(DEFAULT_VALUE_ATTR),
               rpts = elem.getAttribute(MAX_OCCURS_ATTR),
               req = elem.getAttribute(REQUIRED_ATTR);
        // TODO add info for additional attributes (e.g. displayName, etc.)
        if (Util.isEmpty(prop)) {   // name is required
            throw error(null, I18n.loc("QOS-6064: Property name is required!"));
        }
        
        // create property
        Property p = new Property(prop, value);
        if (!Util.isEmpty(rpts)) {  // repeating?
            p.setMaxOccurs(Util.parseInt(rpts, 1));
        }
        // set and validate type
        try { p.setType(resolveQName(elem, TYPE_ATTR)); }  // will be validated
        catch (Exception e) { throw error(e, e.getMessage()); }
        // required?
        p.setRequired(!Util.isEmpty(req) || Boolean.valueOf(req).booleanValue());
        
        // constraints
        NodeList list = elem.getElementsByTagNameNS(CONFIG_NS, CONSTRAINT_ELEM);
        if (list != null) {
        	// add and validate
            for (int i = 0, I = list.getLength(); i < I; i++) {
                Constraint con = parseConstraint((Element) list.item(i));
                p.addConstraints(con);
            }
            
            try {
                p.validateConstraints();
            }
            catch (Exception e) {
                throw error(e, I18n.loc(
                        "QOS-6075: Invalid Constraints on Property {0} were specified: {1}", 
                        p.getName(), e.getMessage()));
            }
        }
        
        return p;
    }

    protected Constraint parseConstraint(Element elem) throws DeploymentException {
        String facet = elem.getAttribute(FACET_ATTR), 
               value = elem.getAttribute(VALUE_ATTR);
        // validate
        if (Util.isEmpty(facet)) {
            throw error(null, I18n.loc(
                    "QOS-6066: Constraint {0} is missing required attribute: {1}", 
                    "???", FACET_ATTR));
            
        }
        else if (value == null) {
            throw error(null, I18n.loc(
                    "QOS-6066: Constraint {0} is missing required attribute: {1}", 
                    facet, VALUE_ATTR));
        }
        else {
            Facet f = null;
            try { f = Facet.valueOf(facet); }
            catch (Exception e) { /* ignore */ }

            if (f == null) {
                throw error(null, I18n.loc("QOS-6067: Undefined Facet: {0}", facet)); 
            }
            
            return new Constraint(f, value);
        }
    }
}
