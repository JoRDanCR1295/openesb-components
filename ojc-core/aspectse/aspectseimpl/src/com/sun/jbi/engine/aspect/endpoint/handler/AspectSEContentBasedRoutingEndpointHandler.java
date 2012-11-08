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
 * @(#)AspectSEContentBasedRoutingEndpointHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.handler;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;
import com.sun.jbi.engine.aspect.endpoint.support.Rule;
import com.sun.jbi.engine.aspect.endpoint.support.Ruleset;
import com.sun.jbi.engine.aspect.utils.AspectSEUtil;
import com.sun.jbi.engine.aspect.utils.XMLFile;
/**
 *
 * @author karthikeyan s
 */
public class AspectSEContentBasedRoutingEndpointHandler extends AspectSEEndpointHandler {
    
    /** Creates a new instance of AspectSEContentBasedRoutingEndpointHandler */
    public AspectSEContentBasedRoutingEndpointHandler(AspectSEEndpoint endPt) {
        super(endPt);
    }
    
    protected void parseAdvice() {
        // use the rootpath and filename to get the config file.
        File confFile = new File(rootPath, configFile);
        if(confFile.exists()) {
            // use xmlutil to get rules tags. then create ruleset for them.
            XMLFile util = new XMLFile(confFile);
            NodeList rules = util.getElementByTagName(AspectConstants.RULE_TAG);
            List<Rule> rulesList = new ArrayList<Rule>();
            for(int i = 0; i < rules.getLength(); i++) {
                List<AspectSEEndpoint> dests = new ArrayList<AspectSEEndpoint>();
                Element rule = (Element)rules.item(i);
                NodeList destinations = rule.getElementsByTagName(AspectConstants.RULE_TAG_DESTINATION);
                for(int j = 0; j < destinations.getLength(); j++) {
                    Element dest = (Element)destinations.item(j);
                    String id = dest.getAttribute(AspectConstants.RULE_TAG_DESTINATION_ATTR);
                    AspectSEEndpoint output = AspectSEUtil.getOutputForID(endpoint, id);
                    dests.add(output);
                }
                Rule ruleObj = new Rule(rule, dests);
                rulesList.add(ruleObj);
            }
            Element rulesetElem = (Element)util.getElementByTagName(AspectConstants.RULESET_TAG).item(0);
            String name = rulesetElem.getAttribute(AspectConstants.PROPERTY_ATTR_NAME);
            Ruleset ruleset = new Ruleset(rulesList, name,endpoint.getInvokes());
            endpoint.setRuleset(ruleset);
        }
    }
    
    @Override
    public void save() {
        configObj = null;
        super.save();
    }
}
