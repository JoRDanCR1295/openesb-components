/*
 * AspectSETeeEndpointHandler.java
 *
 * Created on February 8, 2007, 12:29 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
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
public class AspectSETeeEndpointHandler extends AspectSEEndpointHandler {
    
    /** Creates a new instance of AspectSETeeEndpointHandler */
    public AspectSETeeEndpointHandler(AspectSEEndpoint endPt) {
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
        }else{
        	Ruleset ruleset = new Ruleset(new ArrayList<Rule>(), "",endpoint.getInvokes());
            endpoint.setRuleset(ruleset);
        }
    }
    
    @Override
    public void save() {
        configObj = null;
        super.save();
    }
}
