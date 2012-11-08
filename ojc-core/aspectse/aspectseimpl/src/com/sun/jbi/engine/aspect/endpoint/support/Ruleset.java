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
 * @(#)Ruleset.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;

/**
 * 
 * This class contains a set of rule for a given aspect or advice.
 * 
 * @author Sujit Biswas
 * 
 */
public class Ruleset {

	private String name;

	private List<Rule> ruleList;

	private List<AspectSEEndpoint> invokes;

	public Ruleset(List<Rule> rules, String name, List<AspectSEEndpoint> invokes) {
		ruleList = rules;
		this.name = name;
		this.invokes = invokes;
	}

	/**
	 * This iterates over the set of rules.
	 * 
	 * @param source
	 * @return List<String> destinations
	 */
	public List<AspectSEEndpoint> evaluate(DOMSource source) {
		List<AspectSEEndpoint> destinations = new ArrayList<AspectSEEndpoint>();
		Iterator it = ruleList.iterator();
		while (it.hasNext()) {
			Rule rule = (Rule) it.next();

			// use the first rule which matches
			if (!rule.evaluate(source).isEmpty()) {
				destinations.addAll(rule.evaluate(source));
				break;
			}
		}

		if (destinations.size() == 0) {
			// this is the default rule i.e no rule matches the message should
			// be send to all the targets

			return invokes;
		}
		return destinations;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the ruleList
	 */
	public List<Rule> getRuleList() {
		return ruleList;
	}
}
