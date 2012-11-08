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
 * @(#)MasterTestProperties.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Properties;

public class MasterTestProperties {
	public LinkedHashMap deploys = new LinkedHashMap ();
    public LinkedHashMap sends = new LinkedHashMap ();
	public int testInterval;
	public File baseDir;
	
	public void load (File propertyFile) throws Exception {
		Properties props = Utility.loadProperties(propertyFile);
		baseDir = propertyFile.getParentFile();
		String testIntervalStr = props.getProperty("test.interval");
		testInterval = Integer.parseInt(testIntervalStr);
		String deployCaseStrs = props.getProperty("test.deploy.case");
        String msgCaseStrs = props.getProperty("test.message.case");
        loadTests (deploys, deployCaseStrs);
        loadTests (sends, msgCaseStrs);
    }
    
    private void loadTests (LinkedHashMap tests, String toParse) throws Exception{
		boolean allCases = false;
		if (toParse.equalsIgnoreCase("all")) {
			allCases = true;
		}
		List caseLists = new ArrayList ();
		if (allCases) {
			File [] allFiles = baseDir.listFiles();
			for (int i=0; i<allFiles.length; i++) {
				File child = allFiles[i];
				if (child.isDirectory()) {
					File [] childProps = child.listFiles(new FilenameFilter() {
						public boolean accept(File dir, String name)  {
							return name.endsWith (".properties");
						}
					});
					if (childProps != null && childProps.length == 1) {
							File childProp = childProps[0];
							TestProperties testProperty = new TestProperties(child.getName());
							testProperty.load(childProp);
							tests.put(testProperty.name, testProperty);
					}
				}
			}
		} else {
			caseLists = Utility.parseString(toParse, ",");
			for (int i=0 ; i<caseLists.size(); i++) {
				File child = new File (baseDir, (String) caseLists.get(i));
				if (child.isDirectory()) {
					File [] childProps = child.listFiles(new FilenameFilter() {
						public boolean accept(File dir, String name)  {
							return name.endsWith (".properties");
						}
					});
					if (childProps != null && childProps.length == 1) {
							File childProp = childProps[0];
							TestProperties testProperty = new TestProperties(child.getName());
							testProperty.load(childProp);
							tests.put(testProperty.name, testProperty);
					}
				}
			}
		}
		
	}
	

}
