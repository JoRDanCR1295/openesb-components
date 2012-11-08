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
 * @(#)TestProperties.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.File;
import java.util.List;
import java.util.Properties;

public class TestProperties {
	public String name;
	public String testId;
	public String serviceNameSpace;
	public String serviceName;
	public String wsdlFile;
	public List testOutputFile;
	public List testOperations;
	public List testMsgNams;
	public List testInputFiles;
	public List testParts;
	public int testMsgInterval;
	public boolean testDebugDetail;
    public String replyServiceNameSpace;
    public String replyServiceName;
    public String replyOperation;
    public long testWaitInterval = 0;
    public long testWaitTimes = 0;
    public List testVarLineNos;
    public List testVarExpr;
    
	
	public TestProperties (String name) {
		this.name = name;
	}
	
	public void load (File propertyFile) throws Exception {
		Properties props = Utility.loadProperties(propertyFile);
		testId = props.getProperty("test.id");
		
		String testOutputFileStr = props.getProperty("test.output");
		testOutputFile=Utility.parseString(testOutputFileStr, ",");

        replyServiceNameSpace = props.getProperty("test.reply.service.namespace");
        replyServiceName = props.getProperty("test.reply.service.name");
        replyOperation = props.getProperty("test.reply.operation");
		
		serviceNameSpace = props.getProperty("test.service.namespace");
		serviceName=props.getProperty("test.service.name");
		wsdlFile=props.getProperty("test.wsdl.file");
		testMsgInterval=Integer.parseInt(props.getProperty("test.msg.interval"));
		
		String testOps = props.getProperty("test.operation.name");
		testOperations = Utility.parseString(testOps, ",");
		
		String testMsgs = props.getProperty("test.msg.name");
		testMsgNams = Utility.parseString(testMsgs, ",");
		
		String testInputs = props.getProperty("test.input.file");
		testInputFiles = Utility.parseString(testInputs, ",");
		
		String testPartsStr = props.getProperty("test.part.name");
		testParts = Utility.parseString(testPartsStr, ",");
		
		String testDebugDetailStr = props.getProperty("test.print.detail");
		testDebugDetail=Boolean.parseBoolean(testDebugDetailStr);
		
        String testWaitIntervalStr= props.getProperty("test.wait.interval");
        if (testWaitIntervalStr != null && testWaitIntervalStr.trim().length() > 0) {
            testWaitInterval=Long.parseLong(testWaitIntervalStr);
        }
        String testWaitTimesStr= props.getProperty("test.wait.times");
        if (testWaitTimesStr != null && testWaitTimesStr.trim().length() > 0) {
            testWaitTimes=Long.parseLong(testWaitTimesStr);
        }
        
        String testVarLineNosStr = props.getProperty("test.var.lineno");
        testVarLineNos = Utility.parseString(testVarLineNosStr, ",");
        
        String testVarExprStr = props.getProperty(("test.var.expression"));
        testVarExpr = Utility.parseString(testVarExprStr, ",");
        
	}
	
	

}
