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
 * @(#)$Id: XpathTest.java,v 1.8 2010/02/15 19:25:04 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.xpath.test;

import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.Random;
import java.util.Set;

import javax.wsdl.Message;

import junit.framework.TestCase;

import org.apache.commons.jxpath.JXPathContext;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory;
import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.impl.CopyUnitImpl;
import com.sun.jbi.engine.workflow.runtime.model.impl.RuntimeVariablesImpl;
import com.sun.jbi.engine.workflow.util.XPathUtil;
import com.sun.jbi.engine.workflow.xpath.ExpressionInfo;
import com.sun.jbi.engine.workflow.xpath.LdapXpathContext;
import com.sun.jbi.workflow.model.ModelFactory;
import com.sun.jbi.workflow.model.RuntimeVariables;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.utl.ModelUtil;

public class XpathTest extends TestCase {
	

	public void testConstants () throws Exception {
        RuntimeVariables vars = new RuntimeVariablesImpl ();
		JXPathContext jxpathContex = XPathUtil.newJXPathContextFromVar(vars, new XPathUtil.DOMFactory ());
		//Differnt combination
        /* 1. $TaskInput.partName1/po:PO
        2. $TaskInput.partName1
        3. $TaskInput/po.PO
        4. $TaskInput
        5. $TaskInput.partName1/po:PO + $TaskInput.partName1/po:PO
        */

        String query = "$TaskInput.partName1/po:PO";
        List<ExpressionInfo>  exprs = XPathUtil.parseExprForVariables(query);
        assertEquals(1, exprs.size());
        ExpressionInfo info = exprs.iterator().next();
        assertEquals("TaskInput", info.getXpathVariableName());
        assertEquals("po:PO", info.getXpathQuery());
        assertEquals("$TaskInput/po:PO", info.getPath());
        assertEquals("$TaskInput.partName1/po:PO", info.toString());
        
        query = "$TaskOutput.partName1/po:PO";
        exprs = XPathUtil.parseExprForVariables(query);
        assertEquals(1, exprs.size());
        info = exprs.iterator().next();
        assertEquals("TaskOutput", info.getXpathVariableName());
        assertEquals("po:PO", info.getXpathQuery());
        
        query = " $TaskInput.partName1";
        exprs = XPathUtil.parseExprForVariables(query);
        assertEquals(1, exprs.size());
        info = exprs.iterator().next();
        assertEquals("TaskInput", info.getXpathVariableName());
        assertEquals(null, info.getXpathQuery());   
        
        query = "$TaskInput/po:PO";
        exprs = XPathUtil.parseExprForVariables(query);
        assertEquals(1, exprs.size());
        info = exprs.iterator().next();
        assertEquals("TaskInput", info.getXpathVariableName());
        assertEquals("po:PO", info.getXpathQuery());           
             
        query = "$TaskInput";
        exprs = XPathUtil.parseExprForVariables(query);
        assertEquals(1, exprs.size());
        info = exprs.iterator().next();
        assertEquals("TaskInput", info.getXpathVariableName());
        assertEquals(null, info.getXpathQuery());         
        
        query = " $TaskInput.partName1/po:PO + $TaskOutput.partName1/po:PO";
        exprs = XPathUtil.parseExprForVariables(query);
        assertEquals(2, exprs.size());
        info = exprs.get(0);
        assertEquals("TaskInput", info.getXpathVariableName());
        assertEquals("po:PO", info.getXpathQuery());              
       
        info = exprs.get (1);
        assertEquals("TaskOutput", info.getXpathVariableName());
        assertEquals("po:PO", info.getXpathQuery());              
      
       
        
		String strVal = XPathUtil.getStringValue(XPathUtil.evaluateExpression("'High'", jxpathContex), ";");
		assertEquals (0,  XPathUtil.getXPathExpressionList ("'High'").size());
		assertEquals ("High", strVal);		
		
		//Number constant
		strVal = XPathUtil.getStringValue(XPathUtil.evaluateExpression("1", jxpathContex), ";");
		assertEquals ("1.0", strVal);
        
        strVal = XPathUtil.getStringValue(XPathUtil.evaluateExpression("$TaskInput", jxpathContex), ";");
        
     
        
	}
	
	public void testCollections () throws Exception {
		String testFile = "data/testCollectionInput.xml";
		Element el = XpathTestUtil.loadInputMessageElement(testFile);
		RuntimeVariables vars = new RuntimeVariablesImpl ();
		vars.declareVariable("TaskPayLoad", el);
		JXPathContext jxpathContex =  XPathUtil.newJXPathContextFromVar(vars, null);
		String strVal = XPathUtil.getStringValue(XPathUtil.evaluateExpression("$TaskPayLoad/users/user", jxpathContex), ";");
		assertEquals ("a;b;c", strVal);	
	}
	
	public void testSetPartsVariables () throws Exception {
		 final String TASK_WSDL_FILE_NAME = "data/ApprovePurchase_TM_BPEL.wsdl";
		 Message msg = XpathTestUtil.loadMesasge(TASK_WSDL_FILE_NAME, "http://jbi.com.sun/wfse/wsdl/WorkflowApp2/ApprovePurchase_TM_BPEL","TaskInput");
		 Element el = XpathTestUtil.loadInputMessageElement("data/Input.xml");
         RuntimeVariables runtimeVars = new RuntimeVariablesImpl ();
		 runtimeVars = XPathUtil.setPartsAsVariables("TaskInput", el, msg, runtimeVars);
//
//		assertEquals (3, runtimeVars.size());
//		Element payLoadEl = (Element) runtimeVars.getVariable("TaskInput.payLoadPart");
//		assertEquals("purchaseOrder", payLoadEl.getLocalName());
//		String priority = (String) runtimeVars.getVariable("TaskInput.priorityPart");
//		assertEquals("High", priority);
//	 
//		String customer = (String) runtimeVars.getVariable ("TaskInput.customerPart");
//		assertEquals("Xerox", customer);
		JXPathContext jxpathContex = XPathUtil.newJXPathContextFromVar(runtimeVars, new XPathUtil.DOMFactory ());
//		
//		String concatStr = XPathUtil.getStringValue(XPathUtil.evaluateExpression("concat('priority is :', $TaskInput.priorityPart)", jxpathContex), ";");
//		assertEquals("priority is :High", concatStr);
		CopyUnitImpl copy = new CopyUnitImpl ();
        Object fromVal = null;
		fromVal =  XPathUtil.evaluateExpression("$TaskInput.payLoadPart", jxpathContex);
        runtimeVars.declareVariable("TaskInput", fromVal);
        ExpressionInfo expression = null;
//		expression = copy.getToExpression("$TaskPayload", jxpathContex);
//		assertNotNull (expression);
//		copy.executeTo(expression, jxpathContex, fromVal);
//		Object newVal = runtimeVars.getVariable ("TaskPayload");
//		assertNotNull(newVal);
//		assertTrue(newVal instanceof Element);		
        
         fromVal =  XPathUtil.evaluateExpression("$TaskInput.payLoadPart/@Accept", jxpathContex);
         assertEquals("true", fromVal);
         expression = copy.getToExpression("$TaskInput.payLoadPart/@Accept", jxpathContex);
         copy.executeTo(expression, jxpathContex, "false");
         fromVal =  XPathUtil.evaluateExpression("$TaskInput.payLoadPart/@Accept", jxpathContex);
         assertEquals("false", fromVal);
	}
    
    public void testLdapfunctions () throws Exception {
        RuntimeVariables runtimeVars = new RuntimeVariablesImpl ();
        runtimeVars.declareVariable(RuntimeVariables.TASK_INSTANCE_OWNER, "john");
        JXPathContext jxpathContex = XPathUtil.newJXPathContextFromVar(runtimeVars, new XPathUtil.DOMFactory ());
        String owner  = XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-task-owner()", jxpathContex), "");
        assertEquals (owner, "john");
        if (((LdapXpathContext) jxpathContex).getLdapConfig().getLDAPContext() != null) {
            String selfEmail = XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-email-ldap()", jxpathContex), "");
            assertEquals("john.smith@abc.com", selfEmail);
            String peopleEmail = XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-email-ldap('mary')", jxpathContex), "");
            assertEquals("mary.ling@abc.com", peopleEmail);
            String selfManagerEmail = XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-manager-email-ldap()", jxpathContex), "");
            assertEquals("dave.cook@abc.com", selfManagerEmail);
            String peopleManagerEmail = XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-manager-email-ldap('mary')", jxpathContex), "");
            assertEquals("dave.cook@abc.com", peopleManagerEmail);
            String selfManagerUID = XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-manager-uid-ldap()", jxpathContex), "");
            assertEquals("dave", selfManagerUID);
            String peopleManagerUID = XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-manager-uid-ldap('mary')", jxpathContex), "");
            
            assertEquals("dave", peopleManagerUID);     
            String variableManagerUID = XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-manager-uid-ldap($TaskInstance.Owner)", jxpathContex), "");
            assertEquals("dave", selfManagerUID);
        }
        
    }
    
    public void testXpath20Expression () throws Exception {
        //Load task definition
        URL fileUrl = getClass().getResource ("data/ApprovePurchaseXpath20.wf");            
        File wfFile = new File (fileUrl.getFile());
        Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());        
        String taskInputFileName = "data/purchaseOrderTaskInputData.xml";
        Element el = Util.loadInputMessageElement(taskInputFileName, this.getClass());
        RuntimeVariables  runtimeVariables = new RuntimeVariablesImpl();
        runtimeVariables.declareVariable(RuntimeVariables.TASK_INPUT_VAR,
                el);
        JXPathContext jxpathContex = XPathUtil.newJXPathContextFromVar(runtimeVariables, new XPathUtil.DOMFactory ());
        
        Message inputMsg = task.getWSDLOperation().getInput().getMessage();
        //XPathUtil.setPartsAsVariables(RuntimeVariables.TASK_INPUT_VAR, mInput.getInput(), inputMsg, mRuntimeVariables);
        String partName = (String)  inputMsg.getParts().keySet().iterator().next();
        runtimeVariables.declareVariable(RuntimeVariables.TASK_INPUT_VAR + "." +partName ,
               el);
        
        String expression = "$TaskInput.part1/po:amount";
        String result = ModelUtil.getStringValueXpath20(jxpathContex, expression, task, "|");
        assertEquals("501.00", result);
        String expression2 = "if ($TaskInput.part1/po:amount < 1000)  then (if ($TaskInput.part1/po:amount > 500)  then ('john') else ('CustomerServiceRep1')) else  ('CustomerServiceRep')";
        String result2 = ModelUtil.getStringValueXpath20(jxpathContex, expression2, task, "|");
        assertEquals("john", result2);        
        
        expression ="$TaskInput.part1/po:user";
        result = ModelUtil.getStringValueXpath20(jxpathContex, expression, task, "|");
        assertEquals("usr1|usr2", result);
          
    }

}
