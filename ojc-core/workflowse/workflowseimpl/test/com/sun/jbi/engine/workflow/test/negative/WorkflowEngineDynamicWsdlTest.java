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
 * @(#)WorkflowEngineDynamicWsdlTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.test.negative;

import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.test.DefaultTestInOutCallBack;
import java.net.URI;
import java.net.URL;
import java.security.Principal;
import java.util.HashSet;
import java.util.Set;

import junit.framework.*;

import com.sun.corba.se.impl.orbutil.graph.Node;
import com.sun.jbi.engine.workflow.BPELWorkflowRequest;
import com.sun.jbi.engine.workflow.ClientWorkflowRequest;
import com.sun.jbi.engine.workflow.WorkflowEngine;
import com.sun.jbi.engine.workflow.clientapi.StaticOperation;
import com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory;
import com.sun.jbi.engine.workflow.clientapi.StaticOperationFactoryTest;
import com.sun.jbi.engine.workflow.process.InOutCallBack;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactoryTest;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.model.Task;

import javax.security.auth.Subject;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import sun.security.acl.PrincipalImpl;

/**
 *
 * 
 */
public class WorkflowEngineDynamicWsdlTest extends TestCase {

    private PortType mPortTypeTaskStatic;
    
    private PortType mPortTypeTaskDynamic;
    
    
    public WorkflowEngineDynamicWsdlTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        Util.initTaskManager();
        init();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(WorkflowEngineDynamicWsdlTest.class);

        return suite;
    } 
    
    public void testInvalidWorkflowSyntax() throws Exception {
        WorkflowEngine engine = new WorkflowEngine ();
        engine.init();
        DefaultTestInOutCallBack callBack = new DefaultTestInOutCallBack();
        engine.setInOutCallBack(callBack);
        
        engine.start();
        
        //Create task
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseInvalidSyntax.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        org.w3c.dom.Node node = ((DOMSource) taskInput).getNode();
        Element taskEl = null;
        if (node instanceof Document) {
            taskEl = ((Document) node ).getDocumentElement();
        } else {
            taskEl = (Element) node;
        }
 
        BPELWorkflowRequest bpelReq = new BPELWorkflowRequest ("TestCreateTask",  "TestCreateTask", taskEl, task);
        engine.acceptRequest(bpelReq);
        Thread.sleep(5000);
        
        assertTrue( "expected fault there is no assignment in task definition, see output for more details", callBack.isFaulted());    
    }
    
    private void init() throws Exception {
      if(mPortTypeTaskStatic != null && mPortTypeTaskDynamic != null) {
          return;
      }
      
       this.mPortTypeTaskStatic = Util.loadTaskCommonPortType();
       this.mPortTypeTaskDynamic = Util.loadTaskDynamicPortType();
    }
  
}
