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
 * @(#)ModelTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.test;

import com.sun.jbi.workflow.model.Action;
import com.sun.jbi.workflow.model.Address;
import com.sun.jbi.workflow.model.Assignment;
import com.sun.jbi.workflow.model.Copy;
import com.sun.jbi.workflow.model.EmailNotification;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.LocalNotification;
import com.sun.jbi.workflow.model.Message;
import com.sun.jbi.workflow.model.ModelFactory;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.Subject;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.Timeout;
import com.sun.jbi.workflow.model.Variable;
import com.sun.jbi.workflow.model.ChangeVariables;
import com.sun.jbi.workflow.model.XPathInfo;
import com.sun.jbi.workflow.model.utl.ModelUtil;
import com.sun.jbi.workflow.model.xmlbeans.TActionType;
import com.sun.jbi.workflow.model.xmlbeans.TAssignment;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;

import java.io.File;
import java.net.URL;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import junit.framework.*;

import org.apache.commons.jxpath.BasicVariables;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathContextFactory;
import org.apache.commons.jxpath.Variables;
import org.apache.xmlbeans.XmlObject;
import org.w3c.dom.NodeList;

/**
 *
 * 
 */
public class ModelTest extends TestCase {
    
    public ModelTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(ModelTest.class);

        return suite;
    }
    
    // TODO add test methods here. The name must begin with 'test'. For example:
    public void testHello() {
    }

    public void testModelParsing()  {
         try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
             URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchase.wf");            
             File wfFile = new File (fileUrl.getFile());
             Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
             assertNotNull(task);

             Operation opt = task.getWSDLOperation();
             assertEquals("ApprovePurchase", opt.getName());
              
             PortType portType = task.getPortType();
             assertEquals("ApprovePurchasePT", portType.getQName().getLocalPart());
             
             assertEquals("http://jbi.com.sun/wfse/wsdl/workflow_po_3/ApprovePurchase_TM_BPEL", portType.getQName().getNamespaceURI());
             
             Assignment assignment = task.getTaskAssignment();
             assertNotNull("should have one assignment", assignment);
             
             List<TExpression> userList = ((TAssignment) assignment.getDelegate()).getUserList();
             assertEquals("should have users", 2,  userList.size());
             
             List<Timeout> timeoutList = task.getTaskTimeouts();
             assertEquals("should have timeout", 2,  timeoutList.size());
            
             Timeout timeout1 = timeoutList.get(0);
             assertNotNull("should have a deadline", timeout1.getDeadlineObject(context));
              
             
             Timeout timeout2 = timeoutList.get(1);
             assertNotNull("should have a duration", timeout2.getDurationObject(context));
             
             List<Escalation> escalationList = task.getTaskEscalations();
             assertEquals("should have escalation", 2,  escalationList.size());
             
             Escalation escalation1 = escalationList.get(0);
             assertNotNull("should have a deadline", escalation1.getDeadlineObject(context));
             
           
             Escalation escalation2 = escalationList.get(1);
             assertNotNull("should have a duration", escalation2.getDurationObject(context));
             
             XPathInfo info1 = timeout1.getXPathInfo();
             String xpath1 = info1.getXPath();
             Map<String, String> prefixToNS1 = info1.getPrefixToNamespaceMap();
             System.out.println("xpath "+ info1);
             NodeList nodelist1 = ModelUtil.executeXPath(xpath1, task.getDelegate().getDomNode(), prefixToNS1);
             assertTrue("should get first timeout node", nodelist1.item(0).equals(timeout1.getDelegate().getDomNode()));
             
             XPathInfo info2 = timeout2.getXPathInfo();
             String xpath2 = info2.getXPath();
             Map<String, String> prefixToNS2 = info2.getPrefixToNamespaceMap();
             System.out.println("xpath "+ info2);
             NodeList nodelist2 = ModelUtil.executeXPath(xpath2, task.getDelegate().getDomNode(), prefixToNS2);
             assertTrue("should get second timeout node", nodelist2.item(0).equals(timeout2.getDelegate().getDomNode()));
             
             //xmlbeans query
             String query = "declare namespace s='http://jbi.com.sun/wfse'; $this/s:timeout";
             XmlObject[] result = task.getDelegate().selectPath(query);
             assertTrue("should get 2 timeout XmlObjects", result.length == 2);
             
             System.out.println("xpath result" + result);
             assertEquals(timeout1.getDelegate(), result[0]);
             assertEquals(timeout2.getDelegate(), result[1]);
             
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
         
         
     }
    
    public void testDeadline()  {
        try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
            URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseValidDeadline.wf");            
            File wfFile = new File (fileUrl.getFile());
            Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
            assertNotNull(task);

            
            List<Timeout> timeoutList = task.getTaskTimeouts();
            assertEquals("should have timeout", 1,  timeoutList.size());
           
            Timeout timeout1 = timeoutList.get(0);
            assertNotNull("should have a deadline", timeout1.getDeadlineObject(context));
            
            //expected 2006-12-01T23:00:00
            Calendar expectDeadlineCal1 = Calendar.getInstance();
            expectDeadlineCal1.set(2006, 11, 01, 23, 0, 0);
            
            Calendar resultantDeadlineCal1 = Calendar.getInstance();
            resultantDeadlineCal1.setTime(timeout1.getDeadlineObject(context));
                   
            assertEquals("year should be ", expectDeadlineCal1.get(Calendar.YEAR), resultantDeadlineCal1.get(Calendar.YEAR));
            assertEquals("month should be ", expectDeadlineCal1.get(Calendar.MONTH), resultantDeadlineCal1.get(Calendar.MONTH));
            assertEquals("day should be ", expectDeadlineCal1.get(Calendar.DATE), resultantDeadlineCal1.get(Calendar.DATE));
            assertEquals("hour should be ", expectDeadlineCal1.get(Calendar.HOUR), resultantDeadlineCal1.get(Calendar.HOUR));
            assertEquals("minute should be ", expectDeadlineCal1.get(Calendar.MINUTE), resultantDeadlineCal1.get(Calendar.MINUTE));
            assertEquals("second should be ", expectDeadlineCal1.get(Calendar.SECOND), resultantDeadlineCal1.get(Calendar.SECOND));
            
            
            List<Escalation> escalationList = task.getTaskEscalations();
            assertEquals("should have escalation", 1,  escalationList.size());
            
            Escalation escalation1 = escalationList.get(0);
            assertNotNull("should have a deadline", escalation1.getDeadlineObject(context));
            
          
            //expected 2002-10-10T07:00:00Z or 2002-10-10T12:00:00+05:00
            Calendar expectDeadlineCal2 = Calendar.getInstance();
            expectDeadlineCal2.set(2002, 9, 10, 12, 0, 0);
            expectDeadlineCal2.setTimeZone(TimeZone.getTimeZone("GMT+5"));
            
            Calendar resultantDeadlineCal2 = Calendar.getInstance();
            resultantDeadlineCal2.setTime(escalation1.getDeadlineObject(context));
            expectDeadlineCal2.setTimeZone(TimeZone.getTimeZone("GMT"));
            
            assertEquals("year should be ", expectDeadlineCal2.get(Calendar.YEAR), expectDeadlineCal2.get(Calendar.YEAR));
            assertEquals("month should be ", expectDeadlineCal2.get(Calendar.MONTH), expectDeadlineCal2.get(Calendar.MONTH));
            assertEquals("day should be ", expectDeadlineCal2.get(Calendar.DATE), expectDeadlineCal2.get(Calendar.DATE));
            assertEquals("hour should be ", expectDeadlineCal2.get(Calendar.HOUR), expectDeadlineCal2.get(Calendar.HOUR));
            assertEquals("minute should be ", expectDeadlineCal2.get(Calendar.MINUTE), expectDeadlineCal2.get(Calendar.MINUTE));
            assertEquals("second should be ", expectDeadlineCal2.get(Calendar.SECOND), expectDeadlineCal2.get(Calendar.SECOND));
            
       } catch (Exception e) {
           e.printStackTrace();
           // TODO Auto-generated catch block
           fail(e.getMessage());
       }
        
    }
    
    public void testVariables ()  {
        try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
            URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseInVariable.wf");            
            File wfFile = new File (fileUrl.getFile());
            Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
  
            
            assertNotNull (task.getInit());
            assertNotNull (task.getInit().getVariables());
            assertEquals ( 1, task.getInit().getVariables().getVariables().size());
            Variable var = task.getInit().getVariables().getVariables().get(0);
            assertEquals("var1", var.getName());
            
            assertEquals (1, task.getInit().getChangeVariables().size());
            ChangeVariables varInit = task.getInit().getChangeVariables().get(0);
            
            assertEquals (1, varInit.getCopies().size());
            
            Copy copy = varInit.getCopies().get(0);
            
            assertEquals("$var1", copy.getFromExpr());
            assertEquals ("$var2", copy.getToExpr());
            
            Variables variables = new BasicVariables ();
            variables.declareVariable("Taskpayload", ModelTestUtil.loadElement("/data/samples/approvePurchase/Input.xml"));
            context.setVariables(variables);
            
            List<Timeout> timeoutList = task.getTaskTimeouts();
            assertEquals("should have timeout", 1,  timeoutList.size());
           
            Timeout timeout1 = timeoutList.get(0);
            assertNotNull("should have a deadline", timeout1.getDeadlineObject(context));
            
            //expected 2006-12-01T23:00:00
            Calendar expectDeadlineCal1 = Calendar.getInstance();
            expectDeadlineCal1.set(2006, 11, 01, 23, 0, 0);
            
            Calendar resultantDeadlineCal1 = Calendar.getInstance();
            resultantDeadlineCal1.setTime(timeout1.getDeadlineObject(context));
                   
            assertEquals("year should be ", expectDeadlineCal1.get(Calendar.YEAR), resultantDeadlineCal1.get(Calendar.YEAR));
            assertEquals("month should be ", expectDeadlineCal1.get(Calendar.MONTH), resultantDeadlineCal1.get(Calendar.MONTH));
            assertEquals("day should be ", expectDeadlineCal1.get(Calendar.DATE), resultantDeadlineCal1.get(Calendar.DATE));
            assertEquals("hour should be ", expectDeadlineCal1.get(Calendar.HOUR), resultantDeadlineCal1.get(Calendar.HOUR));
            assertEquals("minute should be ", expectDeadlineCal1.get(Calendar.MINUTE), resultantDeadlineCal1.get(Calendar.MINUTE));
            assertEquals("second should be ", expectDeadlineCal1.get(Calendar.SECOND), resultantDeadlineCal1.get(Calendar.SECOND));
            
            
            List<Escalation> escalationList = task.getTaskEscalations();
            assertEquals("should have escalation", 1,  escalationList.size());
            
            Escalation escalation1 = escalationList.get(0);
            assertNotNull("should have a deadline", escalation1.getDeadlineObject(context));
            
          
            //expected 2002-10-10T07:00:00Z or 2002-10-10T12:00:00+05:00
            Calendar expectDeadlineCal2 = Calendar.getInstance();
            expectDeadlineCal2.set(2002, 9, 10, 12, 0, 0);
            expectDeadlineCal2.setTimeZone(TimeZone.getTimeZone("GMT+5"));
            
            Calendar resultantDeadlineCal2 = Calendar.getInstance();
            resultantDeadlineCal2.setTime(escalation1.getDeadlineObject(context));
            expectDeadlineCal2.setTimeZone(TimeZone.getTimeZone("GMT"));
            
            assertEquals("year should be ", expectDeadlineCal2.get(Calendar.YEAR), expectDeadlineCal2.get(Calendar.YEAR));
            assertEquals("month should be ", expectDeadlineCal2.get(Calendar.MONTH), expectDeadlineCal2.get(Calendar.MONTH));
            assertEquals("day should be ", expectDeadlineCal2.get(Calendar.DATE), expectDeadlineCal2.get(Calendar.DATE));
            assertEquals("hour should be ", expectDeadlineCal2.get(Calendar.HOUR), expectDeadlineCal2.get(Calendar.HOUR));
            assertEquals("minute should be ", expectDeadlineCal2.get(Calendar.MINUTE), expectDeadlineCal2.get(Calendar.MINUTE));
            assertEquals("second should be ", expectDeadlineCal2.get(Calendar.SECOND), expectDeadlineCal2.get(Calendar.SECOND));
            
       } catch (Exception e) {
           e.printStackTrace();
           // TODO Auto-generated catch block
           fail(e.getMessage());
       }
        
    }
    
    public void testDuration()  {
        try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
            URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseValidDuration.wf");            
            File wfFile = new File (fileUrl.getFile());
            Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
            assertNotNull(task);
             
            Assignment assignment = task.getTaskAssignment();
            assertNotNull("should have one assignment", assignment);
            
            List<TExpression> userList = ((TAssignment) assignment.getDelegate()).getUserList();
            assertEquals("should have users", 2,  userList.size());
            
            List<Timeout> timeoutList = task.getTaskTimeouts();
            assertEquals("should have timeout", 1,  timeoutList.size());
           
            Timeout timeout1 = timeoutList.get(0);
            assertNotNull("should have a duration", timeout1.getDurationObject(context));
            
            Calendar expectedDuration1 = Calendar.getInstance();
            expectedDuration1.set(1, 2, 3, 10, 30, 0);
            assertEquals("year should be ", expectedDuration1.get(Calendar.YEAR), timeout1.getDurationObject(context).getYear());
            assertEquals("month should be ", expectedDuration1.get(Calendar.MONTH), timeout1.getDurationObject(context).getMonth());
            assertEquals("day should be ", expectedDuration1.get(Calendar.DATE), timeout1.getDurationObject(context).getDay());
            assertEquals("hour should be ", expectedDuration1.get(Calendar.HOUR), timeout1.getDurationObject(context).getHour());
            assertEquals("minute should be ", expectedDuration1.get(Calendar.MINUTE), timeout1.getDurationObject(context).getMinute());
            assertEquals("second should be ", expectedDuration1.get(Calendar.SECOND), timeout1.getDurationObject(context).getSecond());
            
            List<Escalation> escalationList = task.getTaskEscalations();
            assertEquals("should have escalation", 1,  escalationList.size());
            
            Escalation escalation1 = escalationList.get(0);
            assertNotNull("should have a duration", escalation1.getDurationObject(context));
            
            
            assertEquals("year should be ", 0, escalation1.getDurationObject(context).getYear());
            assertEquals("month should be ", 0, escalation1.getDurationObject(context).getMonth());
            assertEquals("day should be ", 0, escalation1.getDurationObject(context).getDay());
            assertEquals("hour should be ", 15, escalation1.getDurationObject(context).getHour());
            assertEquals("minute should be ", 0, escalation1.getDurationObject(context).getMinute());
            assertEquals("second should be ", 0, escalation1.getDurationObject(context).getSecond());
            
       } catch (Exception e) {
           e.printStackTrace();
           fail(e.getMessage());
       }
        
    }
    
    
    public void testInvalidDuration1()  {
        try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
            URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseInvalidDuration.wf");            
            File wfFile = new File (fileUrl.getFile());
            Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
            assertNotNull(task);
            
            List<Timeout> timeoutList = task.getTaskTimeouts();
            assertEquals("should have timeout", 1,  timeoutList.size());
           
            Timeout timeout1 = timeoutList.get(0);
            timeout1.getDurationObject(context);
            
              
       } catch (Exception e) {
           e.printStackTrace();
           assertNotNull("should get exception for invalid duration",  e);
       }
        
    }
    
    public void testInvalidDuration2()  {
        try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
            URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseInvalidDuration.wf");            
            File wfFile = new File (fileUrl.getFile());
            Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
            assertNotNull(task);
             
            List<Escalation> escalationList = task.getTaskEscalations();
            assertEquals("should have escalation", 1,  escalationList.size());
            
            Escalation escalation1 = escalationList.get(0);
            escalation1.getDurationObject(context);
            
       } catch (Exception e) {
           e.printStackTrace();
           assertNotNull("should get exception for invalid duration",  e);
           
       }
        
    }
    
    public void testNegativeModelParsing()  {
         try {
             URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseInvalidSyntax.wf");            
             File wfFile = new File (fileUrl.getFile());
             Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
             assertNotNull(task);
              
             assertNull("assignment should be null", task.getTaskAssignment());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            fail(e.getMessage());
        }
         
         
     }
    
    public void testNotificationValid() {
        try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
             URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseValidNotification.wf");            
             File wfFile = new File (fileUrl.getFile());
             Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
             assertNotNull(task);

             
             List<Notification> notificationList = task.getTaskNotifications();
             assertEquals("should have notication", 1,  notificationList.size());
            
             EmailNotification n = (EmailNotification) notificationList.get(0);
             assertNotNull(n.getName());
             
             assertNotNull(n.getPortType());
             
             List<Action> actions =  task.getTaskActions();
             assertEquals("should have action", 1,  actions.size());
             
             Action action = actions.get(0);
             List<LocalNotification> lnList = action.getLocalNotifications();
             assertEquals("should have one local notification", 1,  lnList.size());
             
             LocalNotification ln = lnList.get(0);
             Notification refNotification = ln.getReferencedNotification();
             assertNotNull(refNotification);
             assertEquals("referenced notification should equal to actual notification ", refNotification, n);
             
             List<Address> addrs1 =  n.getAddresses();
             

             assertEquals("should have 2 address", 2, addrs1.size());
             Address addr1 = addrs1.get(0);
             assertEquals("should have address", "mailto:ritesh.adval@sun.com",  addr1.getContent(context));
             String part1 = n.getPart();
             assertNotNull("part should not be null", part1);
             Part wsdlPart1 = n.getWSDLPart();
             assertNotNull("wsdl part should not be null", wsdlPart1);
             
            
             Subject subject = n.getSubject();
             assertNotNull("subject should not be null", subject);
             
             Message msg = n.getMessage();
             assertNotNull(msg);
             assertEquals("message is ", "The task can be found at <a href=\"localhost\">",  msg.getContent(context));
             String part4 = msg.getPart();
             assertNotNull("part should not be null", part4);
             Part wsdlPart4 = msg.getWSDLPart();
             assertNotNull("wsdl part should not be null", wsdlPart4);
             
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            fail(e.getMessage());
        }
    }
    
    public void testValidEscalationNotification() {
        try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
             URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseValidDeadlineWithNotification.wf");            
             File wfFile = new File (fileUrl.getFile());
             Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
             assertNotNull(task);
              
             List<Notification> notificationList = task.getTaskNotifications();
             assertEquals("should have notication", 1,  notificationList.size());
            
             EmailNotification n = (EmailNotification) notificationList.get(0);

             assertNotNull(n.getWSDLOperation());
             assertNotNull(n.getPortType());
 
             
             
             List<Escalation> escalations =  task.getTaskEscalations();
             assertEquals("should have escalation", 1,  escalations.size());
             
             Escalation escalation = escalations.get(0);
             List<LocalNotification> lnList =  escalation.getLocalNotifications();
             assertEquals("should have local notification", 1,  lnList.size());
             
             LocalNotification ln = lnList.get(0);
             
             Notification refNotification = ln.getReferencedNotification();
             assertNotNull(refNotification);
             assertEquals("should equal with ", n, refNotification); 
            
             
             
        } catch (Exception e) {
            // TODO Auto-generated catch block
            fail(e.getMessage());
        }
    }
    
//    public void testInValidTaskAction() {
//        try {
//             URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseInValidActionType.wf");            
//             File wfFile = new File (fileUrl.getFile());
//             Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
//             assertNotNull(task);
//             
//             List<Action> actionList = task.getTaskActions();
//             assertEquals("should have action", 1,  actionList.size());
//            
//             Action action = actionList.get(0);
//             List<LocalNotification> lnList = action.getLocalNotifications();
//             assertEquals("should have LocalNotification", 0,  lnList.size());
//             
//             TActionType.Enum type = action.getType();
//             assertNull("type should be null", type);
//             
//        } catch (Exception e) {
//            // TODO Auto-generated catch block
//            fail(e.getMessage());
//        }
//    }
    
    public void testInValidEmptyNotificationRecipient() {
        try {
             URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseInValidEmptyNotificationRecipient.wf");            
             File wfFile = new File (fileUrl.getFile());
             Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
             assertNotNull(task);
              
             List<Notification> notificationList = task.getTaskNotifications();
             assertEquals("should have notication", 1,  notificationList.size());
            
             EmailNotification n = (EmailNotification) notificationList.get(0);

             assertNull(n.getWSDLOperation());
             assertNull(n.getPortType());

            
             List<Address> rList = n.getAddresses();
             assertEquals("should have 0 recipient ", 0,  rList.size());
             
              
             
             
             
        } catch (Exception e) {
            // TODO Auto-generated catch block
            fail(e.getMessage());
        }
    }
    
//    public void testInValidNotificationMissingAttributes() {
//        try {
//             URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseInValidNotificationMissingAttributes.wf");            
//             File wfFile = new File (fileUrl.getFile());
//             Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
//             assertNotNull(task);
//             
//             List<Notification> notificationList = task.getTaskNotifications();
//             assertEquals("should have notication", 1,  notificationList.size());
//            
//             EmailNotification n = (EmailNotification) notificationList.get(0);
//
//             assertNull(n.getWSDLOperation());
//             assertNull(n.getPortType());
//        } catch (Exception e) {
//            // TODO Auto-generated catch block
//            fail(e.getMessage());
//        }
//    }
//    
    
    public void testValidValidMultipleNotificationRecipientAddress() {
        try {
            JXPathContext context = JXPathContextFactory.newInstance().newContext(null, null);
             URL fileUrl = getClass().getClass().getResource ("/data/samples/approvePurchase/ApprovePurchaseValidMultipleNotificationRecipientAddress.wf");            
             File wfFile = new File (fileUrl.getFile());
             Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());
             assertNotNull(task);

             List<Notification> notificationList = task.getTaskNotifications();
             assertEquals("should have notication", 1,  notificationList.size());
            
             EmailNotification n = (EmailNotification) notificationList.get(0);

             assertNotNull(n.getWSDLOperation());
             assertNotNull(n.getPortType());
            
             
             List<Address> rList = n.getAddresses();
             assertEquals("should have recipient ", 2,  rList.size());
             

             String part1 = n.getPart();
             assertNotNull("part should not be null", part1);
             Part wsdlPart1 = n.getWSDLPart();
             assertNotNull("wsdl part should not be null", wsdlPart1);             
             
             Subject sub = n.getSubject();
             assertNotNull(sub);
             assertEquals("subject is ", "You have a new task assigned for purchase Order review",  sub.getContent(context));
             String part2 = sub.getPart();
             assertNotNull("part should not be null", part2);
             Part wsdlPart2 = sub.getWSDLPart();
             assertNotNull("wsdl part should not be null", wsdlPart2);
             
             Message msg = n.getMessage();
             assertNotNull(msg);
             assertEquals("message is ", "The task can be found at <a href=\"localhost\">",  msg.getContent(context));
             String part3 = msg.getPart();
             assertNotNull("part should not be null", part3);
             Part wsdlPart3 = msg.getWSDLPart();
             assertNotNull("wsdl part should not be null", wsdlPart3);
             
        } catch (Exception e) {
            // TODO Auto-generated catch block
            fail(e.getMessage());
        }
    }
    
    
    
}
