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
 * @(#)DateTimeTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */package com.sun.jbi.engine.bpel.core.test.modelparsing;

import java.io.InputStream;
import java.net.URL;
import java.util.logging.Logger;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import com.sun.bpel.model.Copy;
import com.sun.bpel.model.From;
import com.sun.bpel.model.To;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.impl.RAssignImpl;
import com.sun.bpel.model.meta.impl.RWaitImpl;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELProcessManagerImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.BPELProcessInstanceImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimeVariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.DateTime;
import com.sun.jbi.engine.bpel.core.bpel.util.FromEvaluatorFactory;
import com.sun.jbi.engine.bpel.core.bpel.util.FromEvaluatorFactory.FromEvaluator;
import com.sun.jbi.engine.bpel.core.test.bpelpersist.AbstractTestCase;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

/**
 * Unit test for testing the application variables support in BPEL engine. The logic for this
 * test is to get the result of executing of bpel activity and then compare that against the known
 *
 * @author Vitaly Bychkov
 */
public class AppVarTest extends AbstractTestCase {

    private static Logger LOGGER = Logger.getLogger(AppVarTest.class.getName());
    protected RBPELProcess mProcess;
    private static final String APP_VAR_NAME_WELCOME_MESS = "welcomeMessage";
    private static final String APP_VAR_VALUE_WELCOME_MESS = "Hello Stranger!";
    private static final String APP_VAR_VALUE_CDATA_WELCOME_MESS = "<syn:typeA xmlns:syn=\"http://xml.netbeans.org/schema/SynchronousSample\"><syn:paramA>CDATA: Hello Stranger!</syn:paramA></syn:typeA>";
    private static final String APP_VAR_VALUE_EII_WELCOME_MESS = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><ns0:typeA xmlns:ns0=\"http://xml.netbeans.org/schema/SynchronousSample\" xmlns:bpws=\"http://docs.oasis-open.org/wsbpel/2.0/process/executable\" xmlns:ns1=\"http://localhost/SynchronousSample/SynchronousSample\" xmlns:sref=\"http://docs.oasis-open.org/wsbpel/2.0/serviceref\" xmlns:sxed=\"http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/Editor\" xmlns:sxt=\"http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/Trace\" xmlns:syn=\"http://xml.netbeans.org/schema/SynchronousSample\" xmlns:wsdlNS=\"http://enterprise.netbeans.org/bpel/SynchronousSample/SynchronousSample_1\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><paramA xmlns=\"http://xml.netbeans.org/schema/SynchronousSample\">EII: Hello Stranger!</paramA></ns0:typeA>";

    private static final String APP_VAR_NAME_MINUTES2WAIT = "minutes2wait";
    private static final String APP_VAR_VALUE_MINUTES2WAIT = "1";
    private static final String APP_VAR_NAME_DATE_UNTIL_WAIT = "dateUntilWait";
    private static final String APP_VAR_VALUE_DATE_UNTIL_WAIT = "1971-01-01T01:01:1.0+00:00";

    public AppVarTest(String testName) {
        super(testName);
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        initProcessEnv();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
        mEng.removeModel(mProcess.getBPELId());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(AppVarTest.class);

        return suite;
    }

    public void testAppVarValidation() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testAppVarValidation");
        Utility.logExit(getClass().getSimpleName(), "testAppVarValidation");
    }

    public void testWaitForAppVar() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testWaitForAppVar");

        DateTime dateTime = new DateTime();
        DateTime correctDateTime = new DateTime(dateTime.toCalendar());

        correctDateTime.addMinutes(Integer.parseInt(APP_VAR_VALUE_MINUTES2WAIT));
        Date correctDate =correctDateTime.toCalendar().getTime();

        Object result = getWaitAppVarTestResult("Wait1", dateTime);
        assertTrue(correctDate.equals(result));
        Utility.logExit(getClass().getSimpleName(), "testWaitForAppVar");
    }

    public void testWaitUntilAppVar() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testWaitUntilAppVar");

        GregorianCalendar cal = new  GregorianCalendar(DateTime.UTC);
        cal.clear();
        cal.setLenient(true);
        cal.set(1971, 0, 1, 1, 1, 1);
        DateTime correctDateTime = new DateTime(cal);
        Date correctDate = correctDateTime.toCalendar().getTime();
        
        Object result = getWaitAppVarTestResult("Wait2", null);
        assertTrue(correctDate.equals(result));

        Utility.logExit(getClass().getSimpleName(), "testWaitUntilAppVar");
    }

    public void testLiteralCDataAppVar() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testLiteralCDataAppVar");

        Object result = getAssignAppVarTestResult("Assign3");
        assertTrue(APP_VAR_VALUE_CDATA_WELCOME_MESS.equals(result));

        Utility.logExit(getClass().getSimpleName(), "testLiteralCDataAppVar");
    }

    public void testLiteralEIIAppVar() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testLiteralEIIAppVar");

        Object result = getAssignAppVarTestResult("Assign2");
        assertTrue(result instanceof Element);
        String resultStr = DOMHelper.createXmlString((Element)result);
        
        assertTrue(APP_VAR_VALUE_EII_WELCOME_MESS.equals(resultStr));

        Utility.logExit(getClass().getSimpleName(), "testLiteralEIIAppVar");
    }

    public void testLiteralAppVar() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testLiteralAppVar");

        Object result = getAssignAppVarTestResult("Assign1");
        assertTrue(APP_VAR_VALUE_WELCOME_MESS.equals(result));

        Utility.logExit(getClass().getSimpleName(), "testLiteralAppVar");
    }

    private void initProcessEnv() {
        if (mProcess == null) {
            mProcess = loadBPELModel("appvar/SynchronousSample.bpel");
            mEng.addModel(mProcess, null, null);
        }
    }

    private Object getAssignAppVarTestResult(String assignName) throws Exception {
        assertTrue(assignName != null);
        
        BPELProcessInstance instance = prepareEnv();
        assertTrue(instance != null);

        RActivity rootAct = (RActivity) mProcess.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
                                       .createActivityUnit((Context)instance, null,
                rootAct, RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());

        RActivity rAssign = HelperFunc.getActivity(mProcess, assignName);

        assertTrue(rAssign != null);

        Copy copy = (Copy) ((RAssignImpl) rAssign).getCopy(0);
        assertTrue(copy != null);

        From from = copy.getFrom();
        To to = copy.getTo();
        assertTrue(from != null);
        assertTrue(to != null);

        FromEvaluator fromEval = FromEvaluatorFactory.getFromEvaluator(from);

        return fromEval == null ? null : fromEval.evaluateFrom(from, rootActUnit.getContext(), to);
    }

    private Object getWaitAppVarTestResult(String waitName, DateTime dateTime) throws Exception {
        assertTrue(waitName != null);
        boolean isFor = true;

        BPELProcessInstance instance = prepareEnv();
        assertTrue(instance != null);

        RActivity rootAct = (RActivity) mProcess.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
                                       .createActivityUnit((Context)instance, null,
                rootAct, RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());

        RActivity rWait = HelperFunc.getActivity(mProcess, waitName);
        assertTrue(rWait != null);

        String expr = ((RWaitImpl) rWait).getFor();
        isFor = expr != null;
        if (!isFor) {
            expr = ((RWaitImpl) rWait).getUntil();
        }

        return com.sun.jbi.engine.bpel.core.bpel.util.Utility.
                getDateFromExpr(rWait, rootActUnit.getContext(), expr, isFor, dateTime);
    }

    private BPELProcessInstance prepareEnv() {
        initProcessEnv();
        //prepare env
        BPELProcessManagerImpl mgr = new BPELProcessManagerImpl(mProcess, mEng, null, null);
        //emulate deploy step - setup app vars
        registerAppVars(mgr);
        BPELProcessInstance instance = new BPELProcessInstanceImpl(mgr, mEng, "123");
        return instance;
    }

    private void registerAppVars(BPELProcessManager mgr) {
        Map<String, Object> appVars = new HashMap<String, Object>();

        appVars.put(APP_VAR_NAME_WELCOME_MESS, new String[]{APP_VAR_VALUE_WELCOME_MESS, "STRING"});
        appVars.put(APP_VAR_NAME_MINUTES2WAIT, new String[]{APP_VAR_VALUE_MINUTES2WAIT, "INT"});
        appVars.put(APP_VAR_NAME_DATE_UNTIL_WAIT, new String[]{APP_VAR_VALUE_DATE_UNTIL_WAIT, "STRING"});
        mgr.setApplicationVariables(appVars);
    }

    private RuntimeVariable createVariable(String varName) {
        Variables vars = mProcess.getVariables();
        RVariable var = (RVariable) vars.getVariable(varName);

        String scopeId = RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.toString();
        RuntimeVariable retVal = new RuntimeVariableImpl(var, null, scopeId);
        WSMessage msg = constructMsg();
        msg.addInternalReference(var);
        retVal.setWSMessage(msg);

        return retVal;
    }

    private WSMessage constructMsg() {
        String dataFilePath = "bpel/appvar/SynchronousSample.xml";
        InputSource ipSource = null;
        WSMessage domMsg = null;

        try {
            URL fileURL = getClass().getResource(dataFilePath);
            InputStream ipstream = fileURL.openStream();
            ipSource = new InputSource(ipstream);

            //Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(ipSource);
            Element elem = Utility.createDOMElement(ipSource);

            WSDLDocument doc = loadWSDL("bpel/appvar/SynchronousSample.wsdl");
            Definition defn = doc.getDefinition();

            QName qName = new QName(defn.getTargetNamespace(),
                    "requestMessage");

            //domMsg = new JBIMessageImpl(doc, msg);
            domMsg = new JBIMessageImpl(elem.getOwnerDocument(), defn.getMessage(qName));
        } catch (Throwable t) {
            fail(" failed to parse the file " + t.getMessage());
        }

        return domMsg;
    }
}
