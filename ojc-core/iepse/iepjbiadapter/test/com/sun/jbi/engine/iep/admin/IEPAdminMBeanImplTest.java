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
package com.sun.jbi.engine.iep.admin;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.util.IEPEngineManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Iterator;


/**
 *
 * @author rdwivedi
 */
public class IEPAdminMBeanImplTest {
    
   static private IEPEngineManager  mgr =  new IEPEngineManager();
    public IEPAdminMBeanImplTest() {
        
    }

    @org.junit.BeforeClass
    public static void setUpClass() throws Exception {
        mgr.startIEPEngineWithEmbeddedDerby();
        //C:/open-esb-jbi-component/open-jbi-components/ojc-core/iepse/iepjbiadapter/
        File f = new File("test/com/sun/jbi/engine/iep/projectdata/iepAttributeBasedWindow.iep");
        mgr.deployIEPProcessToEngine(f);
    }

    @org.junit.AfterClass
    public static void tearDownClass() throws Exception {
        mgr.getIEPEngine().stopAll();
    }

    @org.junit.Before
    public void setUp() throws Exception {
        
        
    }

    @org.junit.After
    public void tearDown() throws Exception {
    }

   

    /**
     * Test of listProcessAttributes method, of class IEPAdminMBeanImpl.
     */
     
    @org.junit.Test
    public void listProcessAttributes() {
        String[] expStr = new String[6];
        expStr[0] = "StreamInput0";
        expStr[1] = "AttributeBasedWindow0";
        expStr[2] = "TupleBasedWindow0";
        expStr[3] = "RelationStream0";
        expStr[4] = "Table0";
        expStr[5] = "StreamOutput0";
        int i = 0;
        System.out.println("listProcessAttributes");
        String serviceUnitName = "iepAttributeBasedWindow_iep";
        QueryPlan plan = mgr.getPlan(serviceUnitName);
        IEPAdminUtil util = new IEPAdminUtil();
        String[] str =  util.getListOfOperators(plan);
        assertEquals(str.length,6);
        for(int k=0; k < 6; k++){
            assertEquals(expStr[k],str[k]);
        }
       //fail("The test case is a prototype.");
    }

    /**
     * Test of getOperatorDependentAttributes method, of class IEPAdminMBeanImpl.
     */
    @org.junit.Test
    public void getOperatorDependentAttributes() {
        String serviceUnitName = "iepAttributeBasedWindow_iep";
        QueryPlan plan = mgr.getPlan(serviceUnitName);
        Map<String,Object> map = null;
        IEPAdminUtil util = new IEPAdminUtil();
        map =  util.getAdministrablePropertiesForOperator(plan, "AttributeBasedWindow0");
        Set keys = map.keySet();
        assertEquals(keys.size(),4);
        Iterator iter = keys.iterator();
        
        String fKey = (String)iter.next();
        assertEquals(fKey,"attribute");
        assertEquals((String)map.get(fKey),"value");
        
        fKey = (String)iter.next();
        assertEquals(fKey,"size");
        assertEquals(map.get(fKey),new Double(2.5));
            
        }

    /**
     * Test of listProcessAttributes method, of class IEPAdminMBeanImpl.
     */
    public void testListProcessAttributes() {
        System.out.println("listProcessAttributes");
        String serviceUnitName = "";
        IEPAdminMBeanImpl instance = null;
        String[] expResult = null;
        String[] result = instance.listProcessAttributes(serviceUnitName);
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of listOperators method, of class IEPAdminMBeanImpl.
     */
    public void testListOperators() {
        System.out.println("listOperators");
        String serviceunitName = "";
        String processDeployName = "";
        IEPAdminMBeanImpl instance = null;
        String[] expResult = null;
        String[] result = instance.listOperators( processDeployName);
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getManagedAttributesForOperator method, of class IEPAdminMBeanImpl.
     */
    public void testGetManagedAttributesForOperator() {
        System.out.println("getManagedAttributesForOperator");
        String processDeployName = "";
        String operatorName = "";
        IEPAdminMBeanImpl instance = null;
        Map<String, Object> expResult = null;
        Map<String, Object> result = instance.getManagedAttributesForOperator(processDeployName, operatorName);
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }
        
    

    
}